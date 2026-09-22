"""Model and simulation artifacts for the app's modelling pages.

Everything here is recomputed from data/app/history.csv through the same
src/features.py, src/model.py and src/engine.py the live pipeline uses, so the
app's "how good is this model" page is showing the real model, not a
transcription. main() verifies against the acceptance numbers in
PROJECT_MASTER_CONTEXT.md Phase 8.8 before writing anything.

Uses xgboost, shap and statsmodels, which are not in requirements.txt. That is
deliberate: this runs locally on demand, like src/comparator.py and
src/exports.py, and the hosted app only ever reads the CSVs it produces.

Run with `python -m src.model_exports`.
"""
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import brier_score_loss, confusion_matrix, log_loss, accuracy_score
from sklearn.preprocessing import StandardScaler

from . import config, data, engine, features, model

HOLDOUT_SEASON = "2526"
TRAIN_SEASONS = ["1920", "2021", "2122", "2223", "2324", "2425"]
BACKTEST_SEASON = "2526"
CLASS_NAMES = {0: "Loss", 1: "Draw", 2: "Win"}

FEATURE_GLOSSARY = {
    "xG_roll5": "The team's own expected goals, averaged over its previous 5 matches.",
    "xGA_roll5": "Expected goals conceded, averaged over its previous 5 matches.",
    "pts_roll5": "Points per match over its previous 5 matches.",
    "win_rate_roll5": "Share of its previous 5 matches won.",
    "is_home": "1 if the team is playing at home, 0 away.",
    "is_big6_opp": "1 if the opponent is one of the traditional Big 6 clubs.",
    "opp_xgd_roll5": "The opponent's own rolling expected goal difference, a continuous measure of opponent quality.",
    "parity_gap": "How evenly matched the two sides are, the absolute difference between their rolling goal differences.",
    "stakes_intensity": "How much is riding on this match, 0 to 1, from table position, time of season, form and rivalry.",
    "is_non_big6_rivalry": "1 for a derby that is not already captured by the Big 6 flag, currently only Liverpool vs Everton.",
    "h2h_pts_avg3": "Average points this team took from its last 3 meetings with this opponent.",
}


def build_feature_frame() -> pd.DataFrame:
    """The full featured historical table, 2019-20 through 2025-26, all 20 clubs."""
    return features.build_features(data.load_history())


def _split(feat: pd.DataFrame):
    rows = model.prepare_rows(feat)
    train = rows[rows["season"].isin(TRAIN_SEASONS)]
    test = rows[rows["season"] == HOLDOUT_SEASON]
    return train, test


# ---------------------------------------------------------------------------
# Model comparison
# ---------------------------------------------------------------------------

def build_model_comparison(feat: pd.DataFrame) -> pd.DataFrame:
    """Four models on one identical holdout, the comparison NB06 and NB07 both run."""
    import xgboost as xgb

    train, test = _split(feat)
    F = config.FEATURES_FINAL
    y_tr, y_te = train["y"].to_numpy(), test["y"].to_numpy()
    w = train["recency_weight"].to_numpy()

    scaler = StandardScaler().fit(train[F])
    Xtr, Xte = scaler.transform(train[F]), scaler.transform(test[F])

    rows = []

    def record(name, proba, preds, note):
        onehot = np.zeros_like(proba)
        onehot[np.arange(len(y_te)), y_te] = 1
        rows.append({
            "model": name,
            "log_loss": log_loss(y_te, proba, labels=[0, 1, 2]),
            "accuracy": accuracy_score(y_te, preds),
            "brier": float(np.mean(np.sum((proba - onehot) ** 2, axis=1))),
            "n_test": len(test),
            "note": note,
        })

    dummy = model.dummy_baseline(train, test)
    prior = train["y"].value_counts(normalize=True).reindex([0, 1, 2]).to_numpy()
    record("Always predict the base rate", np.tile(prior, (len(test), 1)),
           np.full(len(test), int(np.argmax(prior))),
           "No features at all. The bar any real model has to clear.")
    rows[-1]["log_loss"], rows[-1]["accuracy"] = dummy["log_loss"], dummy["accuracy"]

    fitted = model.fit(train)
    record("Logistic regression", fitted.model.predict_proba(Xte), fitted.model.predict(Xte),
           "The production model. Simple, linear, best calibrated.")

    rf = RandomForestClassifier(n_estimators=300, random_state=42, n_jobs=-1)
    rf.fit(Xtr, y_tr, sample_weight=w)
    record("Random forest", rf.predict_proba(Xte), rf.predict(Xte),
           "Flexible, non-linear. Did not beat the linear model here.")

    gb = xgb.XGBClassifier(n_estimators=300, max_depth=4, learning_rate=0.05,
                           objective="multi:softprob", num_class=3,
                           random_state=42, n_jobs=-1, verbosity=0)
    gb.fit(Xtr, y_tr, sample_weight=w)
    record("XGBoost", gb.predict_proba(Xte), gb.predict(Xte),
           "Gradient boosting. Also did not beat the linear model.")

    return pd.DataFrame(rows).sort_values("log_loss").reset_index(drop=True)


def build_coefficients(feat: pd.DataFrame) -> pd.DataFrame:
    """Production model coefficients with p-values from statsmodels MNLogit.
    Coefficients are on standardised features, so they are directly comparable
    to each other in size."""
    import statsmodels.api as sm

    rows_df = model.prepare_rows(feat)
    prod = rows_df[rows_df["season"].isin(config.ARTETA_SEASONS)]
    F = config.FEATURES_FINAL

    fitted = model.fit(prod)
    Xs = fitted.scaler.transform(prod[F])
    mnl = sm.MNLogit(prod["y"].to_numpy(), sm.add_constant(Xs)).fit(disp=False)

    # mnl.pvalues is (n_params, n_classes - 1): rows are const + the 11
    # features, columns are the two non-reference outcomes (class 0 is the
    # reference, so it has no column of its own).
    pvals = np.asarray(mnl.pvalues)

    out = []
    for ci, cls in enumerate(fitted.model.classes_):
        for fi, name in enumerate(F):
            p = np.nan if cls == 0 else float(pvals[fi + 1, ci - 1])
            out.append({
                "outcome": CLASS_NAMES[cls],
                "feature": name,
                "coefficient": float(fitted.model.coef_[ci, fi]),
                "abs_coefficient": abs(float(fitted.model.coef_[ci, fi])),
                "p_value": p,
                "significant": bool(p < 0.05) if p == p else None,
                "description": FEATURE_GLOSSARY[name],
            })
    return pd.DataFrame(out)


def build_shap(feat: pd.DataFrame) -> pd.DataFrame:
    """Mean absolute SHAP value per feature for the Win class, the measure that
    put stakes_intensity on a par with opponent quality in NB06."""
    import shap
    import xgboost as xgb

    train, test = _split(feat)
    F = config.FEATURES_FINAL
    scaler = StandardScaler().fit(train[F])
    gb = xgb.XGBClassifier(n_estimators=300, max_depth=4, learning_rate=0.05,
                           objective="multi:softprob", num_class=3,
                           random_state=42, n_jobs=-1, verbosity=0)
    gb.fit(scaler.transform(train[F]), train["y"], sample_weight=train["recency_weight"])

    values = shap.TreeExplainer(gb).shap_values(scaler.transform(test[F]))
    values = np.array(values)
    # shap returns (n_samples, n_features, n_classes) for multiclass XGB
    if values.ndim == 3 and values.shape[-1] == 3:
        win = values[:, :, 2]
    else:
        win = values[2]

    out = pd.DataFrame({
        "feature": F,
        "mean_abs_shap": np.abs(win).mean(axis=0),
    })
    out["description"] = out["feature"].map(FEATURE_GLOSSARY)
    return out.sort_values("mean_abs_shap", ascending=False).reset_index(drop=True)


def build_calibration(feat: pd.DataFrame, n_bins: int = 10) -> pd.DataFrame:
    """Does a 70% win prediction actually win 70% of the time? Win class,
    holdout season (NB06 Section I)."""
    train, test = _split(feat)
    fitted = model.fit(train)
    proba = fitted.model.predict_proba(fitted.scaler.transform(test[config.FEATURES_FINAL]))[:, 2]
    actual = (test["y"].to_numpy() == 2).astype(float)

    bins = np.linspace(0, 1, n_bins + 1)
    idx = np.clip(np.digitize(proba, bins) - 1, 0, n_bins - 1)
    rows = []
    for b in range(n_bins):
        m = idx == b
        if m.sum() == 0:
            continue
        rows.append({
            "bin_lower": bins[b], "bin_upper": bins[b + 1],
            "predicted_probability": float(proba[m].mean()),
            "actual_frequency": float(actual[m].mean()),
            "n_matches": int(m.sum()),
        })
    out = pd.DataFrame(rows)
    out["brier_win_class"] = brier_score_loss(actual, proba)
    return out


def build_confusion(feat: pd.DataFrame) -> pd.DataFrame:
    train, test = _split(feat)
    fitted = model.fit(train)
    preds = fitted.model.predict(fitted.scaler.transform(test[config.FEATURES_FINAL]))
    cm = confusion_matrix(test["y"], preds, labels=[0, 1, 2])
    rows = []
    for i, actual in enumerate([0, 1, 2]):
        for j, pred in enumerate([0, 1, 2]):
            rows.append({
                "actual": CLASS_NAMES[actual], "predicted": CLASS_NAMES[pred],
                "count": int(cm[i, j]),
                "share_of_actual": float(cm[i, j] / cm[i].sum()) if cm[i].sum() else 0.0,
            })
    return pd.DataFrame(rows)


def build_ablation(feat: pd.DataFrame) -> pd.DataFrame:
    """Drop one feature at a time and see what it was worth. Head-to-head was
    the only late addition that earned its place (NB07 Section F)."""
    train, test = _split(feat)
    full = model.evaluate(model.fit(train), test)
    rows = [{"removed_feature": "(nothing removed)", "log_loss": full["log_loss"],
             "accuracy": full["accuracy"], "log_loss_cost": 0.0,
             "description": "The full 11-feature model."}]
    for f in config.FEATURES_FINAL:
        reduced = [c for c in config.FEATURES_FINAL if c != f]
        tr = model.prepare_rows(feat, reduced)
        tr_f = tr[tr["season"].isin(TRAIN_SEASONS)]
        te_f = tr[tr["season"] == HOLDOUT_SEASON]
        res = model.evaluate(model.fit(tr_f, reduced), te_f)
        rows.append({
            "removed_feature": f, "log_loss": res["log_loss"], "accuracy": res["accuracy"],
            "log_loss_cost": res["log_loss"] - full["log_loss"],
            "description": FEATURE_GLOSSARY[f],
        })
    return pd.DataFrame(rows).sort_values("log_loss_cost", ascending=False).reset_index(drop=True)


def build_walk_forward(feat: pd.DataFrame) -> pd.DataFrame:
    """Train on everything before season N, test on season N. Repeated at every
    season boundary, which is the only honest way to validate a time series."""
    rows_df = model.prepare_rows(feat)
    seasons = config.ARTETA_SEASONS
    out = []
    for i in range(2, len(seasons)):
        test_season = seasons[i]
        tr = rows_df[rows_df["season"].isin(seasons[:i])]
        te = rows_df[rows_df["season"] == test_season]
        if len(te) == 0:
            continue
        res = model.evaluate(model.fit(tr), te)
        dummy = model.dummy_baseline(tr, te)
        out.append({
            "test_season": test_season,
            "train_seasons": len(seasons[:i]),
            "n_train": len(tr), "n_test": len(te),
            "log_loss": res["log_loss"], "accuracy": res["accuracy"],
            "dummy_log_loss": dummy["log_loss"], "dummy_accuracy": dummy["accuracy"],
            "beats_dummy": bool(res["log_loss"] < dummy["log_loss"]),
        })
    return pd.DataFrame(out)


# ---------------------------------------------------------------------------
# Simulation artifacts
# ---------------------------------------------------------------------------

def build_backtest(feat: pd.DataFrame, cutoff_gw: int) -> pd.DataFrame:
    """Freeze the completed 2025-26 season at a gameweek, simulate forward with
    a model that never saw that season, and compare against what really
    happened (NB07 Section J)."""
    rows_df = model.prepare_rows(feat)
    train = rows_df[rows_df["season"] < BACKTEST_SEASON]
    fitted = model.fit(train)

    snap = engine.backtest_snapshot(feat, BACKTEST_SEASON, cutoff_gw)
    final = engine.run_simulation(snap["fixtures"], snap["current_pts"], fitted,
                                  n_runs=config.N_RUNS, seed=config.BACKTEST_SEED)
    summary = engine.summarize(final, snap["teams"])

    season = feat[feat["season"] == BACKTEST_SEASON]
    real = season.groupby("team")["points"].sum()
    real_rank = real.rank(ascending=False, method="min")

    out = summary.copy()
    out["real_points"] = real.reindex(out.index)
    out["real_rank"] = real_rank.reindex(out.index).astype(int)
    out["inside_range"] = (out["real_points"] >= out["pts_p5"]) & (out["real_points"] <= out["pts_p95"])
    out["cutoff_gameweek"] = cutoff_gw
    return out.reset_index().rename(columns={"index": "team"})


def build_convergence(feat: pd.DataFrame) -> pd.DataFrame:
    """Is 10,000 runs enough? Title probability at three run counts (NB07
    Section I's convergence check), on the current live snapshot."""
    history = data.load_history()
    current = pd.read_csv(config.DATA_APP_DIR / "current_season.csv")
    current["date"] = pd.to_datetime(current["date"])
    current["season"] = current["season"].astype(str)
    combined = data.build_combined(history, current)
    live_feat = features.build_features(combined)

    rows_df = model.prepare_rows(live_feat)
    fitted = model.fit(rows_df)
    snap = engine.live_snapshot(live_feat)

    out = []
    for n in [1000, 10000, 50000]:
        final = engine.run_simulation(snap["fixtures"], snap["current_pts"], fitted,
                                      n_runs=n, seed=config.CONVERGENCE_SEED)
        s = engine.summarize(final, snap["teams"])
        for team in s.head(4).index:
            out.append({"n_runs": n, "team": team, "title_prob": s.loc[team, "title_prob"]})
    return pd.DataFrame(out)


def build_experiments() -> pd.DataFrame:
    """Ideas that were tried and did not survive. Transcribed from
    decisions_log.md's NB06 and NB07 sections, which is the record of work done
    in the notebooks; these are not recomputed here because several take hours
    (roughly 1,100 model fits for the combined prototype alone). Kept because a
    list of honestly-measured dead ends is worth as much as the model that won.
    """
    rows = [
        ("Class weighting (balanced)", "NB06", 1.0589, 0.4429,
         "Forcing the model to call more draws made both metrics worse. It reallocates confidence, it does not create signal."),
        ("Two-stage draw classifier", "NB06", 1.0700, 0.4400,
         "Predict draw-or-not first, then win-or-loss. Same failure as class weighting."),
        ("Poisson / Dixon-Coles goals model", "NB06", None, None,
         "Model goals scored, derive W/D/L. Underperformed the direct classifier."),
        ("Hyperparameter tuning (balanced objective)", "NB06", None, None,
         "Tuned inside the class-balanced objective, so it inherited its problem."),
        ("Combined best-of-everything prototype", "NB06", 1.0176, 0.5429,
         "20-team data, walk-forward retraining, class weights, draw-targeted features, tuned decision rule. Lost on both metrics."),
        ("Match-level walk-forward retraining", "NB06", 0.9641, 0.5571,
         "Retraining after every match did not beat one model fit once before the season."),
        ("Aggressive recency weighting (decay 0.4)", "NB06", 0.9891, 0.5357,
         "Down-weighting old seasons hurt monotonically. It shrinks the usable training set without fixing any real bias."),
        ("Random forest on all 20 teams", "NB07", 1.0465, None,
         "Five times the data did not make flexibility pay off."),
        ("XGBoost on all 20 teams", "NB07", 1.0455, None,
         "Same verdict as random forest."),
        ("Tuned regularisation strength", "NB07", 1.0390, None,
         "Best C=0.03 against C=1.0's 1.0391. Noise, not a gain."),
        ("is_promoted flag", "NB07", 1.0512, None,
         "Promoted clubs are genuinely harder to predict, but a flag does not fix a missing history."),
        ("Separate opponent attack and defence terms", "NB07", 1.0476, None,
         "Splitting opponent quality in two added nothing."),
        ("stakes x Big 6 interaction", "NB07", 1.0477, None,
         "Slightly worse than leaving the two features separate."),
        ("Head-to-head record", "NB07", 1.0362, 0.4886,
         "KEPT. The only late addition that earned its place, and the first feature that says anything about these two specific clubs."),
    ]
    out = pd.DataFrame(rows, columns=["experiment", "notebook", "log_loss", "accuracy", "verdict"])
    out["kept"] = out["experiment"] == "Head-to-head record"
    return out


def build_feature_glossary() -> pd.DataFrame:
    return pd.DataFrame(
        [{"feature": k, "description": v, "in_final_model": k in config.FEATURES_FINAL}
         for k, v in FEATURE_GLOSSARY.items()]
    )


# ---------------------------------------------------------------------------

def main() -> None:
    feat = build_feature_frame()
    print(f"featured history: {feat.shape[0]} rows")

    comparison = build_model_comparison(feat)
    coefficients = build_coefficients(feat)
    shap_vals = build_shap(feat)
    calibration = build_calibration(feat)
    confusion = build_confusion(feat)
    ablation = build_ablation(feat)
    walk_forward = build_walk_forward(feat)
    bt20 = build_backtest(feat, 20)
    bt5 = build_backtest(feat, 5)
    convergence = build_convergence(feat)

    outputs = {
        "model_comparison.csv": comparison,
        "coefficients.csv": coefficients,
        "shap.csv": shap_vals,
        "calibration.csv": calibration,
        "confusion.csv": confusion,
        "ablation.csv": ablation,
        "walk_forward.csv": walk_forward,
        "backtest_gw20.csv": bt20,
        "backtest_gw5.csv": bt5,
        "convergence.csv": convergence,
        "experiments.csv": build_experiments(),
        "feature_glossary.csv": build_feature_glossary(),
    }

    config.DATA_APP_DIR.mkdir(parents=True, exist_ok=True)
    for name, frame in outputs.items():
        frame.to_csv(config.DATA_APP_DIR / name, index=False)
        print(f"wrote {name:<24} {frame.shape[0]:>4} rows x {frame.shape[1]:>2} cols")

    print("\nverification against Phase 8.8 acceptance numbers")
    print("-" * 64)

    def check(label, got, want, tol=0.005):
        print(f"  {'OK  ' if abs(got - want) <= tol else 'MISS'}  {label:<42} {got:.4f} (doc {want:.4f})")

    lr = comparison.set_index("model").loc["Logistic regression"]
    dm = comparison.set_index("model").loc["Always predict the base rate"]
    check("logistic regression log loss", lr["log_loss"], 1.0362)
    check("logistic regression accuracy", lr["accuracy"], 0.4886)
    check("dummy baseline log loss", dm["log_loss"], 1.0984)
    check("dummy baseline accuracy", dm["accuracy"], 0.3600)

    no_h2h = ablation.set_index("removed_feature").loc["h2h_pts_avg3"]
    check("without head-to-head, log loss", no_h2h["log_loss"], 1.0455)
    check("without head-to-head, accuracy", no_h2h["accuracy"], 0.4757)

    prod_rows = len(model.prepare_rows(feat))
    print(f"  {'OK  ' if prod_rows == 4894 else 'MISS'}  {'production training rows':<42} "
          f"{prod_rows} (doc 4894)")

    a20 = bt20.set_index("team").loc["Arsenal"]
    check("GW20 backtest, Arsenal title prob", a20["title_prob"], 0.8566)
    print(f"  {'OK  ' if a20['pts_median'] == 85 else 'MISS'}  "
          f"{'GW20 backtest, Arsenal median points':<42} {a20['pts_median']:.0f} (doc 85)")
    n_in = int(bt20["inside_range"].sum())
    print(f"  {'OK  ' if n_in == 17 else 'MISS'}  {'GW20 backtest, clubs inside range':<42} "
          f"{n_in} (doc 17)")
    misses = sorted(bt20.loc[~bt20["inside_range"], "team"].tolist())
    want_misses = sorted(["Manchester United", "Bournemouth", "Tottenham"])
    print(f"  {'OK  ' if misses == want_misses else 'MISS'}  {'GW20 backtest, which clubs missed':<42} "
          f"{', '.join(misses)}")

    a5 = bt5.set_index("team").loc["Arsenal"]
    check("GW5 backtest, Arsenal title prob", a5["title_prob"], 0.297, 0.01)
    n_in5 = int(bt5["inside_range"].sum())
    print(f"  {'OK  ' if n_in5 == 14 else 'MISS'}  {'GW5 backtest, clubs inside range':<42} "
          f"{n_in5} (doc 14)")


if __name__ == "__main__":
    main()
