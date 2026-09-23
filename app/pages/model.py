"""The model behind the live predictor: what it is, how good it is, and what failed."""
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "What a model can learn instead",
    "The statistics could not settle the bottle question, so the project changes approach: "
    "instead of testing a hypothesis on ten matches a season, train a model on every match "
    "in the league and see what it learns. This page is that model, held to an honest standard.",
    eyebrow="The evidence",
)

comparison = loaders.table("model_comparison.csv")
coefs = loaders.table("coefficients.csv")
shap_vals = loaders.table("shap.csv")
calib = loaders.table("calibration.csv")
confusion = loaders.table("confusion.csv")
ablation = loaders.table("ablation.csv")
walk = loaders.table("walk_forward.csv")
experiments = loaders.table("experiments.csv")

lr = comparison[comparison["model"] == "Logistic regression"].iloc[0]
dummy = comparison[comparison["model"] == "Always predict the base rate"].iloc[0]

ui.metric_grid([
    ("W / D / L", "Predicts", "Win, draw or loss for the home team"),
    ("11", "Features", "Engineered inputs the model reads for each match"),
    ("4,894", "Training rows", "Matches the production model is fitted on"),
    (f"{lr['log_loss']:.4f}", "Log loss", "Lower is better. This is the metric that matters"),
    (f"{lr['accuracy']*100:.1f}%", "Accuracy", "On a three-way outcome, not two"),
    (f"{dummy['log_loss']:.4f}", "Baseline", "A model using no information at all"),
])

ui.callout(
    "definition", "Why accuracy is the wrong headline.",
    "A model that says 'home win, 40% confident' for every match can look accurate while being "
    "useless. What a simulation needs is well-calibrated probabilities, not confident guesses. "
    "**Log loss** measures that: it punishes a model for being confidently wrong far more than "
    "for being uncertain. Lower is better, and it is the number every decision on this page "
    "was made on.",
)

# ---------------------------------------------------------------------------
st.subheader("Four models, one identical test", anchor=False)

comp = comparison.sort_values("log_loss", ascending=False)
fig = go.Figure(go.Bar(
    y=comp["model"], x=comp["log_loss"], orientation="h",
    marker_color=[theme.COLOR["brand"] if m == "Logistic regression"
                  else theme.COLOR["faint"] for m in comp["model"]],
    text=[f"{v:.4f}" for v in comp["log_loss"]], textposition="outside",
    cliponaxis=False, textfont=dict(color=theme.COLOR["text_primary"], size=12),
    hovertemplate="<b>%{y}</b><br>Log loss %{x:.4f}<extra></extra>",
))
theme.apply(fig, height=320, legend=False,
            xaxis=dict(title=dict(text="Log loss (lower is better)"),
                       range=[0.95, float(comp["log_loss"].max()) * 1.10]))
ui.chart(
    fig,
    title="The simplest model beat both tree models",
    verdict_text=(
        "<strong>The simplest model won.</strong> Logistic regression beat both a random "
        "forest and XGBoost, which are far more flexible and are usually expected to win this "
        "kind of comparison. With roughly 4,900 training rows and eleven features, there is "
        "not enough data for that flexibility to pay for itself, and the tree models spend it "
        "on patterns that do not generalise."
    ),
    method_text=(
        "All four models are trained on seasons 2019-20 through 2024-25 and tested on 2025-26, "
        "a strictly forward-in-time split. Random k-fold cross-validation is never used "
        "anywhere in this project, because shuffling matches would let a model train on the "
        "future and test on the past. The baseline predicts the historical rate of wins, draws "
        "and losses for every match, using no information at all."
    ),
)

with st.expander("Full comparison, including accuracy and Brier score",
                 icon=":material/table_chart:"):
    comp_show = comparison.copy()
    comp_show["accuracy"] = comp_show["accuracy"] * 100
    ui.table(comp_show, {
        "model": st.column_config.TextColumn("Model", pinned=True, width="medium"),
        "log_loss": st.column_config.NumberColumn("Log loss", format="%.4f"),
        "accuracy": st.column_config.NumberColumn("Accuracy", format="%.1f%%"),
        "brier": st.column_config.NumberColumn("Brier score", format="%.4f"),
        "n_test": st.column_config.NumberColumn("Test matches"),
        "note": st.column_config.TextColumn("What it is", width="large"),
    })

# ---------------------------------------------------------------------------
st.subheader("Does it hold up across every season, not just one?", anchor=False)

labels = [theme.season_label(s) for s in walk["test_season"]]
fig = go.Figure()
fig.add_trace(go.Bar(name="The model", x=labels, y=walk["log_loss"],
                     marker_color=theme.COLOR["brand"],
                     hovertemplate="<b>%{x} · the model</b><br>Log loss %{y:.4f}<extra></extra>"))
fig.add_trace(go.Bar(name="Baseline (no features)", x=labels, y=walk["dummy_log_loss"],
                     marker_color=theme.COLOR["faint"],
                     hovertemplate="<b>%{x} · baseline</b><br>Log loss %{y:.4f}<extra></extra>"))
theme.apply(fig, height=370, barmode="group",
            yaxis=dict(title=dict(text="Log loss"), range=[0.9, 1.16]))
n_beat = int(walk["beats_dummy"].sum())
ui.chart(
    fig,
    title=f"It beats the baseline at {n_beat} of {len(walk)} season boundaries",
    verdict_text=(
        "Train on everything before a season, test on that season, repeat. The model beats the "
        f"no-information baseline in <strong>{n_beat} of {len(walk)}</strong> season boundaries "
        "tested, so the headline result is not one lucky split. It is a modest edge, "
        "consistently present."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("What the model actually pays attention to", anchor=False)

# Both panels share one row order, taken from SHAP importance, so a feature
# sits at the same height in each and the two can actually be compared. Their
# x-axes are different units, so those are deliberately not shared.
order = shap_vals.sort_values("mean_abs_shap")["feature"].tolist()

c1, c2 = st.columns(2, gap="medium")
with c1:
    win = coefs[coefs["outcome"] == "Win"].set_index("feature").reindex(order).reset_index()
    fig = go.Figure(go.Bar(
        y=[theme.feature_label(f) for f in win["feature"]], x=win["coefficient"],
        orientation="h",
        marker_color=[theme.judgement_color(v) for v in win["coefficient"]],
        customdata=np.stack([win["p_value"], win["feature"], win["description"]], axis=-1),
        hovertemplate=("<b>%{y}</b><br>Weight %{x:+.2f}  ·  p = %{customdata[0]:.3f}"
                       "<br><i>%{customdata[1]}</i><br>%{customdata[2]}<extra></extra>"),
    ))
    fig.add_vline(x=0, line=dict(color=theme.COLOR["muted"], width=1))
    theme.apply(fig, height=420, legend=False,
                xaxis=dict(title=dict(text="Weight on a home win")))
    ui.bare_chart(fig, title="What the linear model leans on")
with c2:
    sv = shap_vals.set_index("feature").reindex(order).reset_index()
    fig = go.Figure(go.Bar(
        y=[theme.feature_label(f) for f in sv["feature"]], x=sv["mean_abs_shap"],
        orientation="h", marker_color=theme.CLUB["Manchester City"],
        customdata=np.stack([sv["feature"], sv["description"]], axis=-1),
        hovertemplate=("<b>%{y}</b><br>Importance %{x:.3f}"
                       "<br><i>%{customdata[0]}</i><br>%{customdata[1]}<extra></extra>"),
    ))
    theme.apply(fig, height=420, legend=False,
                xaxis=dict(title=dict(text="Mean absolute SHAP value")))
    ui.bare_chart(fig, title="What XGBoost leans on")

win_all = coefs[coefs["outcome"] == "Win"]
stakes_lin = win_all[win_all["feature"] == "stakes_intensity"].iloc[0]
rank = int(shap_vals.reset_index(drop=True).query("feature == 'stakes_intensity'").index[0]) + 1
biggest = win_all.loc[win_all["coefficient"].abs().idxmax()]
ui.verdict(
    "Both charts describe the same eleven features, ordered the same way, so a feature sits "
    "at the same height in each. The pressure metric is the one this project cares about most, "
    f"and it lands modestly. In the linear model it is <strong>statistically reliable but "
    f"small</strong> (p = {stakes_lin['p_value']:.3f}, weight {stakes_lin['coefficient']:+.2f} "
    f"against {theme.feature_label(biggest['feature']).lower()} at "
    f"{biggest['coefficient']:+.2f}). To XGBoost it ranks <strong>{rank} of 11</strong>, "
    "well below opponent quality. <strong>How much a match matters does affect the result, "
    "just far less than who you are playing and whether you are at home.</strong>"
)
ui.callout(
    "scope", "This is not the same verdict the earlier notebook reached.",
    "An earlier model, fitted on four clubs with nine features, found the pressure metric "
    "statistically insignificant while tree-based importance rated it highly, and that "
    "tension was reported as unresolved. The production model shown here covers all twenty "
    "clubs with eleven features and gives a cleaner answer in both directions. The two are "
    "different models on different data, so both readings stand for the model that produced "
    "them.",
)

# ---------------------------------------------------------------------------
st.subheader("What each feature is worth", anchor=False)

abl = ablation[ablation["removed_feature"] != "(nothing removed)"].sort_values("log_loss_cost")
fig = go.Figure(go.Bar(
    y=[theme.feature_label(f) for f in abl["removed_feature"]], x=abl["log_loss_cost"],
    orientation="h",
    marker_color=[theme.judgement_color(v) for v in abl["log_loss_cost"]],
    customdata=np.stack([abl["removed_feature"], abl["description"]], axis=-1),
    hovertemplate=("<b>Removing %{y}</b><br>Changes log loss by %{x:+.4f}"
                   "<br><i>%{customdata[0]}</i><br>%{customdata[1]}<extra></extra>"),
))
fig.add_vline(x=0, line=dict(color=theme.COLOR["muted"], width=1))
theme.apply(fig, height=420, legend=False,
            xaxis=dict(title=dict(text="Increase in log loss when this feature is dropped")))
best = abl.iloc[-1]
ui.chart(
    fig,
    title=f"{theme.feature_label(best['removed_feature'])} is the costliest feature to lose",
    verdict_text=(
        "Drop one feature, refit, retest. Bars to the right are features the model misses. "
        f"<strong>{theme.feature_label(best['removed_feature'])}</strong> costs the most to "
        "lose. Features at or below zero are earning nothing, and a couple actively improve "
        "the model by their absence, which is a normal sign that eleven features is close to "
        "as many as this much data can support."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Can you trust the probabilities?", anchor=False)

c1, c2 = st.columns([3, 2], gap="medium")
with c1:
    fig = go.Figure()
    fig.add_trace(go.Scatter(
        x=[0, 1], y=[0, 1], mode="lines", name="Perfect calibration",
        line=dict(color=theme.COLOR["faint"], width=1, dash="dash"), hoverinfo="skip",
    ))
    fig.add_trace(go.Scatter(
        x=calib["predicted_probability"], y=calib["actual_frequency"],
        mode="markers+lines", name="This model",
        line=dict(color=theme.COLOR["brand"], width=2.5),
        marker=dict(size=calib["n_matches"] / calib["n_matches"].max() * 16 + 7,
                    color=theme.COLOR["brand"]),
        customdata=calib["n_matches"],
        hovertemplate=("<b>Predicted %{x:.0%}</b><br>Actually won %{y:.0%}"
                       "<br>%{customdata} matches in this band<extra></extra>"),
    ))
    theme.apply(fig, height=380,
                xaxis=dict(title=dict(text="Predicted win probability"), tickformat=".0%"),
                yaxis=dict(title=dict(text="Actually won"), tickformat=".0%"))
    ui.bare_chart(fig, title="When it says 70%, does it win 70% of the time?")
with c2:
    cm = confusion.pivot(index="actual", columns="predicted", values="count")
    cm = cm.reindex(index=["Win", "Draw", "Loss"], columns=["Win", "Draw", "Loss"])
    fig = go.Figure(go.Heatmap(
        z=cm.values, x=cm.columns, y=cm.index,
        colorscale=theme.SCALE_SEQUENTIAL, showscale=False,
        text=cm.values, texttemplate="%{text}",
        textfont=dict(size=16, color=theme.COLOR["text_primary"]),
        hovertemplate="<b>Actually %{y}, predicted %{x}</b><br>%{z} matches<extra></extra>",
    ))
    theme.apply(fig, height=380, legend=False,
                xaxis=dict(title=dict(text="Predicted")),
                yaxis=dict(title=dict(text="Actual")))
    ui.bare_chart(fig, title="Predicted against actual")

draws_called = int(confusion[(confusion["actual"] == "Draw")
                             & (confusion["predicted"] == "Draw")]["count"].iloc[0])
ui.verdict(
    "The calibration line tracking the diagonal is what matters for the simulation: when this "
    "model says 60%, roughly 60% of those matches really are won. The grid on the right shows "
    f"its blind spot. It correctly called <strong>{draws_called} draws</strong>. Draws are "
    "genuinely the hardest outcome to predict from form alone, and the next section is what "
    "happened when we tried to fix that."
)

# ---------------------------------------------------------------------------
st.subheader("Fourteen things we tried, and the one that worked", anchor=False)

exp = experiments.copy()
exp["Verdict"] = np.where(exp["kept"], "Kept", "Rejected")
# Several experiments have no recorded figure; coerce so blanks stay blank
# rather than rendering the string "None", and put accuracy on a 0-100 scale.
exp["log_loss"] = pd.to_numeric(exp["log_loss"], errors="coerce")
exp["accuracy"] = pd.to_numeric(exp["accuracy"], errors="coerce") * 100
ui.table(
    exp[["experiment", "notebook", "Verdict", "log_loss", "accuracy", "verdict"]],
    {
        "experiment": st.column_config.TextColumn("Idea", pinned=True, width="medium"),
        "notebook": st.column_config.TextColumn("Stage", width="small"),
        "Verdict": st.column_config.TextColumn("Outcome", width="small"),
        "log_loss": st.column_config.NumberColumn("Log loss", format="%.4f"),
        "accuracy": st.column_config.NumberColumn("Accuracy", format="%.1f%%"),
        "verdict": st.column_config.TextColumn("What happened", width="large"),
    },
)
ui.verdict(
    "Every one of these was implemented and measured on the identical test set rather than "
    "argued about. <strong>Only head-to-head record earned a place.</strong> The most "
    "instructive failures are the ones aimed at the draw problem: forcing the model to predict "
    "more draws raised draw recall every time and made both log loss and accuracy worse every "
    "time. Rebalancing does not create information, it just moves confidence away from the "
    "calls the model can actually make."
)
ui.callout(
    "scope", "These fourteen numbers are transcribed, not recomputed.",
    "They come from the notebooks' own recorded runs. Several take hours, and one involved "
    "roughly 1,100 model fits. Everything else on this page is recomputed from the raw data "
    "every time the exports are rebuilt.",
)

st.subheader("The honest summary", anchor=False)
ui.cards([
    ("It beats the baseline, consistently",
     f"Log loss {lr['log_loss']:.4f} against {dummy['log_loss']:.4f} for a model with no "
     "information, and it wins at every season boundary tested. That edge is real."),
    ("It is nowhere near solving football",
     f"{lr['accuracy']*100:.0f}% accuracy on a three-way outcome. Predicting individual "
     "matches is genuinely hard, and anyone claiming much better from public data is usually "
     "measuring something easier."),
    ("The ceiling is the data, not the method",
     "Fourteen documented attempts to improve it failed. Getting meaningfully further needs "
     "player-level data, injuries or betting odds, not a cleverer algorithm."),
])

ui.prev_next("pages/model.py")
