"""The per-gameweek update job. Run as `python -m src.update`.

Detects whether a full Premier League gameweek has completed since the last
publish. If not, does nothing and writes nothing. If so: pulls Understat live
(no_cache=True), rebuilds features, refits the model on history PLUS every
2026-27 match played so far (an intentional extension beyond the notebook's
fixed-through-2025-26 recipe, see decisions_log.md), resimulates, and writes
the artifacts the app and the next run both read.

Everything under data/app/ is small and committed to the repo (not gitignored,
unlike data/raw/ and data/processed/), since the hosted app and the CI runner
both need it and neither has access to the local soccerdata cache.
"""
import json
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from . import config, data, engine, features, model


def completed_gameweeks(season_df: pd.DataFrame) -> int:
    """The fewest matches any of the 20 clubs has played. This is what
    "gameweek N is complete" means here: a club with a postponed match
    holds this number back for everyone until it is replayed."""
    played = season_df[season_df["scored"].notna()]
    counts = played.groupby("team").size()
    if len(counts) < 20:
        return 0  # not every club has a row yet (shouldn't happen once the season starts)
    return int(counts.min())


def load_state(out_dir: Path) -> dict:
    path = Path(out_dir) / "state.json"
    if path.exists():
        return json.loads(path.read_text())
    return {"last_published_gameweek": 0}


def run_update(out_dir: Path = config.DATA_APP_DIR, force: bool = False) -> dict:
    """Returns a dict describing what happened. Writes nothing if no new
    gameweek has completed, unless force=True (for testing)."""
    out_dir = Path(out_dir)
    state = load_state(out_dir)

    current = data.fetch_current_season()
    season_current = current[current["season"] == config.CURRENT_SEASON]
    completed_gw = completed_gameweeks(season_current)

    if not force and completed_gw <= state.get("last_published_gameweek", 0):
        return {
            "changed": False,
            "completed_gameweek": completed_gw,
            "last_published_gameweek": state.get("last_published_gameweek", 0),
        }

    history = data.load_history()
    combined = data.build_combined(history, current)
    feat = features.build_features(combined)

    # Decision 4: train on history AND every 2026-27 match played so far, refit
    # from scratch each update. Distinct from NB07's shipped recipe, which
    # fixes the production model through 2025-26 only (see decisions_log.md).
    training_seasons = config.ARTETA_SEASONS + [config.CURRENT_SEASON]
    train_rows = model.prepare_rows(feat[feat["season"].isin(training_seasons)], config.FEATURES_FINAL)
    fitted = model.fit(train_rows)

    snap = engine.live_snapshot(feat, config.CURRENT_SEASON)
    final_pts = engine.run_simulation(snap["fixtures"], snap["current_pts"], fitted,
                                       n_runs=config.N_RUNS, seed=config.LIVE_SEED)
    summary = engine.summarize(final_pts, snap["teams"])

    now = datetime.now(timezone.utc).isoformat()
    out_dir.mkdir(parents=True, exist_ok=True)

    current.to_csv(out_dir / "current_season.csv", index=False)
    snap["fixtures"].to_csv(out_dir / "fixtures.csv", index=False)
    pd.DataFrame({"team": snap["teams"], "current_pts": snap["current_pts"]}).to_csv(
        out_dir / "current_points.csv", index=False
    )
    summary.reset_index(names="team").to_csv(out_dir / "summary.csv", index=False)
    model.save(fitted, out_dir / "model.json", meta={
        "trained_through_gameweek": completed_gw,
        "training_rows": len(train_rows),
        "updated_at": now,
    })

    _append_odds_history(out_dir, summary, completed_gw, now)

    new_state = {
        "last_published_gameweek": completed_gw,
        "updated_at": now,
        "training_rows": len(train_rows),
    }
    (out_dir / "state.json").write_text(json.dumps(new_state, indent=2))

    return {
        "changed": True,
        "completed_gameweek": completed_gw,
        "previous_gameweek": state.get("last_published_gameweek", 0),
        "training_rows": len(train_rows),
        "summary": summary,
    }


def _append_odds_history(out_dir: Path, summary: pd.DataFrame, gameweek: int, timestamp: str) -> None:
    """One row per team per gameweek, for the 'how odds moved' chart. Safe to
    rerun: rows for this exact gameweek are replaced, not duplicated."""
    path = Path(out_dir) / "odds_history.csv"
    new_rows = summary.reset_index().rename(columns={"index": "team"})
    new_rows.insert(0, "gameweek", gameweek)
    new_rows.insert(1, "updated_at", timestamp)

    if path.exists():
        existing = pd.read_csv(path)
        existing = existing[existing["gameweek"] != gameweek]
        combined = pd.concat([existing, new_rows], ignore_index=True)
    else:
        combined = new_rows
    combined.sort_values(["gameweek", "team"]).to_csv(path, index=False)


if __name__ == "__main__":
    result = run_update()
    if result["changed"]:
        print(f"Published gameweek {result['completed_gameweek']} "
              f"(was {result['previous_gameweek']}), trained on {result['training_rows']} rows.")
        print(result["summary"].head(6).round(4).to_string())
    else:
        print(f"No new gameweek (currently {result['completed_gameweek']}, "
              f"already published through {result['last_published_gameweek']}). Nothing written.")
