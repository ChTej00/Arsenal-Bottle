"""Fetching and reshaping Understat schedule data.

Mirrors notebooks/07_monte_carlo_simulation.ipynb Sections C, D and G
(fetch_live_schedule, wide_to_long). Historical seasons are read from the
committed data/app/history.csv, built once by build_history() below, so the
update job only ever hits the network for the current season.
"""
import pandas as pd
import soccerdata as sd

from . import config

RESHAPE_COLS = [
    "league", "season", "game_id", "date", "team", "opponent",
    "venue", "xG", "xGA", "scored", "conceded",
]


def wide_to_long(sched: pd.DataFrame) -> pd.DataFrame:
    """One row per match becomes two rows, one per team. Matches NB07's wide_to_long."""
    home = sched.rename(columns={
        "home_team": "team", "away_team": "opponent",
        "home_goals": "scored", "away_goals": "conceded",
        "home_xg": "xG", "away_xg": "xGA",
    })
    home["venue"] = "home"
    away = sched.rename(columns={
        "away_team": "team", "home_team": "opponent",
        "away_goals": "scored", "home_goals": "conceded",
        "away_xg": "xG", "home_xg": "xGA",
    })
    away["venue"] = "away"
    long = pd.concat([home[RESHAPE_COLS], away[RESHAPE_COLS]], ignore_index=True)
    for c in ["scored", "conceded", "xG", "xGA"]:
        long[c] = long[c].astype("float64")
    return long.sort_values(["team", "season", "date"]).reset_index(drop=True)


def fetch_schedule(seasons, no_cache: bool = False) -> pd.DataFrame:
    """Raw Understat schedule for the given season(s), wide format, one row per match."""
    u = sd.Understat(leagues=config.LEAGUE, seasons=seasons, no_cache=no_cache)
    sched = u.read_schedule().reset_index()
    sched["date"] = pd.to_datetime(sched["date"])
    return sched


def build_history() -> pd.DataFrame:
    """Historical long-format schedule, 2019-20 through 2025-26, all 20 teams.

    Reads from the local soccerdata cache, no network call for these completed
    seasons. Run this once (or whenever ARTETA_SEASONS changes, i.e. at season
    rollover) to refresh data/app/history.csv. The update job reads that
    committed file instead of calling this.
    """
    sched = fetch_schedule(config.ARTETA_SEASONS, no_cache=False)
    return wide_to_long(sched)


def load_history() -> pd.DataFrame:
    """Committed historical table, data/app/history.csv."""
    path = config.DATA_APP_DIR / "history.csv"
    df = pd.read_csv(path)
    df["date"] = pd.to_datetime(df["date"])
    df["season"] = df["season"].astype(str)
    return df


def fetch_current_season() -> pd.DataFrame:
    """Live pull of the current season. no_cache=True so a stale cached copy
    from before this gameweek is never served (soccerdata otherwise caches
    the first response it ever saw for a season)."""
    sched = fetch_schedule(config.CURRENT_SEASON, no_cache=True)
    return wide_to_long(sched)


def build_combined(history_df: pd.DataFrame, current_df: pd.DataFrame) -> pd.DataFrame:
    """History plus the current season's long-format rows (played and not),
    ready for features.build_features(). Matches NB07 Section G's `combined`."""
    combined = pd.concat([history_df[RESHAPE_COLS], current_df[RESHAPE_COLS]], ignore_index=True)
    for c in ["scored", "conceded", "xG", "xGA"]:
        combined[c] = combined[c].astype("float64")
    return combined.sort_values(["team", "season", "date"]).reset_index(drop=True)


if __name__ == "__main__":
    # One-off / rollover use: rebuild data/app/history.csv from the local cache.
    history = build_history()
    config.DATA_APP_DIR.mkdir(parents=True, exist_ok=True)
    out_path = config.DATA_APP_DIR / "history.csv"
    history.to_csv(out_path, index=False)
    print(f"Wrote {out_path} : {history.shape}")
