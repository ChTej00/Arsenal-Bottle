"""Derived tables for the app's Tab 1 (Arsenal story) and Tab 2 (comparator).

data/processed/ is gitignored (reproducible from the raw scrapes, not meant
for public viewing), so the hosted app can't read it directly. This module
recomputes the NB03/NB04 pressure metrics from it and writes small committable
tables to data/app/. Run once with `python -m src.comparator`, and again
whenever data/processed/all_4teams_processed.csv changes.

Scope note: this only covers the four teams with full match-level histories
(Arsenal, Liverpool, Manchester City, Manchester United), the same four NB03
and NB04 analysed. The comparator can't cover all 20 clubs, since the other
sixteen never had Drop Index or is_high_stakes_retro computed for them.
"""
from pathlib import Path

import pandas as pd
from scipy import stats

from . import config

PROCESSED_PATH = config.DATA_PROCESSED_DIR / "all_4teams_processed.csv"
CONTENTION_MIN_MATCHES = 5  # NB02/NB04's filter: >=5 matches with is_high_stakes True that season


def build_team_pressure_table() -> pd.DataFrame:
    """One row per team-season: the Bottle Gap (high-stakes-retro PPG minus
    that season's own baseline PPG, NB03 Chart C5/NB04 Chart B1) plus the
    xG/xGA stakes split and its Welch's t-test p-value (NB03 Chart C4/NB04
    Chart C1)."""
    df = pd.read_csv(PROCESSED_PATH)

    g = df.groupby(["team", "season"])
    baseline_ppg = g["points"].mean()
    hs_ppg = df[df["is_high_stakes_retro"]].groupby(["team", "season"])["points"].mean()
    contention_count = g["is_high_stakes"].sum()

    rows = []
    for (team, season), grp in g:
        hs = grp[grp["is_high_stakes_retro"]]
        nm = grp[~grp["is_high_stakes_retro"]]
        xg_p = stats.ttest_ind(hs["xG"], nm["xG"], equal_var=False).pvalue if len(hs) > 1 else float("nan")
        xga_p = stats.ttest_ind(hs["xGA"], nm["xGA"], equal_var=False).pvalue if len(hs) > 1 else float("nan")
        rows.append({
            "team": team, "season": season,
            "baseline_ppg": baseline_ppg[(team, season)],
            "high_stakes_ppg": hs_ppg.get((team, season), float("nan")),
            "ppg_gap": hs_ppg.get((team, season), float("nan")) - baseline_ppg[(team, season)],
            "is_contention_season": bool(contention_count[(team, season)] >= CONTENTION_MIN_MATCHES),
            "xG_high_stakes": hs["xG"].mean(), "xG_normal": nm["xG"].mean(), "xG_pvalue": xg_p,
            "xGA_high_stakes": hs["xGA"].mean(), "xGA_normal": nm["xGA"].mean(), "xGA_pvalue": xga_p,
            "win_rate": grp["is_win"].mean(),
            "win_rate_high_stakes": hs["is_win"].mean(),
        })
    return pd.DataFrame(rows).sort_values(["team", "season"])


def build_team_season_progress() -> pd.DataFrame:
    """Per team-season rolled-up trend numbers (PPG, xG, xGA, win rate), the
    raw material for Tab 1's 'the Rise' progression and Tab 2's overview."""
    df = pd.read_csv(PROCESSED_PATH)
    out = df.groupby(["team", "season"]).agg(
        ppg=("points", "mean"),
        xG=("xG", "mean"),
        xGA=("xGA", "mean"),
        win_rate=("is_win", "mean"),
        matches=("game_id", "count"),
    ).reset_index()
    return out.sort_values(["team", "season"])


def build_pressure_resilience_index(pressure_table: pd.DataFrame) -> pd.DataFrame:
    """One row per team: mean PPG gap across contention seasons only (NB04
    Chart B2)."""
    contention = pressure_table[pressure_table["is_contention_season"]]
    return contention.groupby("team")["ppg_gap"].mean().rename("pressure_resilience_index").reset_index()


if __name__ == "__main__":
    pressure = build_team_pressure_table()
    progress = build_team_season_progress()
    pri = build_pressure_resilience_index(pressure)

    config.DATA_APP_DIR.mkdir(parents=True, exist_ok=True)
    pressure.to_csv(config.DATA_APP_DIR / "team_pressure.csv", index=False)
    progress.to_csv(config.DATA_APP_DIR / "team_season_progress.csv", index=False)
    pri.to_csv(config.DATA_APP_DIR / "pressure_resilience_index.csv", index=False)

    print(f"Wrote team_pressure.csv: {pressure.shape}")
    print(f"Wrote team_season_progress.csv: {progress.shape}")
    print(f"Wrote pressure_resilience_index.csv: {pri.shape}")
    print(pri.round(3).to_string(index=False))
