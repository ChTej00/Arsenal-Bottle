"""Feature engineering shared by the historical build and the live update.

Mirrors notebooks/07_monte_carlo_simulation.ipynb Sections D, E, F and G
(result/points, rolling form, opponent quality, stakes_intensity with the
relegation-boundary fix, head-to-head). The notebook runs this logic twice,
once for the pure-historical frame and again for historical+live combined;
here it is one function so both callers share the exact same code path.

Works on a frame with unplayed rows too (scored/conceded/xG/xGA all NaN),
as long as every row has team/season/date/opponent/venue/game_id. Unplayed
rows get NaN result/points/xgd and carry through to NaN rolling features
for their own row, same as the notebook's `combined` frame in Section G.
"""
import numpy as np
import pandas as pd

from . import config


def add_result_columns(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["result"] = np.where(
        df["scored"] > df["conceded"], "W",
        np.where(df["scored"] == df["conceded"], "D", "L"),
    )
    df.loc[df["scored"].isna(), "result"] = np.nan
    df["points"] = df["result"].map({"W": 3, "D": 1, "L": 0})
    df["xgd"] = df["xG"] - df["xGA"]
    df["is_win"] = (df["result"] == "W").astype(float)
    df["is_home"] = (df["venue"] == "home").astype(int)
    df["is_big6_opp"] = df["opponent"].isin(config.BIG6).astype(int)
    df["is_rivalry"] = df.apply(lambda r: r["opponent"] in config.RIVALRIES.get(r["team"], []), axis=1)
    df["is_non_big6_rivalry"] = (df["is_rivalry"] & (df["is_big6_opp"] == 0)).astype(int)
    return df


def _roll(g, col, window, min_p=3):
    return g[col].transform(lambda x: x.shift(1).rolling(window, min_periods=min_p).mean())


def add_rolling_features(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    g = df.groupby(["team", "season"])
    df["xG_roll5"] = _roll(g, "xG", 5)
    df["xGA_roll5"] = _roll(g, "xGA", 5)
    df["pts_roll5"] = _roll(g, "points", 5)
    df["win_rate_roll5"] = _roll(g, "is_win", 5)
    df["xgd_roll5"] = _roll(g, "xgd", 5)
    return df


def add_opponent_quality(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    opp_lookup = df[["team", "season", "game_id", "xgd_roll5"]].rename(
        columns={"team": "opponent", "xgd_roll5": "opp_xgd_roll5"}
    )
    df = df.merge(opp_lookup, on=["opponent", "season", "game_id"], how="left")
    df["parity_gap"] = (df["xgd_roll5"] - df["opp_xgd_roll5"]).abs()
    return df


def _nth_place_pts(x: pd.Series, n: int) -> float:
    return x.nlargest(n).iloc[-1] if len(x) >= n else x.min()


def _boundary_gap(cum_pts, top, next_):
    return np.where(cum_pts >= top, top - next_, top - cum_pts)


def add_stakes_intensity(df: pd.DataFrame) -> pd.DataFrame:
    """gameweek, standings-derived gaps, and stakes_intensity (all 20 teams,
    relegation boundary included). Matches NB07 Section E/G exactly, including
    the fix: releg_gap for a safe team is its OWN distance above 18th place,
    not the shared top-vs-next cushion _boundary_gap would give cl_gap/eur_gap.
    """
    df = df.copy()
    df["gameweek"] = df.groupby(["team", "season"])["date"].rank(method="dense").astype(int)
    df["cum_pts"] = df.groupby(["team", "season"])["points"].transform(lambda x: x.fillna(0).cumsum())
    df["leader_pts"] = df.groupby(["season", "gameweek"])["cum_pts"].transform("max")
    df["title_gap"] = df["leader_pts"] - df["cum_pts"]

    for n, name in [(4, "4th"), (5, "5th"), (6, "6th"), (7, "7th"), (17, "17th"), (18, "18th")]:
        df[f"pts_{name}"] = df.groupby(["season", "gameweek"])["cum_pts"].transform(lambda x: _nth_place_pts(x, n))

    df["cl_gap"] = _boundary_gap(df["cum_pts"], df["pts_4th"], df["pts_5th"])
    df["eur_gap"] = _boundary_gap(df["cum_pts"], df["pts_6th"], df["pts_7th"])
    df["releg_gap"] = np.where(
        df["cum_pts"] >= df["pts_17th"],
        df["cum_pts"] - df["pts_18th"],
        df["pts_17th"] - df["cum_pts"],
    )

    gw_factor = 1 / (1 + np.exp(-config.GW_SIGMOID_K * (df["gameweek"] - config.GW_SIGMOID_MID)))
    pts_remaining = (38 - df["gameweek"] + 1) * 3

    def _f(gap):
        return (1 - gap / pts_remaining).clip(lower=0)

    title_raw = gw_factor * _f(df["title_gap"])
    cl_raw = gw_factor * _f(df["cl_gap"]) * config.CL_WEIGHT
    eur_raw = gw_factor * _f(df["eur_gap"]) * config.EUR_WEIGHT
    releg_raw = gw_factor * _f(df["releg_gap"]) * config.RELEG_WEIGHT
    base_stakes = np.maximum.reduce([title_raw, cl_raw, eur_raw, releg_raw]) / config.GW38_MAX

    recent_form = df.groupby(["team", "season"])["points"].transform(
        lambda x: x.shift(1).rolling(3, min_periods=1).mean()
    ).fillna(1.5)
    form_mult = np.where(recent_form < config.FORM_POOR_THRESHOLD, config.FORM_MULT_BONUS, 1.0)
    rivalry_bonus = np.where(df["is_rivalry"], config.RIVALRY_BONUS, 0)

    df["stakes_intensity"] = (base_stakes * form_mult + rivalry_bonus).clip(0, 1).round(4)
    return df


def add_h2h(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    h2h = df[["team", "opponent", "date", "points"]].sort_values(["team", "opponent", "date"]).copy()
    h2h["h2h_pts_avg3"] = h2h.groupby(["team", "opponent"])["points"].transform(
        lambda x: x.shift(1).rolling(3, min_periods=1).mean()
    )
    df = df.merge(h2h[["team", "opponent", "date", "h2h_pts_avg3"]], on=["team", "opponent", "date"], how="left")
    df["h2h_pts_avg3"] = df["h2h_pts_avg3"].fillna(1.5)
    return df


def add_recency_weight(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["recency_weight"] = df["season"].map(config.RECENCY_WEIGHTS)
    return df


def build_features(df: pd.DataFrame) -> pd.DataFrame:
    """Full pipeline, in the same order NB07 applies it. `df` must have the
    base reshape columns from data.wide_to_long (one row per team per match,
    played or not)."""
    df = df.sort_values(["team", "season", "date"]).reset_index(drop=True)
    df = add_result_columns(df)
    df = add_rolling_features(df)
    df = add_opponent_quality(df)
    df = add_stakes_intensity(df)
    df = add_h2h(df)
    df = add_recency_weight(df)
    return df
