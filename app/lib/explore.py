"""The one place the app is allowed to compute a number rather than read one.

Everywhere else on this site the app is a read-only consumer of figures the
pipeline produced. The Act 2 threshold control is the exception: it lets a
reader redefine "a big match" and watch the bottle gap move, which cannot be
precomputed because the reader chooses the setting.

That makes drift a real risk. If this recomputation differed even slightly from
the pipeline's, the control would disagree with the published figure at the
published setting, on the same page. So `verified_gaps()` refuses to return
anything unless the top-25% case reproduces `team_pressure.csv` exactly, and the
page shows an error instead of a wrong chart if it ever stops matching.

The definitions mirrored here are:
  NB02 cell 23        cutoff = per team-season quantile of stakes_intensity,
                      flagged when stakes_intensity >= cutoff
  src/comparator.py   gap = high-stakes PPG minus that season's own mean PPG
"""
import pandas as pd
import streamlit as st

PUBLISHED_TOP_PCT = 0.25
TOLERANCE = 1e-9


def _gap_table(matches: pd.DataFrame, top_pct: float) -> pd.DataFrame:
    """Bottle gap per team-season with 'big match' set to the top `top_pct`."""
    d = matches[["team", "season", "points", "stakes_intensity"]].copy()
    cutoff = d.groupby(["team", "season"])["stakes_intensity"].transform(
        lambda x: x.quantile(1 - top_pct)
    )
    flagged = d[d["stakes_intensity"] >= cutoff]

    grouped = d.groupby(["team", "season"])
    out = pd.DataFrame({
        "baseline_ppg": grouped["points"].mean(),
        "high_stakes_ppg": flagged.groupby(["team", "season"])["points"].mean(),
        "n_matches": flagged.groupby(["team", "season"]).size(),
    })
    out["ppg_gap"] = out["high_stakes_ppg"] - out["baseline_ppg"]
    return out.reset_index()


@st.cache_data
def drift_check(matches: pd.DataFrame, published: pd.DataFrame) -> str | None:
    """None if the recompute reproduces the published figures at the published
    setting, otherwise a message describing the drift."""
    recomputed = _gap_table(matches, PUBLISHED_TOP_PCT)
    merged = published[["team", "season", "ppg_gap"]].merge(
        recomputed[["team", "season", "ppg_gap"]],
        on=["team", "season"], suffixes=("_pub", "_new"), how="outer",
    )
    if merged["ppg_gap_pub"].isna().any() or merged["ppg_gap_new"].isna().any():
        return "the recomputation and the published table cover different team-seasons"
    worst = (merged["ppg_gap_pub"] - merged["ppg_gap_new"]).abs().max()
    if worst > TOLERANCE:
        return (f"at the published top-25% setting the recomputed bottle gap differs "
                f"from the published figure by up to {worst:.4f}")
    return None


@st.cache_data
def verified_gaps(matches: pd.DataFrame, published: pd.DataFrame,
                  top_pct: float) -> pd.DataFrame | None:
    """The gap table at `top_pct`, or None if the drift check has failed."""
    if drift_check(matches, published) is not None:
        return None
    return _gap_table(matches, top_pct)
