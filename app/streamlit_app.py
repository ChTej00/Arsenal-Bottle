"""Arsenal Bottle: the story, a team comparator, and a live 2026-27 predictor.

Reads only precomputed files from data/app/, written by src/update.py. Never
scrapes Understat at page load. Run locally with:
    streamlit run app/streamlit_app.py
"""
import json
import sys
from pathlib import Path

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

APP_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(APP_ROOT))

from src import config, engine, model  # noqa: E402

DATA_APP = config.DATA_APP_DIR
TRACKED_TEAMS = config.TITLE_TEAMS
TEAM_COLORS = config.TEAM_PALETTE
DEFAULT_COLOR = "#7A7A7A"

st.set_page_config(page_title="Arsenal Bottle", layout="wide")


# ---------------------------------------------------------------------------
# Data loading. Cached so a page interaction doesn't re-read disk every time;
# a fresh deploy (new commit to data/app/) starts a new process, so the cache
# never serves stale data across an actual update.
# ---------------------------------------------------------------------------

@st.cache_data
def load_state() -> dict:
    return json.loads((DATA_APP / "state.json").read_text())


@st.cache_data
def load_summary() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "summary.csv").set_index("team")


@st.cache_data
def load_odds_history() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "odds_history.csv")


@st.cache_data
def load_fixtures() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "fixtures.csv")


@st.cache_data
def load_current_points() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "current_points.csv")


@st.cache_resource
def load_model():
    return model.load(DATA_APP / "model.json")


@st.cache_data
def load_team_pressure() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "team_pressure.csv")


@st.cache_data
def load_team_progress() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "team_season_progress.csv")


@st.cache_data
def load_pri() -> pd.DataFrame:
    return pd.read_csv(DATA_APP / "pressure_resilience_index.csv")


def _team_order() -> list:
    cp = load_current_points()
    return sorted(cp["team"].tolist())


def _current_pts_array(teams: list):
    cp = load_current_points().set_index("team")
    return cp.reindex(teams)["current_pts"].values.astype(float)


@st.cache_data(show_spinner="Running 10,000 simulated seasons...")
def run_baseline_simulation() -> pd.DataFrame:
    teams = _team_order()
    fx = load_fixtures()
    fitted = load_model()
    final_pts = engine.run_simulation(fx, _current_pts_array(teams), fitted,
                                       n_runs=config.N_RUNS, seed=config.LIVE_SEED)
    return engine.summarize(final_pts, teams)


@st.cache_data(show_spinner="Resimulating with your fixed result...")
def run_whatif_simulation(game_id: int, outcome: int) -> pd.DataFrame:
    teams = _team_order()
    fx = load_fixtures()
    fitted = load_model()
    final_pts = engine.run_simulation(fx, _current_pts_array(teams), fitted,
                                       n_runs=config.N_RUNS, seed=config.LIVE_SEED,
                                       overrides={game_id: outcome})
    return engine.summarize(final_pts, teams)


def team_color(team: str) -> str:
    return TEAM_COLORS.get(team, DEFAULT_COLOR)


# ---------------------------------------------------------------------------
# Header, shared across tabs
# ---------------------------------------------------------------------------

state = load_state()
st.title("Arsenal Bottle")
st.caption(
    f"Updated after gameweek {state['last_published_gameweek']} of 2026-27 "
    f"(last run {state['updated_at'][:16].replace('T', ' ')} UTC, model trained on {state['training_rows']} rows)."
)

tab1, tab2, tab3 = st.tabs(["The Arsenal Story", "Team Comparator", "Live 2026-27 Predictor"])


# ---------------------------------------------------------------------------
# Tab 1: The Arsenal Story
# ---------------------------------------------------------------------------

with tab1:
    st.header("Four acts of a title race")
    st.markdown(
        "Arsenal led the Premier League title race for long stretches in three straight seasons "
        "(2022-23, 2023-24, 2024-25) and finally won it in 2025-26. This project asks what actually "
        "separated the seasons they lost from the one they won, using the data rather than the "
        "narrative around it."
    )

    st.subheader("What the data actually showed")
    st.markdown(
        "The first hypothesis was that Arsenal's process collapsed under pressure: worse chance creation "
        "and prevention in high-stakes matches. That is not what the data says. Arsenal's chance creation "
        "(xG) is statistically flat between high-stakes and normal matches. Chance prevention (xGA) is not: "
        "1.32 xG conceded in high-stakes matches against 1.05 normal, a real gap (p=0.031 before correction). "
        "The clearer signal is in points, not process: Arsenal's points-per-game in their highest-stakes "
        "matches falls below their own season baseline in both true bottle seasons, and rises above it in "
        "2023-24 and 2025-26."
    )

    pressure = load_team_pressure()
    progress = load_team_progress()
    ars_pressure = pressure[pressure["team"] == "Arsenal"].sort_values("season")
    ars_progress = progress[progress["team"] == "Arsenal"].sort_values("season")

    season_labels = {
        "1920": "19-20", "2021": "20-21", "2122": "21-22", "2223": "22-23",
        "2324": "23-24", "2425": "24-25", "2526": "25-26",
    }
    ars_pressure = ars_pressure.assign(season_label=ars_pressure["season"].astype(str).map(season_labels))
    ars_progress = ars_progress.assign(season_label=ars_progress["season"].astype(str).map(season_labels))

    col1, col2 = st.columns(2)
    with col1:
        fig = px.bar(
            ars_pressure, x="season_label", y="ppg_gap",
            title="The Bottle Gap: high-stakes PPG minus season baseline",
            labels={"season_label": "Season", "ppg_gap": "PPG gap"},
            color=ars_pressure["ppg_gap"] > 0,
            color_discrete_map={True: "#2E8B57", False: TEAM_COLORS["Arsenal"]},
        )
        fig.update_layout(showlegend=False)
        st.plotly_chart(fig, use_container_width=True)
        st.caption("22-23 and 24-25, the two true bottle seasons, are the only two below zero.")
    with col2:
        fig = px.line(
            ars_progress, x="season_label", y="ppg", markers=True,
            title="Points per game by season",
            labels={"season_label": "Season", "ppg": "PPG"},
        )
        fig.update_traces(line_color=TEAM_COLORS["Arsenal"])
        st.plotly_chart(fig, use_container_width=True)

    st.subheader("Does it hold up to a real test?")
    st.markdown(
        "NB05 put every finding above through permutation tests, Wilcoxon signed-rank tests and multiple-"
        "comparisons correction. Nothing survives. The xGA gap doesn't survive correcting for testing 8 "
        "hypotheses at once. The Bottle Gap itself, tested two different ways, comes back null both times. "
        "This isn't proof the effect isn't real: a power analysis shows detecting a genuine effect at this "
        "sample size (10 high-stakes matches a season) needs a very large effect, d=1.06, and the effects "
        "actually observed are far smaller (d=0.05 to 0.19). Ten matches a season can't settle this either "
        "way. It's a power problem, not a null result."
    )

    st.subheader("Does a model predict it?")
    st.markdown(
        "NB06 built a real classifier (logistic regression beat both random forest and XGBoost on log loss) "
        "and found stakes_intensity's status genuinely contested: not significant in the linear model once "
        "opponent quality and form are included, but ranked as comparably important to opponent quality by "
        "XGBoost's SHAP values. Seven follow-up ideas were tried to push accuracy past 56%, and only one "
        "helped, and only slightly. The honest conclusion: this feature set is close to its ceiling, and "
        "getting further needs genuinely new information, not a cleverer model."
    )
    st.caption("Full reasoning and every rejected idea, with its numbers, is in decisions_log.md.")


# ---------------------------------------------------------------------------
# Tab 2: Team Comparator
# ---------------------------------------------------------------------------

with tab2:
    st.header("Team Comparator")
    st.markdown(
        "Covers the four teams with full match-level pressure analysis: Arsenal, Liverpool, Manchester "
        "City and Manchester United. The other sixteen clubs were never analysed at this level of detail "
        "in NB03/NB04, so they aren't part of this comparison."
    )

    pressure = load_team_pressure()
    pri = load_pri()

    c1, c2 = st.columns(2)
    with c1:
        team_a = st.selectbox("Team A", TRACKED_TEAMS, index=0)
    with c2:
        team_b = st.selectbox("Team B", TRACKED_TEAMS, index=1)

    if team_a == team_b:
        st.warning("Pick two different teams to compare.")
    else:
        compare_df = pressure[pressure["team"].isin([team_a, team_b])].copy()
        compare_df["season_label"] = compare_df["season"].astype(str).map(season_labels)

        fig = px.bar(
            compare_df, x="season_label", y="ppg_gap", color="team", barmode="group",
            title="Bottle Gap by season",
            labels={"season_label": "Season", "ppg_gap": "PPG gap"},
            color_discrete_map={team_a: team_color(team_a), team_b: team_color(team_b)},
        )
        st.plotly_chart(fig, use_container_width=True)

        col1, col2 = st.columns(2)
        with col1:
            pri_a = pri.loc[pri["team"] == team_a, "pressure_resilience_index"].iloc[0]
            pri_b = pri.loc[pri["team"] == team_b, "pressure_resilience_index"].iloc[0]
            st.metric(f"{team_a} Pressure Resilience Index", f"{pri_a:+.3f}")
            st.metric(f"{team_b} Pressure Resilience Index", f"{pri_b:+.3f}")
            st.caption("Mean PPG gap across each team's contention seasons (>=5 high-stakes matches that season).")
        with col2:
            st.markdown("**xG/xGA under high stakes vs normal**")
            xg_table = compare_df[["team", "xG_high_stakes", "xG_normal", "xG_pvalue",
                                    "xGA_high_stakes", "xGA_normal", "xGA_pvalue"]].groupby("team").mean().round(3)
            st.dataframe(xg_table)
            st.caption(
                "p-values are per-season Welch's t-tests, uncorrected. NB05 found none of these survive "
                "correction for testing multiple teams and seasons at once."
            )


# ---------------------------------------------------------------------------
# Tab 3: Live 2026-27 Predictor
# ---------------------------------------------------------------------------

with tab3:
    st.header("2026-27 Title Race")

    gw = state["last_published_gameweek"]
    if gw <= 8:
        st.warning(
            f"Only {gw} gameweeks played. Rolling form restarts every season, so this early in the year "
            "the odds rest on a handful of matches and are the least reliable part of this project. A "
            "GW5 backtest of the completed 2025-26 season gave Arsenal 29.7% title probability and only "
            "14 of 20 clubs inside their predicted range, against 85.7% and 17 of 20 once a full season's "
            "form (GW20) was available. Read these numbers as a snapshot, not a settled forecast."
        )

    summary = run_baseline_simulation()

    st.subheader("Title race")
    top8 = summary.head(8).sort_values("title_prob")
    fig = go.Figure(go.Bar(
        x=top8["title_prob"] * 100, y=top8.index, orientation="h",
        marker_color=[team_color(t) for t in top8.index],
    ))
    fig.update_layout(xaxis_title="Title probability (%)", title="Title probability, top 8 clubs")
    st.plotly_chart(fig, use_container_width=True)

    st.subheader("Projected final points")
    by_median = summary.sort_values("pts_median")
    fig = go.Figure()
    for team, r in by_median.iterrows():
        fig.add_trace(go.Scatter(
            x=[r["pts_p5"], r["pts_p95"]], y=[team, team], mode="lines",
            line=dict(color=team_color(team), width=6), opacity=0.6, showlegend=False,
        ))
        fig.add_trace(go.Scatter(
            x=[r["pts_median"]], y=[team], mode="markers",
            marker=dict(color=team_color(team), size=8), showlegend=False,
        ))
    fig.update_layout(xaxis_title="Final points", title="Median and 5th-95th percentile range",
                       height=650)
    st.plotly_chart(fig, use_container_width=True)

    col1, col2 = st.columns(2)
    with col1:
        st.subheader("Top 4")
        st.dataframe((summary["top4_prob"] * 100).round(1).sort_values(ascending=False).head(8)
                     .rename("Top-4 probability (%)"))
    with col2:
        st.subheader("Relegation")
        st.dataframe((summary["releg_prob"] * 100).round(1).sort_values(ascending=False).head(8)
                     .rename("Relegation probability (%)"))

    st.subheader("How title odds have moved")
    odds_hist = load_odds_history()
    if odds_hist["gameweek"].nunique() < 2:
        st.caption("Only one published gameweek so far; the odds-movement chart needs at least two to draw a line.")
    else:
        top_teams = summary.head(6).index.tolist()
        hist_top = odds_hist[odds_hist["team"].isin(top_teams)]
        fig = px.line(
            hist_top, x="gameweek", y="title_prob", color="team", markers=True,
            labels={"gameweek": "Gameweek", "title_prob": "Title probability"},
            color_discrete_map={t: team_color(t) for t in top_teams},
        )
        fig.update_yaxes(tickformat=".0%")
        st.plotly_chart(fig, use_container_width=True)

    st.subheader("What if...")
    st.markdown("Fix one remaining fixture's result and resimulate the rest of the season around it.")
    fixtures = load_fixtures().sort_values(["gameweek", "date"])
    fixtures["label"] = ("GW" + fixtures["gameweek"].astype(str) + ": "
                          + fixtures["home_team"] + " vs " + fixtures["away_team"])

    picked_label = st.selectbox("Fixture", fixtures["label"].tolist())
    picked = fixtures[fixtures["label"] == picked_label].iloc[0]
    outcome_label = st.radio(
        "Result", [f"{picked['home_team']} win", "Draw", f"{picked['away_team']} win"], horizontal=True,
    )
    outcome_map = {f"{picked['home_team']} win": 2, "Draw": 1, f"{picked['away_team']} win": 0}

    if st.button("Resimulate"):
        wi_summary = run_whatif_simulation(int(picked["game_id"]), outcome_map[outcome_label])
        compare_teams = sorted(set(summary.head(6).index) | {picked["home_team"], picked["away_team"]})
        compare = pd.DataFrame({
            "Before": (summary.loc[compare_teams, "title_prob"] * 100).round(1),
            "After": (wi_summary.loc[compare_teams, "title_prob"] * 100).round(1),
        })
        compare["Change"] = (compare["After"] - compare["Before"]).round(1)
        st.dataframe(compare)
        st.caption("Title probability (%), before and after fixing this one result.")
