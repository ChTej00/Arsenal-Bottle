"""Cached readers for everything in data/app/.

One function per file. All cached, so a page interaction never re-reads disk.
A real data change arrives as a new commit, which restarts the process on
Streamlit Cloud, so the cache can never serve stale numbers across an update.
"""
import json

import pandas as pd
import streamlit as st

from src import config, engine, model

DATA = config.DATA_APP_DIR


@st.cache_data
def table(name: str) -> pd.DataFrame:
    """Any CSV in data/app/ by filename, with season normalised to a string."""
    df = pd.read_csv(DATA / name)
    if "season" in df.columns:
        df["season"] = df["season"].astype(str)
    if "date" in df.columns:
        df["date"] = pd.to_datetime(df["date"])
    return df


@st.cache_data
def state() -> dict:
    return json.loads((DATA / "state.json").read_text())


@st.cache_data
def summary() -> pd.DataFrame:
    return pd.read_csv(DATA / "summary.csv").set_index("team")


@st.cache_resource
def fitted_model():
    return model.load(DATA / "model.json")


def _team_order() -> list:
    return sorted(table("current_points.csv")["team"].tolist())


def _start_points(teams: list):
    cp = table("current_points.csv").set_index("team")
    return cp.reindex(teams)["current_pts"].values.astype(float)


@st.cache_data(show_spinner="Simulating 10,000 seasons...")
def baseline_simulation() -> pd.DataFrame:
    teams = _team_order()
    final = engine.run_simulation(table("fixtures.csv"), _start_points(teams),
                                  fitted_model(), n_runs=config.N_RUNS,
                                  seed=config.LIVE_SEED)
    return engine.summarize(final, teams)


@st.cache_data(show_spinner="Simulating 10,000 seasons...")
def baseline_points() -> tuple:
    """Raw final-points matrix (n_runs x 20) plus the team order, so pages can
    draw full outcome distributions instead of only the percentile summary."""
    teams = _team_order()
    final = engine.run_simulation(table("fixtures.csv"), _start_points(teams),
                                  fitted_model(), n_runs=config.N_RUNS,
                                  seed=config.LIVE_SEED)
    return final, teams


@st.cache_data(show_spinner="Resimulating with your fixed results...")
def whatif_simulation(overrides: tuple) -> pd.DataFrame:
    """overrides is a tuple of (game_id, outcome) pairs, hashable so the cache
    can key on it."""
    teams = _team_order()
    final = engine.run_simulation(table("fixtures.csv"), _start_points(teams),
                                  fitted_model(), n_runs=config.N_RUNS,
                                  seed=config.LIVE_SEED, overrides=dict(overrides))
    return engine.summarize(final, teams)
