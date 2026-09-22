"""Shared constants for the live app pipeline.

This mirrors notebooks/07_monte_carlo_simulation.ipynb (Step 1, Section E, Section G).
It is a hand-kept copy, not an import from the notebook. If the notebook's RIVALRIES,
recency weights or feature set ever change, this file has to be updated by hand and
the verification checks re-run against the notebook's own printed numbers.
"""
import numpy as np
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_RAW_DIR = PROJECT_ROOT / "data" / "raw"
DATA_PROCESSED_DIR = PROJECT_ROOT / "data" / "processed"
DATA_APP_DIR = PROJECT_ROOT / "data" / "app"

LEAGUE = "ENG-Premier League"
CURRENT_SEASON = "2627"
ARTETA_SEASONS = ["1920", "2021", "2122", "2223", "2324", "2425", "2526"]

TITLE_TEAMS = ["Arsenal", "Liverpool", "Manchester City", "Manchester United"]
BIG6 = ["Arsenal", "Liverpool", "Manchester City", "Manchester United", "Chelsea", "Tottenham"]

# Copied from notebooks/07_monte_carlo_simulation.ipynb Step 1 (the user's 20-team
# version, not the 4-team dict still used in tutorials/07_monte_carlo_simulation_tutorial.ipynb).
RIVALRIES = {
    "Arsenal": ["Tottenham", "Manchester United", "Chelsea", "Manchester City"],
    "Brentford": ["Fulham", "Chelsea"],
    "Everton": ["Liverpool"],
    "Hull": ["Leeds"],
    "Ipswich": [],  # Main rival (Norwich City) is not in the Premier League this season
    "Nottingham Forest": ["Coventry", "Aston Villa"],
    "Brighton": ["Crystal Palace", "Bournemouth"],
    "Manchester City": ["Manchester United", "Liverpool", "Arsenal"],
    "Newcastle United": ["Sunderland"],
    "Fulham": ["Chelsea", "Brentford"],
    "Crystal Palace": ["Brighton"],
    "Bournemouth": ["Brighton"],
    "Coventry": ["Aston Villa", "Nottingham Forest"],
    "Liverpool": ["Manchester United", "Everton", "Manchester City", "Chelsea"],
    "Tottenham": ["Arsenal", "Chelsea"],
    "Chelsea": ["Arsenal", "Tottenham", "Fulham", "Leeds", "Liverpool"],
    "Leeds": ["Manchester United", "Chelsea", "Hull"],
    "Manchester United": ["Liverpool", "Manchester City", "Leeds", "Arsenal"],
    "Sunderland": ["Newcastle United"],
    "Aston Villa": ["Coventry", "Nottingham Forest"],
}

TEAM_PALETTE = {
    "Arsenal": "#EF0107",
    "Liverpool": "#00B2A9",
    "Manchester City": "#6CABDD",
    "Manchester United": "#FFB81C",
}

# recency_weight, sample_weight at fit time. 2627 added per NB07 Section G (1.6).
RECENCY_WEIGHTS = {
    "1920": 1.0, "2021": 1.0, "2122": 1.0, "2223": 1.1,
    "2324": 1.2, "2425": 1.3, "2526": 1.5, "2627": 1.6,
}

Y_MAP = {"L": 0, "D": 1, "W": 2}

# Final 11-feature recipe, NB07 Section F/G.
FEATURES_FINAL = [
    "xG_roll5", "xGA_roll5", "pts_roll5", "win_rate_roll5", "is_home",
    "is_big6_opp", "opp_xgd_roll5", "parity_gap", "stakes_intensity",
    "is_non_big6_rivalry", "h2h_pts_avg3",
]

# Fixture-level columns that don't depend on the simulated table (everything
# except stakes_intensity, which is recomputed every simulated gameweek).
STATIC_COLS = [
    "xG_roll5", "xGA_roll5", "pts_roll5", "win_rate_roll5", "is_home", "is_big6_opp",
    "opp_xgd_roll5", "parity_gap", "is_non_big6_rivalry", "h2h_pts_avg3",
]

SNAP_COLS = ["xG_roll5", "xGA_roll5", "pts_roll5", "win_rate_roll5", "xgd_roll5"]

# stakes_intensity formula constants, NB02 + NB07 Section E (relegation boundary).
GW_SIGMOID_K = 0.2
GW_SIGMOID_MID = 22
GW38_MAX = 1 / (1 + np.exp(-GW_SIGMOID_K * (38 - GW_SIGMOID_MID)))
TITLE_WEIGHT = 1.0
CL_WEIGHT = 0.75
EUR_WEIGHT = 0.50
RELEG_WEIGHT = 0.80
FORM_POOR_THRESHOLD = 1.0
FORM_MULT_BONUS = 1.15
RIVALRY_BONUS = 0.15

# Simulation seeds, NB07 Section I/J.
LIVE_SEED = 42
BACKTEST_SEED = 7
CONVERGENCE_SEED = 1
TIEBREAK_SEED = 123
N_RUNS = 10000
