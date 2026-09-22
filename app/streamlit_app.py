"""Arsenal Bottle: entry point and navigation.

Nine pages, each a script under app/pages/. Every page reads only precomputed
files from data/app/, written by src/update.py (live simulation), src/exports.py
(analysis tables) and src/model_exports.py (model artifacts). Nothing here ever
scrapes Understat or fits a model at page load.

Run locally with:  streamlit run app/streamlit_app.py
"""
import sys
from pathlib import Path

import streamlit as st

APP_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = APP_DIR.parent
for p in (str(PROJECT_ROOT), str(APP_DIR)):
    if p not in sys.path:
        sys.path.insert(0, p)

st.set_page_config(
    page_title="Arsenal Bottle",
    page_icon="⚽",
    layout="wide",
    initial_sidebar_state="expanded",
)

PAGES = APP_DIR / "pages"

nav = st.navigation({
    "Start here": [
        st.Page(PAGES / "overview.py", title="Overview", icon=":material/home:", default=True),
    ],
    "The story": [
        st.Page(PAGES / "rise.py", title="Act 1 · The Rise", icon=":material/trending_up:"),
        st.Page(PAGES / "bottle.py", title="Act 2 · The Bottle", icon=":material/heart_broken:"),
        st.Page(PAGES / "breakthrough.py", title="Act 3 · The Breakthrough", icon=":material/trophy:"),
        st.Page(PAGES / "compare.py", title="Team comparator", icon=":material/compare_arrows:"),
    ],
    "The evidence": [
        st.Page(PAGES / "significance.py", title="Is it real?", icon=":material/science:"),
        st.Page(PAGES / "model_page.py", title="The model", icon=":material/network_node:"),
    ],
    "Live": [
        st.Page(PAGES / "predictor.py", title="Act 4 · 2026-27 predictor", icon=":material/insights:"),
    ],
    "Reference": [
        st.Page(PAGES / "method.py", title="Method & glossary", icon=":material/book:"),
    ],
})

with st.sidebar:
    st.markdown("### Arsenal Bottle")
    st.caption("Seven seasons of Premier League data, one question: did Arsenal bottle it?")

nav.run()

with st.sidebar:
    st.divider()
    st.caption(
        "[Source on GitHub](https://github.com/ChTej00/Arsenal-Bottle) · "
        "Data from Understat"
    )
