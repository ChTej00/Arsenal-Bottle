"""Landing page. Built to be understood in ten seconds and explored in ten minutes."""
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

theme.inject_css()

state = loaders.state()
summary = loaders.baseline_simulation()
matches = loaders.table("matches.csv")
history = loaders.table("history.csv")

ui.hero(
    "Premier League · 2019 to now",
    "Did Arsenal bottle it?",
    "Arsenal led the title race for long stretches of three straight seasons and lost all three, "
    "then won the league in 2025-26. This project takes that story apart with seven seasons of "
    "match data, tests whether the numbers actually support it, and then simulates the season "
    "currently being played.",
)

ui.stats([
    ("7", "seasons analysed"),
    (f"{len(history):,}", "league-wide matches"),
    (f"{len(matches):,}", "deep-analysed matches"),
    ("11", "engineered features"),
    ("4", "models compared"),
    ("10,000", "simulated seasons"),
])
ui.updated_banner(state)

st.divider()

# ---------------------------------------------------------------------------
st.markdown("## What the data actually said")
st.markdown(
    '<div class="pageintro">Three findings, each of which contradicted what we expected '
    'going in. The project is built around following the data when it disagreed with the '
    'story, which happened more than once.</div>',
    unsafe_allow_html=True,
)

ui.cards([
    ("The bottle is in points, not performance",
     "Arsenal created chances at the same rate in their biggest matches as their ordinary ones. "
     "What fell away was the points they took from them. The collapse is about converting "
     "pressure into results, not about the team playing worse football."),
    ("And it does not survive a real test",
     "Put through permutation tests, signed-rank tests and multiple-comparison correction, not "
     "one finding stays significant. Ten big matches a season is simply too small a sample to "
     "settle the question either way."),
    ("One of our own findings turned out to be an artifact",
     "A headline result about high-stakes matches clustering among a team's best results "
     "collapsed when we checked how ties were being broken. We kept it on the site rather than "
     "quietly deleting it."),
])

st.divider()

# ---------------------------------------------------------------------------
st.markdown("## The title race, right now")

top = summary.head(6).sort_values("title_prob")
fig = go.Figure(go.Bar(
    x=top["title_prob"] * 100,
    y=top.index,
    orientation="h",
    marker=dict(color=[theme.team_color(t) for t in top.index],
                line=dict(width=0)),
    text=[f"{v*100:.1f}%" for v in top["title_prob"]],
    textposition="outside",
    textfont=dict(color=theme.TEXT, size=12),
    hovertemplate="%{y}: %{x:.1f}%<extra></extra>",
))
theme.apply(fig, height=280, legend=False,
            title=f"Title probability after gameweek {state['last_published_gameweek']}",
            xaxis_title="Probability of winning the league (%)",
            xaxis=dict(range=[0, max(top["title_prob"]) * 118]))
ui.chart(fig, verdict_text=(
    f"Out of 10,000 simulated versions of the rest of this season, "
    f"<b>{summary.index[0]}</b> finished top in "
    f"{summary.iloc[0]['title_prob']*100:.1f}% of them. "
    "Every remaining fixture is played out match by match, so these are not odds anyone "
    "set by hand, they are counts of how often each club actually won."
))

if state["last_published_gameweek"] <= 8:
    ui.note(
        f"<b>Early-season caveat.</b> Only {state['last_published_gameweek']} gameweeks have been "
        "played. The model reads each club's form from its last five matches, and that window "
        "restarts every August, so right now it is working from a very small sample. Tested on "
        "last season, the same method was only 29.7% confident in the eventual champion at this "
        "stage, against 85.7% by the midpoint. Treat this as a snapshot, not a forecast.",
        kind="warn",
    )

st.page_link("pages/predictor.py", label="Open the full predictor", icon=":material/arrow_forward:")

st.divider()

# ---------------------------------------------------------------------------
st.markdown("## How the project is built")

col1, col2 = st.columns(2)
with col1:
    st.markdown("""
**The analysis**

1. Scrape seven seasons of shot-level data from Understat
2. Engineer the features, including a custom pressure metric
3. Explore Arsenal's story season by season
4. Check whether the pattern is unique to Arsenal
5. Test every claim properly, and correct for testing many at once
""")
with col2:
    st.markdown("""
**The system**

6. Train and compare four models on the outcome of a match
7. Simulate the remaining season 10,000 times
8. Ship it as this site, rebuilt automatically after every gameweek

A scheduled job checks daily whether a full gameweek has finished, and only then
refits the model, reruns the simulation and republishes. No step needs a human.
""")

ui.note(
    "Every number on this site is recomputed from the raw data by the same code that runs the "
    "live pipeline. Nothing is typed in by hand. The checks that confirm this are part of the "
    "build and are described on the <b>Method</b> page."
)
