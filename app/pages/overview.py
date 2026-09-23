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
    "Arsenal led the title race for long stretches of three straight seasons and lost all "
    "three, then won the league in 2025-26. This project takes that story apart with seven "
    "seasons of match data, tests whether the numbers actually support it, and then simulates "
    "the season currently being played.",
)

st.write("")
ui.metric_grid([
    ("7", "Seasons", "2019-20 through 2025-26, plus the live 2026-27 season"),
    (f"{len(history):,}", "Matches", "Every Premier League match across all 20 clubs"),
    ("11", "Features", "Engineered from raw match data, including a custom pressure metric"),
    ("4", "Models", "Compared on an identical forward-in-time holdout"),
    ("10,000", "Simulations", "Monte Carlo runs of the remaining season"),
    ("35", "Checks", "Automated verifications run every time the data is rebuilt"),
])
ui.updated_banner(state)

st.divider()

# ---------------------------------------------------------------------------
st.header("What the data actually said", anchor=False)
st.markdown(
    "Three findings, each of which contradicted what we expected going in. The project is "
    "built around following the data when it disagreed with the story, which happened more "
    "than once."
)

ui.cards([
    ("The bottle is in points, not performance",
     "Arsenal created chances at the same rate in their biggest matches as their ordinary "
     "ones. What fell away was the points they took from them. The collapse is about "
     "converting pressure into results, not about playing worse football."),
    ("And it does not survive a real test",
     "Put through permutation tests, signed-rank tests and multiple-comparison correction, "
     "not one finding stays significant. Ten big matches a season is too small a sample to "
     "settle the question either way."),
    ("One of our own findings was wrong",
     "A headline result about high-stakes matches clustering among a team's best results "
     "collapsed when we checked how ties were being broken. It is corrected on the site "
     "rather than quietly deleted."),
])

st.write("")
c1, c2, c3 = st.columns(3)
c1.page_link("pages/bottle.py", label="See the evidence", icon=":material/query_stats:")
c2.page_link("pages/significance.py", label="See it tested", icon=":material/science:")
c3.page_link("pages/model.py", label="See the model", icon=":material/network_node:")

st.divider()

# ---------------------------------------------------------------------------
st.header("The title race, right now", anchor=False)

top = summary.head(6).sort_values("title_prob")
fig = go.Figure(go.Bar(
    x=top["title_prob"] * 100,
    y=top.index,
    orientation="h",
    marker=dict(color=[theme.team_color(t) for t in top.index], line=dict(width=0)),
    text=[f"{v*100:.1f}%" for v in top["title_prob"]],
    textposition="outside",
    textfont=dict(color=theme.COLOR["text_primary"], size=12),
    cliponaxis=False,
    hovertemplate="<b>%{y}</b><br>Wins the league in %{x:.1f}% of simulations<extra></extra>",
))
theme.apply(fig, height=290, legend=False,
            xaxis=dict(title=dict(text="Probability of winning the league (%)"),
                       range=[0, float(top["title_prob"].max()) * 132]))
ui.chart(
    fig,
    title=f"{summary.index[0]} leads the title race after gameweek "
          f"{state['last_published_gameweek']}",
    verdict_text=(
        f"Out of 10,000 simulated versions of the rest of this season, "
        f"<strong>{summary.index[0]}</strong> finished top in "
        f"{summary.iloc[0]['title_prob']*100:.1f}% of them. Every remaining fixture is played "
        "out match by match, so these are not odds anyone set by hand, they are counts of how "
        "often each club actually won."
    ),
)

if state["last_published_gameweek"] <= 8:
    ui.callout(
        "caveat", "Early-season caveat.",
        f"Only {state['last_published_gameweek']} gameweeks have been played. The model reads "
        "each club's form from its last five matches, and that window restarts every August, "
        "so right now it is working from a very small sample. Tested on last season, the same "
        "method was only 29.7% confident in the eventual champion at this stage, against 85.7% "
        "by the midpoint. Treat this as a snapshot, not a forecast.",
    )

st.page_link("pages/predictor.py", label="Open the full predictor",
             icon=":material/arrow_forward:")

st.divider()

# ---------------------------------------------------------------------------
st.header("How the project is built", anchor=False)

c1, c2 = st.columns(2, gap="medium")
with c1.container(border=True, height=290):
    st.markdown("**The analysis**")
    st.markdown("""
1. Scrape seven seasons of shot-level data from Understat
2. Engineer the features, including a custom pressure metric
3. Explore Arsenal's story season by season
4. Check whether the pattern is unique to Arsenal
5. Test every claim, correcting for testing many at once
""")
with c2.container(border=True, height=290):
    st.markdown("**The system**")
    st.markdown("""
1. Train and compare four models on the outcome of a match
2. Simulate the remaining season 10,000 times
3. Publish it as this site, rebuilt after every gameweek

A scheduled job checks daily whether a full gameweek has finished,
and only then refits, resimulates and republishes.
""")

ui.callout(
    "scope", "Everything here is recomputed, not copied.",
    "Every number on this site comes from the same code that runs the live pipeline. Nothing "
    "is typed in by hand, and 35 automated checks compare the fresh output against the "
    "project's recorded values every time it rebuilds. The **Method** page has the detail.",
)

ui.prev_next("pages/overview.py")
