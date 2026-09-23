"""How everything on this site is actually built, and what the words mean."""
import numpy as np
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui
from src import config

ui.page_header(
    "How all of this works",
    "Everything behind the rest of the site: where the data comes from, how the pressure "
    "metric is built, what protects the model from cheating, and what all the terms mean.",
    eyebrow="Method & glossary",
)

glossary = loaders.table("feature_glossary.csv")
normality = loaders.table("normality.csv")
state = loaders.state()

# ---------------------------------------------------------------------------
st.markdown("### The pipeline")

c1, c2, c3 = st.columns(3)
with c1:
    st.markdown("""
**Data**

Shot-level Premier League data from Understat, 2019-20 to now. Every shot carries an expected
goal value, which aggregates to team-level xG and xGA per match.

Seven completed seasons of all 20 clubs, plus four clubs analysed at full match-level depth
for the pressure work.
""")
with c2:
    st.markdown("""
**Features**

Rolling five-match form, opponent quality, home advantage, rivalry, head-to-head record,
and a custom pressure score.

Every rolling feature is shifted by one match before it is computed, so a match is never
described using its own result.
""")
with c3:
    st.markdown("""
**Publishing**

A scheduled job runs daily and checks whether a full gameweek has finished. Only then does it
refit the model, rerun 10,000 simulations and republish.

If nothing has finished, it writes nothing. Most days it does nothing.
""")

ui.callout(
    "scope", "Where the site currently stands.",
    f"It is showing the state after gameweek {state['last_published_gameweek']} of 2026-27, "
    f"with a model trained on {state['training_rows']:,} matches. The job decides a gameweek "
    "is complete when the club that has played fewest matches has played another one, which "
    "handles postponements gracefully: a delayed match holds the count back for everybody "
    "until it is played.",
)

# ---------------------------------------------------------------------------
st.markdown("### The pressure score, in detail")

st.markdown(
    "Most analysis of big matches picks them by hand, which invites bias. This project scores "
    "every match from 0 to 1 instead, from four ingredients."
)

c1, c2 = st.columns([3, 2])
with c1:
    gws = np.arange(1, 39)
    curve = 1 / (1 + np.exp(-config.GW_SIGMOID_K * (gws - config.GW_SIGMOID_MID)))
    fig = go.Figure()
    fig.add_trace(go.Scatter(
        x=gws, y=curve / config.GW38_MAX, mode="lines", name="time pressure",
        line=dict(color=theme.COLOR["brand"], width=3),
        hovertemplate="<b>Gameweek %{x}</b><br>Time pressure factor %{y:.2f}<extra></extra>",
    ))
    fig.add_vline(x=config.GW_SIGMOID_MID,
                  line=dict(color=theme.COLOR["annotation"], width=1, dash="dash"),
                  annotation_text=f"midpoint, GW{config.GW_SIGMOID_MID}",
                  annotation_font=dict(color=theme.COLOR["annotation"], size=11))
    theme.apply(fig, height=330, legend=False,
                xaxis=dict(title=dict(text="Gameweek")),
                yaxis=dict(title=dict(text="Time pressure factor")))
    ui.bare_chart(fig, title="How much a match matters purely because of when it is")
with c2:
    st.markdown("""
**1. When in the season.** An S-curve centred on gameweek 22. A tight race in August barely
registers; the same gap in April is decisive.

**2. How close the prize is.** Distance to the title, a Champions League place, a Europa place,
or safety from relegation, measured against how many points are still available.

**3. Recent form.** A team on a bad run is under more pressure, worth a 15% uplift.

**4. Is it a derby.** A flat bonus for a rivalry fixture.
""")

st.markdown(
    "The four competition boundaries are weighted differently (title 1.0, Champions League "
    "0.75, Europa 0.5, relegation 0.8) and the highest one wins, so a mid-table club fighting "
    "for seventh still registers real pressure even with no title chance."
)

ui.callout(
    "definition", "Two versions of the flag exist, deliberately.",
    "One uses a fixed threshold and only "
    "ever looks at a single match, which makes it safe to use for live predictions. The other "
    "takes the top quarter of each team-season, which gives every season a comparable slice of "
    "its own biggest matches but needs the whole season to compute, so it is only ever used "
    "for looking backwards. Mixing them up would leak the future into a prediction, so the "
    "unmarked name is the safe one and the retrospective version has to be asked for by name."
)

# ---------------------------------------------------------------------------
st.markdown("### What stops the model cheating")

st.markdown("""
Leakage is when a model is accidentally shown something it would not know at prediction time.
It produces impressive results that collapse in the real world. Four things guard against it here.

1. **Rolling features are shifted before averaging.** A five-match average for match 20 covers
   matches 15 to 19, never match 20 itself.
2. **Form never crosses a season boundary.** Rolling windows are grouped by team and season, so
   May's form does not bleed into August.
3. **Train/test splits are always forward in time.** Train on earlier seasons, test on a later
   one. Random shuffling is never used anywhere in this project.
4. **The backtest explicitly hides the future.** When replaying a completed season from a
   chosen gameweek, every later result is masked out before any feature is computed, including
   head-to-head records.
""")

ui.callout(
    "caveat", "One documented exception, stated rather than hidden.",
    "The historical pressure score is computed from a league table that includes the match "
    "being scored. The simulation correctly uses the table before the match. Recomputing "
    "history the strict way changes the model's log loss by about 0.001, which is why it was "
    "left alone, but it is a genuine inconsistency and is recorded as one.",
)

# ---------------------------------------------------------------------------
st.markdown("### The features the model uses")

glossary_show = glossary[["feature", "description"]].copy()
glossary_show.insert(0, "label", glossary_show["feature"].map(theme.feature_label))
ui.table(glossary_show, {
    "label": st.column_config.TextColumn("Feature", pinned=True, width="medium"),
    "feature": st.column_config.TextColumn("Name in the code", width="medium"),
    "description": st.column_config.TextColumn("What it means", width="large"),
})

# ---------------------------------------------------------------------------
st.markdown("### How to read the colours")

st.markdown("""
Two colour families run through the site and they never mix inside one chart.

**Judgements** use green for better than the baseline and orange for worse. That pair is
readable with the common forms of colour blindness, which red and green are not, and neither
end of it collides with Arsenal's red.

**Clubs** use red for Arsenal, sky blue for Manchester City, teal for Liverpool and amber for
Manchester United. These are chosen so four clubs can appear on one chart and stay apart, not
to match kits: Arsenal, Liverpool and Manchester United all play in red, so real club colours
would make those charts unreadable. Arsenal keeps red because it is the subject of the project.
""")

st.markdown("### Glossary")

c1, c2 = st.columns(2)
with c1:
    st.markdown("""
**Expected goals (xG)** — How many goals the chances a team created were worth, based on how
often similar chances have been scored historically. A tap-in is worth far more than a shot
from thirty yards.

**xGA** — The same thing for chances a team allowed its opponent to have.

**xGD** — Expected goal difference, xG minus xGA. The single best one-number summary of how
well a team is playing.

**Points per match (PPG)** — Points divided by matches. Roughly 2.0 is title-winning pace.

**The bottle gap** — This project's own term: points per match in a team's highest-pressure
matches, minus its points per match across that whole season.
""")
with c2:
    st.markdown("""
**p-value** — The chance of seeing a pattern this strong if nothing real were there. Under
0.05 is the usual bar.

**Multiple-comparison correction** — Running many tests means some clear that bar by luck.
Correction raises the bar to account for how many were run.

**Cohen's d** — The size of a difference, in standard deviations. Roughly 0.2 small, 0.5
medium, 0.8 large.

**Log loss** — How good a model's probabilities are, not just its guesses. Punishes confident
mistakes heavily. Lower is better.

**Monte Carlo simulation** — Play out the remaining season thousands of times with random
results drawn from the model's probabilities, then count how often each outcome happened.
""")

# ---------------------------------------------------------------------------
st.markdown("### Verification")

st.markdown("""
Every figure on this site is recomputed from the raw data by the same code that runs the live
pipeline. Nothing is typed in from a previous run. Two automated check suites run whenever the
data is rebuilt and compare the fresh numbers against the values recorded in the project's own
documentation.

The statistical checks cover every p-value, the cross-team test, the effect-size analysis and
the named individual matches. The model checks cover the holdout log loss and accuracy, the
no-information baseline, the feature ablation, the training row count and both backtests
including which specific clubs fell outside their predicted range.

Building this site found two errors in the earlier analysis. One was a headline statistical
finding that turned out to be an artifact of how tied results were sorted, documented in full
on the **Is it real?** page. The other was a set of individual match statistics that had been
written down from an early draft and never rechecked against what the pipeline actually
produced.
""")

ui.callout(
    "scope", "Source and data.",
    "The code, including the notebooks this site is built from, is at "
    "[github.com/ChTej00/Arsenal-Bottle](https://github.com/ChTej00/Arsenal-Bottle). "
    "Data from [Understat](https://understat.com).",
    icon=":material/code:",
)

with st.expander("Distribution checks behind the statistical tests",
                 icon=":material/functions:"):
    ui.table(normality, {
        "variable": st.column_config.TextColumn("Variable", width="medium"),
        "shapiro_W": st.column_config.NumberColumn("Shapiro-Wilk W", format="%.3f"),
        "p_value": st.column_config.NumberColumn("p-value", format="%.2e"),
        "normal_at_05": st.column_config.CheckboxColumn("Normal?"),
        "n": st.column_config.NumberColumn("n"),
    })
    st.markdown(
        "Points can only be 0, 1 or 3, so it is nowhere near a normal distribution and fails "
        "badly, exactly as expected. This is why every parametric test on this site is paired "
        "with a non-parametric equivalent that assumes nothing about distribution shape, and "
        "why the cross-team comparison uses Kruskal-Wallis rather than the ANOVA originally "
        "planned."
    )

ui.prev_next("pages/method.py")
