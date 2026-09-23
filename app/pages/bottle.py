"""Act 2: the three near misses, and where the points actually went."""
import numpy as np
import plotly.graph_objects as go
import streamlit as st
from plotly.subplots import make_subplots

from lib import explore, loaders, theme, ui

ui.page_header(
    "Where the points went",
    "Arsenal led the league for long stretches in 2022-23, 2023-24 and 2024-25 and won none "
    "of them. The accusation is that they buckled when it mattered. This page tests that "
    "against the match data, and the first answer it gives is not the expected one.",
    eyebrow="Act 2 · The Bottle",
)
ui.stepper("pages/bottle.py")
st.write("")

matches = loaders.table("matches.csv")
pressure = loaders.table("team_pressure.csv")
killers = loaders.table("killer_matches.csv")

ars = matches[matches["team"] == "Arsenal"].copy()
ars["label"] = ars["season"].map(theme.season_label)
ars_pressure = pressure[pressure["team"] == "Arsenal"].sort_values("season").copy()
ars_pressure["label"] = ars_pressure["season"].map(theme.season_label)

ui.callout(
    "definition", "What counts as a big match?",
    "Rather than picking them by hand, every match gets a pressure score from 0 to 1 built "
    "from how close the team is to a title, Champions League or relegation place, how late in "
    "the season it is, recent form, and whether it is a derby. The top quarter of each season "
    "by that score is what this page calls high-stakes: ten matches a season, every season, so "
    "the comparison is fair across years.",
)

# ---------------------------------------------------------------------------
st.subheader("Did Arsenal actually play worse in big matches?", anchor=False)

fig = go.Figure()
for stake, color, size in [("Normal", theme.FAINT, 6), ("High Stakes", theme.RED, 9)]:
    sub = ars[ars["stake_label"] == stake]
    fig.add_trace(go.Scatter(
        x=sub["date"], y=sub["match_drop_index"], mode="markers", name=stake,
        marker=dict(color=color, size=size, opacity=0.75 if stake == "Normal" else 0.95,
                    line=dict(width=0)),
        customdata=np.stack([sub["opponent"], sub["result"], sub["xG"], sub["xGA"]], axis=-1),
        hovertemplate=("%{customdata[0]}<br>%{customdata[1]} · created %{customdata[2]:.2f} "
                       "vs conceded %{customdata[3]:.2f}<br>Drop index %{y:+.2f}<extra></extra>"),
    ))
fig.add_hline(y=0, line=dict(color=theme.MUTED, width=1, dash="dash"))
theme.apply(fig, height=420,
            yaxis=dict(title=dict(text="Worse than usual  ↑     ↓  Better than usual")))
hs_di = ars[ars["stake_label"] == "High Stakes"]["match_drop_index"].mean()
nm_di = ars[ars["stake_label"] == "Normal"]["match_drop_index"].mean()
ui.chart(
    fig,
    title="Every Arsenal match under Arteta, by performance drop",
    verdict_text=(
        "Each dot is one match. Above the line, Arsenal created fewer chances or allowed more "
        "than their own average that season. Red dots are the big matches. They are scattered "
        f"through the same range as everything else: the average for big matches is "
        f"<strong>{hs_di:+.2f}</strong> against <strong>{nm_di:+.2f}</strong> for ordinary "
        "ones. There is no visible collapse in performance."
    ),
    method_text=(
        "The drop index for a match is `((season average xG − match xG) + (match xGA − season "
        "average xGA)) / 2`, so it rises when a team both created less and conceded more than "
        "it typically does that season. Positive means a worse performance than usual."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Performance against results, side by side", anchor=False)

seasons = sorted(ars["season"].unique())
labels = [theme.season_label(s) for s in seasons]
gws = list(range(1, 39))


def grid(value_col):
    return np.array([
        [ars.loc[(ars["season"] == s) & (ars["gameweek"] == g), value_col].mean()
         for g in gws] for s in seasons
    ])


# One figure, two panels, shared y axis and matched gameweek axes, because the
# reading below asks you to compare their right-hand edges. Colourbars are off:
# the two panels measure different things on different scales, so a shared bar
# would be meaningless and separate bars made the panels different widths.
fig = make_subplots(rows=1, cols=2, shared_yaxes=True, horizontal_spacing=0.04,
                    subplot_titles=["Performance (how well they played)",
                                    "Results (points won)"])
fig.add_trace(go.Heatmap(
    z=grid("match_drop_index"), x=gws, y=labels,
    colorscale=theme.SCALE_DIVERGING, reversescale=True, zmid=0, showscale=False,
    hovertemplate="<b>%{y} · gameweek %{x}</b><br>Drop index %{z:+.2f}<extra></extra>",
), row=1, col=1)
fig.add_trace(go.Heatmap(
    z=grid("points"), x=gws, y=labels,
    colorscale=theme.SCALE_DIVERGING, zmin=0, zmax=3, showscale=False,
    hovertemplate="<b>%{y} · gameweek %{x}</b><br>%{z:.0f} points<extra></extra>",
), row=1, col=2)
fig.add_vrect(x0=28.5, x1=38.5, row=1, col=1, line_width=0,
              fillcolor=theme.COLOR["text_primary"], opacity=0.05)
fig.add_vrect(x0=28.5, x1=38.5, row=1, col=2, line_width=0,
              fillcolor=theme.COLOR["text_primary"], opacity=0.05)
theme.apply(fig, height=330, legend=False)
theme.style_subplots(fig, legend_below=False)
fig.update_xaxes(title=dict(text="Gameweek", font=dict(color=theme.COLOR["muted"], size=11)),
                 range=[0.5, 38.5])
ui.chart(
    fig,
    title="The football held up through the run-in. The results did not.",
    verdict_text=(
        "Both panels use one colour scale: <strong>orange is worse, green is better</strong>. "
        "Read the shaded band on the right of each, the run-in from gameweek 29. On the left "
        "there is no pattern, performance in the closing months looks like performance in any "
        "other month. On the right, 2022-23 and 2024-25 turn orange, meaning dropped points. "
        "<strong>The same quality of football stopped producing the same results.</strong> "
        "That gap is what this project calls the bottle gap."
    ),
    method_text=(
        "The two panels measure different things on different scales, so they deliberately "
        "share no colourbar: the left is a drop index in expected-goal units, the right is "
        "points from 0 to 3. What they share is the direction of the scale, so orange means "
        "the same thing in both. The panels sit in one figure with a shared vertical axis so "
        "their gameweek axes line up exactly, which is what makes the comparison readable."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("The bottle gap, season by season", anchor=False)

fig = go.Figure(go.Bar(
    x=ars_pressure["label"], y=ars_pressure["ppg_gap"],
    marker_color=[theme.judgement_color(v) for v in ars_pressure["ppg_gap"]],
    text=[f"{v:+.2f}" for v in ars_pressure["ppg_gap"]],
    textposition="outside", cliponaxis=False,
    textfont=dict(color=theme.COLOR["text_primary"], size=12),
    hovertemplate="<b>%{x}</b><br>%{y:+.2f} points per match against its own average<extra></extra>",
))
fig.add_hline(y=0, line=dict(color=theme.COLOR["muted"], width=1))
theme.apply(fig, height=370, legend=False,
            yaxis=dict(title=dict(text="Points per match, relative to normal"),
                       range=[-0.82, 0.45]))
ui.chart(
    fig,
    title="The two negative seasons are the two everyone calls bottles",
    verdict_text=(
        "Below zero means the team took fewer points from its biggest matches than from a "
        "typical one that season. <strong>2022-23 and 2024-25, the two seasons everyone calls "
        "bottles, are the two that come out negative.</strong> 2023-24 and the title-winning "
        "2025-26 are positive. The metric agrees with the narrative, which is exactly why the "
        "next page goes on to ask whether it survives a significance test."
    ),
    method_text=(
        "Each season's own average points per match is the baseline, so a strong season is not "
        "penalised for having a high bar. The gap is the team's points per match across its "
        "ten highest-pressure matches minus that baseline. The comparison is always within a "
        "season, never across them."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Try to break it yourself", anchor=False)

st.markdown(
    "Everything above rests on one arbitrary choice: that a big match is the top quarter of "
    "a season by pressure score. Nothing makes 25% the right number. If the bottle is real it "
    "should survive a different cut. Change it and see."
)


@st.fragment
def threshold_control() -> None:
    """The one place this app computes rather than reads, so it refuses to draw
    anything unless the recomputation still reproduces the published figures at
    the published setting. See lib/explore.py."""
    drift = explore.drift_check(matches, pressure)
    if drift is not None:
        ui.callout(
            "correction", "This control is switched off.",
            f"It recomputes the bottle gap live, and a self-check found that {drift}. Rather "
            "than show you a number that disagrees with the rest of the site, it is hidden "
            "until the pipeline and this page agree again.",
        )
        return

    options = {"Top 15%": 0.15, "Top 20%": 0.20, "Top 25%": 0.25,
               "Top 30%": 0.30, "Top 40%": 0.40}
    choice = st.segmented_control(
        "What counts as a big match?", list(options), default="Top 25%",
        key="stakes_threshold",
    ) or "Top 25%"
    pct = options[choice]

    gaps = explore.verified_gaps(matches, pressure, pct)
    ars_gaps = gaps[gaps["team"] == "Arsenal"].sort_values("season").copy()
    ars_gaps["label"] = ars_gaps["season"].map(theme.season_label)
    n_per_season = int(ars_gaps["n_matches"].mode().iloc[0])

    published = ars_pressure.set_index("season")["ppg_gap"]
    fig = go.Figure()
    fig.add_trace(go.Bar(
        x=ars_gaps["label"], y=ars_gaps["ppg_gap"],
        marker_color=[theme.judgement_color(v) for v in ars_gaps["ppg_gap"]],
        text=[f"{v:+.2f}" for v in ars_gaps["ppg_gap"]],
        textposition="outside", cliponaxis=False,
        textfont=dict(color=theme.COLOR["text_primary"], size=12),
        name=choice,
        hovertemplate="<b>%{x}</b><br>%{y:+.2f} points per match<extra></extra>",
    ))
    if pct != explore.PUBLISHED_TOP_PCT:
        fig.add_trace(go.Scatter(
            x=ars_gaps["label"], y=published.reindex(ars_gaps["season"]).to_numpy(),
            mode="markers", name="Published setting (top 25%)",
            marker=dict(symbol="line-ew", size=30,
                        line=dict(color=theme.COLOR["muted"], width=2)),
            hovertemplate="<b>%{x}</b><br>%{y:+.2f} at the published setting<extra></extra>",
        ))
    fig.add_hline(y=0, line=dict(color=theme.COLOR["muted"], width=1))
    theme.apply(fig, height=380, legend=(pct != explore.PUBLISHED_TOP_PCT),
                yaxis=dict(title=dict(text="Points per match, relative to normal"),
                           range=[-0.95, 0.72]))

    bottles = ["2223", "2425"]
    still_negative = [s for s in bottles
                      if ars_gaps.loc[ars_gaps["season"] == s, "ppg_gap"].iloc[0] < 0]
    flipped = [theme.season_label(s) for s in bottles if s not in still_negative]

    if pct == explore.PUBLISHED_TOP_PCT:
        reading = ("This is the published setting, and it reproduces the chart above exactly. "
                   "Move it and watch what happens.")
    elif len(still_negative) == 2:
        reading = (f"At the top {int(pct*100)}%, both bottle seasons are still negative. "
                   "The finding holds at this cut.")
    elif still_negative:
        reading = (f"<strong>At the top {int(pct*100)}%, "
                   f"{' and '.join(flipped)} flips positive.</strong> One of the two seasons "
                   "the whole story rests on stops looking like a bottle purely because the "
                   "definition moved.")
    else:
        reading = (f"<strong>At the top {int(pct*100)}%, neither bottle season is negative "
                   "any more.</strong> The entire finding depends on where the line is drawn.")

    ui.chart(
        fig,
        title=f"Arsenal's bottle gap, big match = {choice.lower()} of the season "
              f"({n_per_season} matches)",
        verdict_text=reading,
        method_text=(
            "Changing the setting reruns the same calculation the pipeline uses: flag the top "
            "share of each team-season by pressure score, then take those matches' points per "
            "match minus that season's own average. At the top 25% this reproduces the "
            "published figures for all 28 team-seasons to within floating-point error, and "
            "the page checks that before it will draw anything. Only the definition of a big "
            "match changes, never the underlying match data."
        ),
    )


threshold_control()

ui.callout(
    "finding", "This is the honest reading of the whole page.",
    "2022-23 stays negative wherever you put the line. 2024-25 does not: it is positive at "
    "the top 15% and 20%, and only turns negative at 25% and wider. Half the evidence for "
    "the bottle is an artefact of a choice nobody had a principled reason to make. The next "
    "page tests all of this properly, and finds the same fragility.",
)

# ---------------------------------------------------------------------------
st.subheader("Attack held up. Defence did not, slightly.", anchor=False)

fig = go.Figure()
for metric, label in [("xG", "Chances created"), ("xGA", "Chances conceded")]:
    for stake, color, side in [("Normal", theme.COLOR["faint"], "negative"),
                               ("High Stakes", theme.COLOR["brand"], "positive")]:
        sub = ars[ars["stake_label"] == stake]
        fig.add_trace(go.Violin(
            x=[label] * len(sub), y=sub[metric], name=stake, side=side,
            legendgroup=stake, showlegend=(metric == "xG"),
            line=dict(color=color, width=1.5), fillcolor=color, opacity=0.5,
            points=False, width=0.85,
            meanline=dict(visible=True, color=theme.COLOR["text_primary"], width=1),
            hovertemplate=f"<b>{label}, {stake.lower()} matches</b>"
                          "<br>%{y:.2f} expected goals<extra></extra>",
        ))
theme.apply(fig, height=400, violinmode="overlay",
            yaxis=dict(title=dict(text="Expected goals in a match"), range=[0, 5]))
ui.chart(
    fig,
    title="Attack held up. Defence softened.",
    verdict_text=(
        "Each shape is the spread of match-by-match values, with the mean marked. Arsenal's "
        "chance creation is statistically indistinguishable between big matches and ordinary "
        "ones (1.84 against 1.78, p=0.665). Chances conceded are not: 1.32 against 1.05, which "
        "on its own would count as significant (p=0.031). <strong>So the attack holds, and the "
        "defence softens a little when it matters most.</strong> Hold that thought, because "
        "the evidence page shows this result does not survive being tested properly."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("The matches that actually cost them", anchor=False)

ars_kill = killers[(killers["team"] == "Arsenal") & killers["is_title_challenge"]].copy()
ars_kill["Season"] = ars_kill["season"].map(theme.season_label)
ars_kill["Score"] = (ars_kill["scored"].astype(int).astype(str) + "-"
                     + ars_kill["conceded"].astype(int).astype(str))
show = ars_kill[["Season", "gameweek", "opponent", "Score", "result",
                 "xG", "xGA", "match_drop_index"]]
ui.table(show, {
    "Season": st.column_config.TextColumn("Season", pinned=True),
    "gameweek": st.column_config.NumberColumn("GW", width="small"),
    "opponent": st.column_config.TextColumn("Opponent", width="medium"),
    "Score": st.column_config.TextColumn("Score", width="small"),
    "result": st.column_config.TextColumn("Result", width="small"),
    "xG": st.column_config.NumberColumn("Chances created", format="%.2f"),
    "xGA": st.column_config.NumberColumn("Chances conceded", format="%.2f"),
    "match_drop_index": st.column_config.NumberColumn(
        "Drop index", format="%+.2f",
        help="Positive means Arsenal played worse than their own season average"),
})
n_neg = int((ars_kill["match_drop_index"] < 0).sum())
ui.verdict(
    f"The {len(ars_kill)} run-in matches across the four title challenges where Arsenal "
    f"dropped points from a high-pressure game. Only <strong>{n_neg} of them</strong> are "
    "matches where Arsenal actually played above their own standard and still failed to win. "
    f"The other {len(ars_kill) - n_neg} are games they underperformed in, and several are "
    "driven by an unusually high chances-conceded number in that specific match. The 2-2 with "
    "Liverpool in 2022-23 allowed 4.64 expected goals, more than double a normal match."
)
ui.callout(
    "correction", "This table corrects an earlier version of this analysis,",
    "which claimed most of these matches were games Arsenal dominated and failed to convert. "
    "Recomputing from the actual pipeline showed the opposite sign for most of them.",
)

ui.prev_next("pages/bottle.py")
