"""Act 2: the three near misses, and where the points actually went."""
import numpy as np
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "Act 2 · The Bottle",
    "Arsenal led the league for long stretches in 2022-23, 2023-24 and 2024-25 and won none "
    "of them. The accusation is that they buckled when it mattered. This page tests that "
    "against the match data, and the first answer it gives is not the expected one.",
)

matches = loaders.table("matches.csv")
pressure = loaders.table("team_pressure.csv")
killers = loaders.table("killer_matches.csv")

ars = matches[matches["team"] == "Arsenal"].copy()
ars["label"] = ars["season"].map(theme.season_label)
ars_pressure = pressure[pressure["team"] == "Arsenal"].sort_values("season").copy()
ars_pressure["label"] = ars_pressure["season"].map(theme.season_label)

ui.note(
    "**What counts as a big match?** Rather than picking them by hand, every match gets a "
    "pressure score from 0 to 1 built from how close the team is to a title, Champions League "
    "or relegation place, how late in the season it is, recent form, and whether it is a "
    "derby. The top quarter of each season by that score is what this page calls high-stakes: "
    "ten matches a season, every season, so the comparison is fair across years.",
    icon=":material/school:",
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


c1, c2 = st.columns(2, gap="medium")
with c1:
    st.markdown("##### Performance")
    fig = go.Figure(go.Heatmap(
        z=grid("match_drop_index"), x=gws, y=labels, colorscale="RdBu_r", zmid=0,
        colorbar=dict(thickness=9, tickfont=dict(color=theme.MUTED, size=10), len=0.9),
        hovertemplate="%{y} GW%{x}<br>Drop index %{z:+.2f}<extra></extra>",
    ))
    theme.apply(fig, height=300, legend=False, xaxis=dict(title=dict(text="Gameweek")))
    st.plotly_chart(fig, width="stretch")
with c2:
    st.markdown("##### Results")
    fig = go.Figure(go.Heatmap(
        z=grid("points"), x=gws, y=labels, colorscale="RdYlGn", zmin=0, zmax=3,
        colorbar=dict(thickness=9, tickfont=dict(color=theme.MUTED, size=10), len=0.9),
        hovertemplate="%{y} GW%{x}<br>%{z:.0f} points<extra></extra>",
    ))
    theme.apply(fig, height=300, legend=False, xaxis=dict(title=dict(text="Gameweek")))
    st.plotly_chart(fig, width="stretch")

ui.verdict(
    "Read the right-hand edge of both charts, the run-in from gameweek 29. On the left there "
    "is no pattern: performance in the closing months looks like performance in any other "
    "month. On the right, 2022-23 and 2024-25 turn red, meaning dropped points. <strong>The "
    "same quality of football stopped producing the same results.</strong> That gap is what "
    "this project calls the bottle gap."
)

# ---------------------------------------------------------------------------
st.subheader("The bottle gap, season by season", anchor=False)

fig = go.Figure(go.Bar(
    x=ars_pressure["label"], y=ars_pressure["ppg_gap"],
    marker_color=[theme.POSITIVE if v > 0 else theme.NEGATIVE for v in ars_pressure["ppg_gap"]],
    text=[f"{v:+.2f}" for v in ars_pressure["ppg_gap"]],
    textposition="outside", textfont=dict(color=theme.TEXT, size=12),
    hovertemplate="%{x}<br>%{y:+.3f} points per match<extra></extra>",
))
fig.add_hline(y=0, line=dict(color=theme.MUTED, width=1))
theme.apply(fig, height=370, legend=False,
            yaxis=dict(title=dict(text="Points per match, relative to normal"),
                       range=[-0.78, 0.42]))
ui.chart(
    fig,
    title="Points per match in big matches, minus that season's own average",
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
st.subheader("Attack held up. Defence did not, slightly.", anchor=False)

fig = go.Figure()
for metric, label in [("xG", "Chances created"), ("xGA", "Chances conceded")]:
    for stake, color, side in [("Normal", theme.FAINT, "negative"),
                               ("High Stakes", theme.RED, "positive")]:
        sub = ars[ars["stake_label"] == stake]
        fig.add_trace(go.Violin(
            x=[label] * len(sub), y=sub[metric], name=stake, side=side,
            legendgroup=stake, showlegend=(metric == "xG"),
            line=dict(color=color, width=1.5), fillcolor=color, opacity=0.5,
            points=False, width=0.85, meanline=dict(visible=True, color=theme.TEXT, width=1),
            hovertemplate=f"{label} ({stake})<br>%{{y:.2f}}<extra></extra>",
        ))
theme.apply(fig, height=400, violinmode="overlay",
            yaxis=dict(title=dict(text="Expected goals in a match"), range=[0, 5]))
ui.chart(
    fig,
    title="Chances created and conceded: big matches against normal",
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
st.dataframe(
    show, width="stretch", hide_index=True,
    column_config={
        "Season": st.column_config.TextColumn("Season", pinned=True),
        "gameweek": st.column_config.NumberColumn("GW", width="small"),
        "opponent": st.column_config.TextColumn("Opponent"),
        "Score": st.column_config.TextColumn("Score", width="small"),
        "result": st.column_config.TextColumn("Result", width="small"),
        "xG": st.column_config.NumberColumn("Chances created", format="%.2f"),
        "xGA": st.column_config.NumberColumn("Chances conceded", format="%.2f"),
        "match_drop_index": st.column_config.NumberColumn(
            "Drop index", format="%+.2f",
            help="Positive means Arsenal played worse than their own season average"),
    },
)
n_neg = int((ars_kill["match_drop_index"] < 0).sum())
ui.verdict(
    f"The {len(ars_kill)} run-in matches across the four title challenges where Arsenal "
    f"dropped points from a high-pressure game. Only <strong>{n_neg} of them</strong> are "
    "matches where Arsenal actually played above their own standard and still failed to win. "
    f"The other {len(ars_kill) - n_neg} are games they underperformed in, and several are "
    "driven by an unusually high chances-conceded number in that specific match. The 2-2 with "
    "Liverpool in 2022-23 allowed 4.64 expected goals, more than double a normal match."
)
ui.note(
    "This table corrects an earlier version of this analysis, which claimed most of these "
    "matches were games Arsenal dominated and failed to convert. Recomputing from the actual "
    "pipeline showed the opposite sign for most of them.",
    icon=":material/history:",
)

st.page_link("pages/breakthrough.py", label="Next: what changed in the season they won",
             icon=":material/arrow_forward:")
