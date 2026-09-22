"""Act 1: what Arteta inherited, and how the underlying numbers changed."""
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "Act 1 · The Rise",
    "Mikel Arteta took over a mid-table Arsenal in December 2019. Before asking why they "
    "fell short of titles, it is worth establishing how far the team actually travelled, "
    "using the underlying numbers rather than the league table.",
)

era = loaders.table("era_comparison.csv")
progress = loaders.table("team_season_progress.csv")
season_tbl = loaders.table("season_table.csv")

ars_prog = progress[progress["team"] == "Arsenal"].sort_values("season").copy()
ars_prog["label"] = ars_prog["season"].map(theme.season_label)
ars_tbl = season_tbl[season_tbl["team"] == "Arsenal"].sort_values("season").copy()
ars_tbl["label"] = ars_tbl["season"].map(theme.season_label)

ui.note(
    "<b>Expected goals (xG)</b> is the single idea this whole project rests on. Every shot is "
    "scored by how likely a chance like it is to be scored, based on thousands of past shots in "
    "the same situation. Add them up and you get how many goals a team <i>should</i> have scored "
    "from the chances it created. <b>xGA</b> is the same thing for chances a team allowed its "
    "opponent. It is a better measure of how well a team played than the actual score, because "
    "one lucky deflection does not move it."
)

# ---------------------------------------------------------------------------
st.markdown("### The starting point")

metrics = [("xG", "Chances created per match"), ("xGA", "Chances conceded per match"),
           ("ppg", "Points per match")]
fig = go.Figure()
for idx, row in era.iterrows():
    fig.add_trace(go.Bar(
        name=row["era"],
        x=[m[1] for m in metrics],
        y=[row[m[0]] for m in metrics],
        marker_color=theme.FAINT if idx == 0 else theme.RED,
        text=[f"{row[m[0]]:.2f}" for m in metrics],
        textposition="outside",
        textfont=dict(color=theme.TEXT, size=12),
        hovertemplate="%{x}<br>%{y:.2f}<extra>" + row["era"] + "</extra>",
    ))
theme.apply(fig, height=380, barmode="group",
            title="Pre-Arteta against the Arteta era",
            yaxis_title="Per match")
ui.chart(fig, verdict_text=(
    "The obvious expectation is that Arteta made Arsenal better at attacking. He did not, at "
    f"least not on average: chances created are essentially unchanged, {era.iloc[0]['xG']:.2f} "
    f"per match before against {era.iloc[1]['xG']:.2f} after. <b>The entire pooled improvement "
    f"is defensive</b>, from {era.iloc[0]['xGA']:.2f} chances conceded per match down to "
    f"{era.iloc[1]['xGA']:.2f}. That is worth sitting with, because a defensive rebuild is "
    "exactly the kind of change a league table describes badly."
), method_text=(
    "The pre-Arteta baseline is Arsenal's 76 Premier League matches across 2017-18 and 2018-19 "
    "under Arsène Wenger and Unai Emery. The Arteta era is all 266 matches from 2019-20 through "
    "2025-26. Both are simple means across every match in the period.\n\n"
    "Averaging the whole Arteta era into one bar does hide the trajectory, which is why the "
    "next chart breaks it out season by season. The pooled figure includes 2019-20, when the "
    "team was still worse than what he inherited."
))

# ---------------------------------------------------------------------------
st.markdown("### It got worse before it got better")

ars_prog["xGD"] = ars_prog["xG"] - ars_prog["xGA"]
pre_xgd = era.iloc[0]["xGD"]
fig = go.Figure()
fig.add_hrect(y0=pre_xgd - 0.001, y1=pre_xgd + 0.001, line_width=0,
              fillcolor=theme.MUTED, opacity=0.9)
fig.add_trace(go.Bar(
    x=ars_prog["label"], y=ars_prog["xGD"],
    marker_color=[theme.NEGATIVE if v < pre_xgd else theme.RED for v in ars_prog["xGD"]],
    text=[f"{v:+.2f}" for v in ars_prog["xGD"]], textposition="outside",
    textfont=dict(color=theme.TEXT, size=12),
    hovertemplate="%{x}<br>Expected goal difference %{y:+.2f}<extra></extra>",
))
fig.add_hline(y=pre_xgd, line=dict(color=theme.MUTED, width=1.5, dash="dot"),
              annotation_text="what Arteta inherited", annotation_position="top left",
              annotation_font=dict(color=theme.MUTED, size=11))
theme.apply(fig, height=390, legend=False,
            title="Expected goal difference per match, by season",
            yaxis_title="Chances created minus chances conceded")
worst = ars_prog.loc[ars_prog["xGD"].idxmin()]
peak = ars_prog.loc[ars_prog["xGD"].idxmax()]
ui.chart(fig, verdict_text=(
    f"Arteta's first full season, <b>{worst['label']}</b>, was worse than the team he took "
    f"over: {worst['xGD']:+.2f} against a pre-Arteta {pre_xgd:+.2f}. It then climbs every "
    f"season to a peak of <b>{peak['xGD']:+.2f}</b> in {peak['label']}. Judging the rebuild on "
    "its first year would have got it badly wrong, which is the case for looking at underlying "
    "numbers over several seasons rather than results over a few months."
))

# ---------------------------------------------------------------------------
st.markdown("### Chance creation and prevention, season by season")

fig = go.Figure()
fig.add_trace(go.Scatter(
    x=ars_prog["label"], y=ars_prog["xG"], name="Created (xG)", mode="lines+markers",
    line=dict(color=theme.RED, width=2.5), marker=dict(size=7),
    hovertemplate="%{x}<br>Created %{y:.2f}<extra></extra>",
))
fig.add_trace(go.Scatter(
    x=ars_prog["label"], y=ars_prog["xGA"], name="Conceded (xGA)", mode="lines+markers",
    line=dict(color=theme.SKY, width=2.5), marker=dict(size=7),
    fill="tonexty", fillcolor="rgba(239,1,7,0.10)",
    hovertemplate="%{x}<br>Conceded %{y:.2f}<extra></extra>",
))
theme.apply(fig, height=400, title="Arsenal expected goals for and against, per match",
            yaxis_title="Expected goals per match")
best = ars_prog.loc[(ars_prog["xG"] - ars_prog["xGA"]).idxmax()]
ui.chart(fig, verdict_text=(
    "The shaded gap is the team's underlying quality: how much better their chances were than "
    f"their opponents'. It widens steadily and is at its largest in <b>{best['label']}</b>, "
    f"where Arsenal created {best['xG']:.2f} and conceded {best['xGA']:.2f} per match. "
    "The rebuild shows up in the process well before it showed up in trophies."
))

# ---------------------------------------------------------------------------
st.markdown("### Results followed")

fig = go.Figure()
fig.add_trace(go.Bar(
    x=ars_prog["label"], y=ars_prog["ppg"], name="Points per match",
    marker_color=theme.RED, opacity=0.85,
    hovertemplate="%{x}<br>%{y:.2f} points per match<extra></extra>",
))
fig.add_trace(go.Scatter(
    x=ars_prog["label"], y=ars_prog["win_rate"] * 100, name="Win rate (%)",
    mode="lines+markers", yaxis="y2",
    line=dict(color=theme.GOLD, width=2.5), marker=dict(size=7),
    hovertemplate="%{x}<br>%{y:.0f}% of matches won<extra></extra>",
))
theme.apply(fig, height=400, title="Points per match and win rate",
            yaxis_title="Points per match",
            yaxis2=dict(title=dict(text="Win rate (%)", font=dict(color=theme.MUTED)),
                        overlaying="y", side="right", showgrid=False,
                        tickfont=dict(color=theme.MUTED)))
ui.chart(fig, verdict_text=(
    f"From {ars_prog.iloc[0]['ppg']:.2f} points a match in 2019-20 to "
    f"{ars_prog.iloc[-1]['ppg']:.2f} in the title-winning season. A points-per-match figure "
    "above roughly 2.0 is championship pace, and Arsenal reached it repeatedly before finally "
    "converting it into a trophy."
))

# ---------------------------------------------------------------------------
st.markdown("### Every Arsenal season at a glance")

show = ars_tbl[["label", "points", "wins", "draws", "losses", "goals_for",
                "goals_against", "xG", "xGA", "ppg"]].rename(columns={
    "label": "Season", "points": "Pts", "wins": "W", "draws": "D", "losses": "L",
    "goals_for": "GF", "goals_against": "GA", "xG": "xG/match",
    "xGA": "xGA/match", "ppg": "Pts/match",
})
st.dataframe(
    show.style.format({"xG/match": "{:.2f}", "xGA/match": "{:.2f}", "Pts/match": "{:.2f}"})
        .background_gradient(subset=["Pts"], cmap="Reds"),
    width="stretch", hide_index=True,
)

st.page_link("pages/bottle.py", label="Next: so why did they keep losing the title?",
             icon=":material/arrow_forward:")
