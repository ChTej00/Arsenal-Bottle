"""Act 1: what Arteta inherited, and how the underlying numbers changed."""
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "What Arteta inherited",
    "Mikel Arteta took over a mid-table Arsenal in December 2019. Before asking why they "
    "fell short of titles, it is worth establishing how far the team actually travelled, "
    "using the underlying numbers rather than the league table.",
    eyebrow="Act 1 · The Rise",
)
ui.stepper("pages/rise.py")
st.write("")

era = loaders.table("era_comparison.csv")
progress = loaders.table("team_season_progress.csv")
season_tbl = loaders.table("season_table.csv")

ars_prog = progress[progress["team"] == "Arsenal"].sort_values("season").copy()
ars_prog["label"] = ars_prog["season"].map(theme.season_label)
ars_prog["xGD"] = ars_prog["xG"] - ars_prog["xGA"]
ars_tbl = season_tbl[season_tbl["team"] == "Arsenal"].sort_values("season").copy()
ars_tbl["label"] = ars_tbl["season"].map(theme.season_label)

ui.callout(
    "definition", "Expected goals (xG)",
    "is the single idea this whole project rests on. Every shot is scored by how likely a "
    "chance like it is to be scored, based on thousands of past shots in the same situation. "
    "Add them up and you get how many goals a team *should* have scored from the chances it "
    "created. **xGA** is the same thing for chances a team allowed its opponent. It is a "
    "better measure of how well a team played than the actual score, because one lucky "
    "deflection does not move it.",
)

# ---------------------------------------------------------------------------
st.subheader("The starting point", anchor=False)

metrics = [("xG", "Chances created"), ("xGA", "Chances conceded"), ("ppg", "Points per match")]
fig = go.Figure()
for idx, row in era.iterrows():
    fig.add_trace(go.Bar(
        name=row["era"],
        x=[m[1] for m in metrics],
        y=[row[m[0]] for m in metrics],
        marker_color=theme.COLOR["faint"] if idx == 0 else theme.COLOR["brand"],
        text=[f"{row[m[0]]:.2f}" for m in metrics],
        textposition="outside", cliponaxis=False,
        textfont=dict(color=theme.COLOR["text_primary"], size=12),
        hovertemplate="<b>%{x}</b><br>%{y:.2f} per match<extra>" + row["era"] + "</extra>",
    ))
theme.apply(fig, height=360, barmode="group",
            yaxis=dict(title=dict(text="Per match"), range=[0, 2.5]))
ui.chart(
    fig,
    title="The attack barely changed. The defence transformed.",
    verdict_text=(
        "The obvious expectation is that Arteta made Arsenal better at attacking. He did not, "
        f"at least not on average: chances created are essentially unchanged, "
        f"{era.iloc[0]['xG']:.2f} per match before against {era.iloc[1]['xG']:.2f} after. "
        f"<strong>The entire pooled improvement is defensive</strong>, from "
        f"{era.iloc[0]['xGA']:.2f} chances conceded per match down to {era.iloc[1]['xGA']:.2f}. "
        "That is worth sitting with, because a defensive rebuild is exactly the kind of change "
        "a league table describes badly."
    ),
    method_text=(
        "The pre-Arteta baseline is Arsenal's 76 Premier League matches across 2017-18 and "
        "2018-19 under Arsène Wenger and Unai Emery. The Arteta era is all 266 matches from "
        "2019-20 through 2025-26. Both are simple means across every match in the period.\n\n"
        "Averaging the whole Arteta era into one bar does hide the trajectory, which is why "
        "the next chart breaks it out season by season. The pooled figure includes 2019-20, "
        "when the team was still worse than what he inherited."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("It got worse before it got better", anchor=False)

pre_xgd = float(era.iloc[0]["xGD"])
fig = go.Figure(go.Bar(
    x=ars_prog["label"], y=ars_prog["xGD"],
    marker_color=[theme.COLOR["negative"] if v < pre_xgd else theme.COLOR["brand"]
                  for v in ars_prog["xGD"]],
    text=[f"{v:+.2f}" for v in ars_prog["xGD"]], textposition="outside",
    cliponaxis=False, textfont=dict(color=theme.COLOR["text_primary"], size=12),
    hovertemplate="<b>%{x}</b><br>Expected goal difference %{y:+.2f} per match<extra></extra>",
))
fig.add_hline(y=pre_xgd, line=dict(color=theme.COLOR["annotation"], width=1.5, dash="dot"),
              annotation_text="what Arteta inherited", annotation_position="bottom right",
              annotation_font=dict(color=theme.COLOR["annotation"], size=11))
theme.apply(fig, height=380, legend=False,
            yaxis=dict(title=dict(text="Expected goals for minus against, per match"),
                       range=[-0.45, 1.7]))
worst = ars_prog.loc[ars_prog["xGD"].idxmin()]
peak = ars_prog.loc[ars_prog["xGD"].idxmax()]
ui.chart(
    fig,
    title="His first full season was worse than the team he took over",
    verdict_text=(
        f"Arteta's first full season, <strong>{worst['label']}</strong>, was worse than the "
        f"team he took over: {worst['xGD']:+.2f} against a pre-Arteta {pre_xgd:+.2f}. It then "
        f"climbs every season to a peak of <strong>{peak['xGD']:+.2f}</strong> in "
        f"{peak['label']}. Judging the rebuild on its first year would have got it badly wrong, "
        "which is the case for looking at underlying numbers over several seasons rather than "
        "results over a few months."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Chance creation and prevention, season by season", anchor=False)

fig = go.Figure()
fig.add_trace(go.Scatter(
    x=ars_prog["label"], y=ars_prog["xG"], name="Created (xG)", mode="lines+markers",
    line=dict(color=theme.COLOR["brand"], width=2.5), marker=dict(size=7),
    hovertemplate="<b>%{x}</b><br>Created %{y:.2f} per match<extra></extra>",
))
fig.add_trace(go.Scatter(
    x=ars_prog["label"], y=ars_prog["xGA"], name="Conceded (xGA)", mode="lines+markers",
    line=dict(color=theme.CLUB["Manchester City"], width=2.5), marker=dict(size=7),
    fill="tonexty", fillcolor=theme.BRAND_FILL,
    hovertemplate="<b>%{x}</b><br>Conceded %{y:.2f} per match<extra></extra>",
))
theme.apply(fig, height=380, yaxis=dict(title=dict(text="Expected goals per match")))
best = ars_prog.loc[ars_prog["xGD"].idxmax()]
ui.chart(
    fig,
    title="The gap between the two widens every season",
    verdict_text=(
        "The shaded gap is the team's underlying quality: how much better their chances were "
        f"than their opponents'. It widens steadily and is at its largest in "
        f"<strong>{best['label']}</strong>, where Arsenal created {best['xG']:.2f} and conceded "
        f"{best['xGA']:.2f} per match. The rebuild shows up in the process well before it "
        "showed up in trophies."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Results followed", anchor=False)

fig = go.Figure()
fig.add_trace(go.Bar(
    x=ars_prog["label"], y=ars_prog["ppg"], name="Points per match",
    marker_color=theme.COLOR["brand"], opacity=0.85,
    hovertemplate="<b>%{x}</b><br>%{y:.2f} points per match<extra></extra>",
))
fig.add_trace(go.Scatter(
    x=ars_prog["label"], y=ars_prog["win_rate"] * 100, name="Win rate (%)",
    mode="lines+markers", yaxis="y2",
    line=dict(color=theme.COLOR["annotation"], width=2.5), marker=dict(size=7),
    hovertemplate="<b>%{x}</b><br>%{y:.0f}% of matches won<extra></extra>",
))
theme.apply(fig, height=380,
            yaxis=dict(title=dict(text="Points per match"), range=[0, 2.6]),
            yaxis2=dict(title=dict(text="Win rate (%)",
                                   font=dict(color=theme.COLOR["muted"])),
                        overlaying="y", side="right", showgrid=False, range=[20, 85],
                        tickfont=dict(color=theme.COLOR["muted"])))
ui.chart(
    fig,
    title="Championship pace, reached repeatedly before it was converted",
    verdict_text=(
        f"From {ars_prog.iloc[0]['ppg']:.2f} points a match in 2019-20 to "
        f"{ars_prog.iloc[-1]['ppg']:.2f} in the title-winning season. A points-per-match figure "
        "above roughly 2.0 is championship pace, and Arsenal reached it repeatedly before "
        "finally converting it into a trophy."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Every Arsenal season at a glance", anchor=False)

show = ars_tbl[["label", "points", "wins", "draws", "losses", "goals_for",
                "goals_against", "xG", "xGA", "ppg"]].rename(columns={"label": "Season"})
ui.table(show, {
    "Season": st.column_config.TextColumn("Season", pinned=True),
    "points": st.column_config.ProgressColumn(
        "Points", min_value=0, max_value=100, format="%d", color=theme.COLOR["brand"]),
    "wins": st.column_config.NumberColumn("W", width="small"),
    "draws": st.column_config.NumberColumn("D", width="small"),
    "losses": st.column_config.NumberColumn("L", width="small"),
    "goals_for": st.column_config.NumberColumn("Goals for", width="small"),
    "goals_against": st.column_config.NumberColumn("Goals against", width="small"),
    "xG": st.column_config.NumberColumn("Chances created", format="%.2f"),
    "xGA": st.column_config.NumberColumn("Chances conceded", format="%.2f"),
    "ppg": st.column_config.NumberColumn("Points/match", format="%.2f"),
})

ui.prev_next("pages/rise.py")
