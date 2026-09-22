"""Act 3: what actually changed in the season Arsenal finally won it."""
import numpy as np
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "Act 3 · The Breakthrough",
    "Arsenal won the 2025-26 Premier League. The interesting question is not that they won "
    "but what was different, because on the underlying numbers 2023-24 was arguably a better "
    "team and finished second.",
)

matches = loaders.table("matches.csv")
pressure = loaders.table("team_pressure.csv")
season_tbl = loaders.table("season_table.csv")

ars = matches[matches["team"] == "Arsenal"].copy()
CONTENDING = ["2223", "2324", "2425", "2526"]

tbl = season_tbl[(season_tbl["team"] == "Arsenal") & season_tbl["season"].isin(CONTENDING)].copy()
prs = pressure[(pressure["team"] == "Arsenal") & pressure["season"].isin(CONTENDING)].copy()
merged = tbl.merge(prs[["season", "ppg_gap"]], on="season")
merged["label"] = merged["season"].map(theme.season_label)

champion = merged[merged["season"] == "2526"].iloc[0]
best_process = merged.loc[(merged["xG"] - merged["xGA"]).idxmax()]

# ---------------------------------------------------------------------------
st.subheader("The four title challenges, compared", anchor=False)

c1, c2, c3, c4 = st.columns(4)
c1.metric("Best underlying season", best_process["label"], border=True,
          help="Largest gap between chances created and conceded")
c2.metric("Where it finished", "2nd", border=True,
          delta=f"{int(best_process['points'])} points", delta_arrow="off")
c3.metric("Title-winning season", champion["label"], border=True)
c4.metric("Its bottle gap", f"{champion['ppg_gap']:+.2f}", border=True,
          delta="positive", help="Points per match in big games, minus its own season average")

show = merged[["label", "points", "xG", "xGA", "ppg", "run_in_points",
               "high_stakes_ppg", "ppg_gap"]]
st.dataframe(
    show, width="stretch", hide_index=True,
    column_config={
        "label": st.column_config.TextColumn("Season", pinned=True),
        "points": st.column_config.ProgressColumn(
            "Final points", min_value=0, max_value=95, format="%d", color=theme.RED),
        "xG": st.column_config.NumberColumn("Chances created", format="%.2f"),
        "xGA": st.column_config.NumberColumn("Chances conceded", format="%.2f"),
        "ppg": st.column_config.NumberColumn("Points/match", format="%.2f"),
        "run_in_points": st.column_config.NumberColumn(
            "Run-in points", format="%d of 30",
            help="Points taken from the final ten matches"),
        "high_stakes_ppg": st.column_config.NumberColumn(
            "Points/match, big games", format="%.2f"),
        "ppg_gap": st.column_config.NumberColumn(
            "Bottle gap", format="%+.3f",
            help="Big-game points per match minus that season's own average"),
    },
)
ui.verdict(
    f"<strong>{best_process['label']} had the best underlying numbers of the four</strong> "
    f"({best_process['xG']:.2f} created against {best_process['xGA']:.2f} conceded) and "
    f"finished second. {champion['label']} won the league with slightly worse process but a "
    f"positive bottle gap of {champion['ppg_gap']:+.3f}. The difference between the seasons "
    "Arsenal lost and the one they won is not that they played better football. It is that the "
    "big matches stopped costing them."
)

# ---------------------------------------------------------------------------
st.subheader("Did the football fall apart in the run-in?", anchor=False)

fig = go.Figure()
for season in CONTENDING:
    s = ars[ars["season"] == season].sort_values("gameweek")
    is_champ = season == "2526"
    fig.add_trace(go.Scatter(
        x=s["gameweek"], y=s["xG"].rolling(5, min_periods=3).mean(),
        name=theme.season_label(season), mode="lines",
        line=dict(width=3 if is_champ else 2,
                  color=theme.RED if is_champ else None,
                  dash="solid" if is_champ else "dot"),
        hovertemplate="%{fullData.name} GW%{x}<br>%{y:.2f} created (5-match average)<extra></extra>",
    ))
fig.add_vrect(x0=29, x1=38, fillcolor=theme.MUTED, opacity=0.07, line_width=0,
              annotation_text="run-in", annotation_position="top left",
              annotation_font=dict(color=theme.MUTED, size=11))
theme.apply(fig, height=390, xaxis=dict(title=dict(text="Gameweek")),
            yaxis=dict(title=dict(text="Expected goals per match")))

runin = {s: ars[(ars["season"] == s) & (ars["gameweek"] >= 29)]["xG"].mean() for s in CONTENDING}
full = {s: ars[ars["season"] == s]["xG"].mean() for s in CONTENDING}
worst_s = min(CONTENDING, key=lambda s: runin[s] - full[s])
ui.chart(
    fig,
    title="Chances created, rolling 5-match average",
    verdict_text=(
        "In three of the four seasons the shaded run-in tracks the rest of the year closely: "
        "the team kept creating chances right to the end. <strong>"
        f"{theme.season_label(worst_s)} is the exception</strong>, with {runin[worst_s]:.2f} "
        f"expected goals per match in the run-in against {full[worst_s]:.2f} across the full "
        f"season, a drop of about {abs(runin[worst_s]/full[worst_s] - 1)*100:.0f}%. That "
        "season's collapse was not purely about converting chances, the attack genuinely "
        "dimmed too."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Where the title was actually lost", anchor=False)

fig = go.Figure()
for season in CONTENDING:
    s = ars[ars["season"] == season].sort_values("gameweek")
    is_champ = season == "2526"
    fig.add_trace(go.Scatter(
        x=s["gameweek"], y=s["points"].cumsum(), name=theme.season_label(season),
        mode="lines", line=dict(width=3 if is_champ else 2,
                                color=theme.RED if is_champ else None,
                                dash="solid" if is_champ else "dot"),
        hovertemplate="%{fullData.name} GW%{x}<br>%{y:.0f} points<extra></extra>",
    ))
theme.apply(fig, height=390, xaxis=dict(title=dict(text="Gameweek")),
            yaxis=dict(title=dict(text="Points")))
ui.chart(
    fig,
    title="Cumulative points through the season",
    verdict_text=(
        "All four seasons are close together for most of the year. They separate late. The "
        "title-winning line pulls clear precisely in the stretch where the two bottle seasons "
        "flatten out, and a flat stretch on this chart is a run of draws, which is exactly how "
        "2024-25 ended."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Results against performance, every season", anchor=False)

allp = season_tbl[season_tbl["team"] == "Arsenal"].copy()
allp["label"] = allp["season"].map(theme.season_label)
allp["xGD"] = allp["xG"] - allp["xGA"]

fig = go.Figure()
z = np.polyfit(allp["xGD"], allp["points"], 1)
xs = np.linspace(allp["xGD"].min() - 0.05, allp["xGD"].max() + 0.05, 20)
fig.add_trace(go.Scatter(x=xs, y=np.polyval(z, xs), mode="lines",
                         line=dict(color=theme.FAINT, width=1, dash="dash"),
                         hoverinfo="skip", showlegend=False))
fig.add_trace(go.Scatter(
    x=allp["xGD"], y=allp["points"], mode="markers+text",
    text=allp["label"], textposition="top center",
    textfont=dict(color=theme.MUTED, size=11),
    marker=dict(size=14, color=[theme.RED if s in CONTENDING else theme.FAINT
                                for s in allp["season"]], line=dict(width=0)),
    hovertemplate="%{text}<br>xGD %{x:+.2f}<br>%{y:.0f} points<extra></extra>",
    showlegend=False,
))
theme.apply(fig, height=400, legend=False,
            xaxis=dict(title=dict(text="Expected goal difference per match")),
            yaxis=dict(title=dict(text="Final points")))
ui.chart(
    fig,
    title="Underlying performance against actual points",
    verdict_text=(
        "A season above the dashed line collected more points than its performance justified, "
        "below it fewer. 2023-24 sits below the line, the season Arsenal played best and got "
        "least for it. 2025-26 sits above. <strong>The gap between those two seasons is almost "
        "entirely luck and conversion, not quality</strong>, which is the most honest summary "
        "of this whole story."
    ),
)

st.page_link("pages/significance.py",
             label="Next: does any of this survive a proper statistical test?",
             icon=":material/arrow_forward:")
