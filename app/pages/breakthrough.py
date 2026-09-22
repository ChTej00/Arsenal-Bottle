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

# ---------------------------------------------------------------------------
st.markdown("### The four title challenges, compared")

show = merged[["label", "points", "xG", "xGA", "ppg", "run_in_points",
               "high_stakes_ppg", "ppg_gap"]].rename(columns={
    "label": "Season", "points": "Final points", "xG": "Chances created",
    "xGA": "Chances conceded", "ppg": "Points/match", "run_in_points": "Run-in points (of 30)",
    "high_stakes_ppg": "Points/match, big games", "ppg_gap": "Bottle gap",
})
st.dataframe(
    show.style.format({"Chances created": "{:.2f}", "Chances conceded": "{:.2f}",
                       "Points/match": "{:.2f}", "Points/match, big games": "{:.2f}",
                       "Bottle gap": "{:+.3f}"})
        .background_gradient(subset=["Bottle gap"], cmap="RdYlGn")
        .background_gradient(subset=["Final points"], cmap="Greys"),
    width="stretch", hide_index=True,
)
best_process = merged.loc[(merged["xG"] - merged["xGA"]).idxmax()]
champion = merged[merged["season"] == "2526"].iloc[0]
ui.verdict(
    f"<b>{best_process['label']} had the best underlying numbers of the four</b> "
    f"({best_process['xG']:.2f} created against {best_process['xGA']:.2f} conceded) and finished "
    f"second. {champion['label']} won the league with slightly worse process but a positive "
    f"bottle gap of {champion['ppg_gap']:+.3f}. The difference between the seasons Arsenal lost "
    "and the one they won is not that they played better football. It is that the big matches "
    "stopped costing them."
)

# ---------------------------------------------------------------------------
st.markdown("### Did the football fall apart in the run-in?")

fig = go.Figure()
for season in CONTENDING:
    s = ars[ars["season"] == season].sort_values("gameweek")
    roll = s["xG"].rolling(5, min_periods=3).mean()
    is_champ = season == "2526"
    fig.add_trace(go.Scatter(
        x=s["gameweek"], y=roll, name=theme.season_label(season), mode="lines",
        line=dict(width=3 if is_champ else 2,
                  color=theme.RED if is_champ else None,
                  dash="solid" if is_champ else "dot"),
        hovertemplate="%{fullData.name} GW%{x}<br>%{y:.2f} xG (5-match average)<extra></extra>",
    ))
fig.add_vrect(x0=29, x1=38, fillcolor=theme.MUTED, opacity=0.08, line_width=0,
              annotation_text="run-in", annotation_position="top left",
              annotation_font=dict(color=theme.MUTED, size=11))
theme.apply(fig, height=400, title="Chances created, rolling 5-match average",
            xaxis_title="Gameweek", yaxis_title="Expected goals per match")

runin_avg = {s: ars[(ars["season"] == s) & (ars["gameweek"] >= 29)]["xG"].mean()
             for s in CONTENDING}
full_avg = {s: ars[ars["season"] == s]["xG"].mean() for s in CONTENDING}
worst_s = min(CONTENDING, key=lambda s: runin_avg[s] - full_avg[s])
ui.chart(fig, verdict_text=(
    "In three of the four seasons the shaded run-in tracks the rest of the year closely: the "
    f"team kept creating chances right to the end. <b>{theme.season_label(worst_s)} is the "
    f"exception</b>, with {runin_avg[worst_s]:.2f} expected goals per match in the run-in "
    f"against {full_avg[worst_s]:.2f} across the full season, a drop of about "
    f"{abs(runin_avg[worst_s]/full_avg[worst_s] - 1)*100:.0f}%. That season's collapse was not "
    "purely about converting chances, the attack genuinely dimmed too."
))

# ---------------------------------------------------------------------------
st.markdown("### Where the title was actually lost")

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
theme.apply(fig, height=400, title="Cumulative points through the season",
            xaxis_title="Gameweek", yaxis_title="Points")
ui.chart(fig, verdict_text=(
    "All four seasons are close together for most of the year. They separate late. The "
    "title-winning line pulls clear precisely in the stretch where the two bottle seasons "
    "flatten out, and a flat stretch on this chart is a run of draws, which is exactly how "
    "2024-25 ended."
))

# ---------------------------------------------------------------------------
st.markdown("### Results against performance, every season")

fig = go.Figure()
allp = season_tbl[season_tbl["team"] == "Arsenal"].copy()
allp["label"] = allp["season"].map(theme.season_label)
allp["xGD"] = allp["xG"] - allp["xGA"]
fig.add_trace(go.Scatter(
    x=allp["xGD"], y=allp["points"], mode="markers+text",
    text=allp["label"], textposition="top center",
    textfont=dict(color=theme.MUTED, size=11),
    marker=dict(size=14, color=[theme.RED if s in CONTENDING else theme.FAINT
                                for s in allp["season"]], line=dict(width=0)),
    hovertemplate="%{text}<br>xGD %{x:+.2f}<br>%{y:.0f} points<extra></extra>",
))
z = np.polyfit(allp["xGD"], allp["points"], 1)
xs = np.linspace(allp["xGD"].min(), allp["xGD"].max(), 20)
fig.add_trace(go.Scatter(x=xs, y=np.polyval(z, xs), mode="lines", name="trend",
                         line=dict(color=theme.MUTED, width=1, dash="dash"),
                         hoverinfo="skip"))
theme.apply(fig, height=400, legend=False,
            title="Underlying performance against actual points",
            xaxis_title="Expected goal difference per match",
            yaxis_title="Final points")
ui.chart(fig, verdict_text=(
    "A season above the dashed line collected more points than its performance justified, "
    "below it fewer. 2023-24 sits below the line, the season Arsenal played best and got least "
    "for it. 2025-26 sits above. <b>The gap between those two seasons is almost entirely luck "
    "and conversion, not quality</b>, which is the most honest summary of this whole story."
))

st.page_link("pages/significance.py",
             label="Next: does any of this survive a proper statistical test?",
             icon=":material/arrow_forward:")
