"""Is the bottle gap an Arsenal thing, or does every big club have one?"""
import plotly.graph_objects as go
import streamlit as st
from plotly.subplots import make_subplots

from lib import loaders, theme, ui
from src import config

ui.page_header(
    "Team comparator",
    "Calling something a bottle only means anything if other clubs do not do it. This page "
    "runs the same pressure analysis across the four clubs that defined this era, and the "
    "answer is not flattering to the narrative.",
)

matches = loaders.table("matches.csv")
pressure = loaders.table("team_pressure.csv")
pri = loaders.table("bootstrap_ci.csv")
tier = loaders.table("opponent_tier.csv")
TEAMS = config.TITLE_TEAMS

ui.note(
    "Scope: these four clubs only. Arsenal, Liverpool, Manchester City and Manchester United "
    "are the ones with full match-level histories collected for this project. The other "
    "sixteen Premier League clubs appear in the live predictor, which uses league-wide data, "
    "but they were never analysed at this depth, so they are honestly not in this comparison."
)

# ---------------------------------------------------------------------------
st.markdown("### Every team's bottle gap, season by season")

fig = make_subplots(rows=2, cols=2, subplot_titles=TEAMS, vertical_spacing=0.16,
                    horizontal_spacing=0.08)
for i, team in enumerate(TEAMS):
    t = pressure[pressure["team"] == team].sort_values("season")
    fig.add_trace(go.Bar(
        x=[theme.season_label(s) for s in t["season"]], y=t["ppg_gap"],
        marker_color=[theme.POSITIVE if v > 0 else theme.NEGATIVE for v in t["ppg_gap"]],
        showlegend=False,
        hovertemplate=f"{team} %{{x}}<br>%{{y:+.3f}} points per match<extra></extra>",
    ), row=i // 2 + 1, col=i % 2 + 1)
fig.update_annotations(font=dict(size=13, color=theme.TEXT))
theme.apply(fig, height=520, legend=False,
            title="Points per match in big games, minus each season's own average")
fig.update_yaxes(gridcolor=theme.LINE, zerolinecolor=theme.MUTED,
                 tickfont=dict(color=theme.MUTED, size=11))
fig.update_xaxes(gridcolor=theme.LINE, tickfont=dict(color=theme.MUTED, size=11))
ui.chart(fig, verdict_text=(
    "<b>Every one of these clubs has negative seasons.</b> What is specific to Arsenal is not "
    "that it has them, but that its two negative seasons land exactly on the two everyone "
    "already calls bottles. For the other three, the good and bad pressure seasons do not map "
    "nearly so neatly onto their own reputations."
))

# ---------------------------------------------------------------------------
st.markdown("### Who actually handles pressure best?")

pri_sorted = pri.sort_values("pressure_resilience_index")
fig = go.Figure()
for _, r in pri_sorted.iterrows():
    fig.add_trace(go.Scatter(
        x=[r["ci_low"], r["ci_high"]], y=[r["team"], r["team"]], mode="lines",
        line=dict(color=theme.team_color(r["team"]), width=5), opacity=0.45,
        showlegend=False, hoverinfo="skip",
    ))
    fig.add_trace(go.Scatter(
        x=[r["pressure_resilience_index"]], y=[r["team"]], mode="markers",
        marker=dict(color=theme.team_color(r["team"]), size=13, line=dict(width=0)),
        showlegend=False,
        hovertemplate=(f"{r['team']}<br>%{{x:+.3f}} points per match"
                       f"<br>95% range {r['ci_low']:+.3f} to {r['ci_high']:+.3f}<extra></extra>"),
    ))
fig.add_vline(x=0, line=dict(color=theme.MUTED, width=1, dash="dash"))
theme.apply(fig, height=330, legend=False,
            title="Pressure resilience, with 95% uncertainty range",
            xaxis_title="Average points-per-match gap in big matches")
ui.chart(fig, verdict_text=(
    "The dot is each club's average, the bar is how uncertain that average is. "
    "<b>Liverpool, not Arsenal, comes out worst.</b> Manchester City is the only one above "
    "zero. But look at the bars: every single range crosses zero and they all overlap each "
    "other heavily. On this evidence you cannot actually rank these four clubs at all."
), method_text=(
    "Only seasons where a club was genuinely in contention count, defined as at least five "
    "matches clearing the absolute pressure threshold that season. The uncertainty range comes "
    "from bootstrapping: resample that club's contention seasons 10,000 times with "
    "replacement, and take the middle 95% of the resulting averages."
))

# ---------------------------------------------------------------------------
st.markdown("### Does the defensive softening happen to everyone?")

fig = make_subplots(rows=2, cols=4, vertical_spacing=0.18, horizontal_spacing=0.05,
                    subplot_titles=[f"{t}" for t in TEAMS] + [""] * 4,
                    row_titles=["Chances created", "Chances conceded"])
tests = loaders.table("stakes_tests.csv")
for col, team in enumerate(TEAMS, start=1):
    t = matches[matches["team"] == team]
    for row, metric in enumerate(["xG", "xGA"], start=1):
        for stake, side, color in [("Normal", "negative", theme.FAINT),
                                   ("High Stakes", "positive", theme.team_color(team))]:
            sub = t[t["stake_label"] == stake]
            fig.add_trace(go.Violin(
                y=sub[metric], name=stake, side=side, legendgroup=stake,
                showlegend=(col == 1 and row == 1),
                line_color=color, fillcolor=color, opacity=0.55,
                points=False, width=1.4, x0=metric,
                hovertemplate=f"{team} {metric} ({stake})<br>%{{y:.2f}}<extra></extra>",
            ), row=row, col=col)
fig.update_annotations(font=dict(size=12, color=theme.TEXT))
theme.apply(fig, height=520, violinmode="overlay",
            title="Chances created and conceded, big matches against normal")
fig.update_yaxes(gridcolor=theme.LINE, tickfont=dict(color=theme.MUTED, size=10))
fig.update_xaxes(showticklabels=False)
ars_xga = tests[(tests["team"] == "Arsenal") & (tests["metric"] == "xGA")].iloc[0]
utd_xga = tests[(tests["team"] == "Manchester United") & (tests["metric"] == "xGA")].iloc[0]
ui.chart(fig, verdict_text=(
    "Arsenal's pattern, flat attack and slightly leakier defence under pressure, is echoed "
    f"almost exactly by Manchester United (chances conceded p={utd_xga['p_welch']:.3f} against "
    f"Arsenal's p={ars_xga['p_welch']:.3f}). Manchester City and Liverpool show neither effect. "
    "<b>So it is not an Arsenal trait, but nor is it universal.</b> Two of four, on samples "
    "this small, is not far from what chance alone would produce."
))

# ---------------------------------------------------------------------------
st.markdown("### Big-game bottlers, or flat-track bullies?")

fig = go.Figure()
for tier_name, color in [("Big 6", theme.NAVY), ("Rest", theme.GOLD)]:
    t = tier[tier["opponent_tier"] == tier_name].set_index("team").reindex(TEAMS)
    fig.add_trace(go.Bar(
        name=f"against {tier_name}", x=TEAMS, y=t["ppg_gap"], marker_color=color,
        text=[f"{v:+.2f}" for v in t["ppg_gap"]], textposition="outside",
        textfont=dict(color=theme.TEXT, size=11),
        hovertemplate="%{x} vs " + tier_name + "<br>%{y:+.3f} points per match<extra></extra>",
    ))
fig.add_hline(y=0, line=dict(color=theme.MUTED, width=1))
theme.apply(fig, height=400, barmode="group",
            title="Pressure gap, split by quality of opponent",
            yaxis_title="Points-per-match gap under pressure")
ui.chart(fig, verdict_text=(
    "Nobody here is a flat-track bully: against weaker opponents every club is flat or better "
    "under pressure. The split shows up against the strongest opponents, and it belongs to "
    "<b>Liverpool and Manchester United, not Arsenal</b>. Arsenal and Manchester City are "
    "slightly better against the Big 6 in high-pressure matches than in ordinary ones."
))

ui.note(
    "A caveat worth stating plainly: Manchester United's 'high pressure' matches in its decline "
    "seasons are not the same kind of match as Arsenal's in a title race. Its top quarter by "
    "pressure score in 2024-25 averages 0.364 out of 1, against 0.6 to 0.95 for the others. "
    "The buckets are the same size but not the same intensity."
)

st.page_link("pages/significance.py", label="Next: none of this has been tested yet",
             icon=":material/arrow_forward:")
