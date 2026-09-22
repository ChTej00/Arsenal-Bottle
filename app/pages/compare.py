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

pressure = loaders.table("team_pressure.csv")
pri = loaders.table("bootstrap_ci.csv")
tier = loaders.table("opponent_tier.csv")
tests = loaders.table("stakes_tests.csv")
TEAMS = config.TITLE_TEAMS

ui.note(
    "**Scope: these four clubs only.** Arsenal, Liverpool, Manchester City and Manchester "
    "United are the ones with full match-level histories collected for this project. The other "
    "sixteen Premier League clubs appear in the live predictor, which uses league-wide data, "
    "but they were never analysed at this depth, so they are honestly not in this comparison."
)

# ---------------------------------------------------------------------------
st.subheader("Every team's bottle gap, season by season", anchor=False)

fig = make_subplots(rows=2, cols=2, subplot_titles=TEAMS,
                    vertical_spacing=0.17, horizontal_spacing=0.07)
for i, team in enumerate(TEAMS):
    t = pressure[pressure["team"] == team].sort_values("season")
    fig.add_trace(go.Bar(
        x=[theme.season_label(s) for s in t["season"]], y=t["ppg_gap"],
        marker_color=[theme.POSITIVE if v > 0 else theme.NEGATIVE for v in t["ppg_gap"]],
        showlegend=False,
        hovertemplate=f"{team} %{{x}}<br>%{{y:+.3f}} points per match<extra></extra>",
    ), row=i // 2 + 1, col=i % 2 + 1)
theme.apply(fig, height=480, legend=False, margin=dict(l=4, r=4, t=30, b=4))
theme.style_subplots(fig, legend_below=False)
fig.update_yaxes(zerolinecolor=theme.MUTED, range=[-0.72, 0.62])
ui.chart(
    fig,
    title="Points per match in big games, minus each season's own average",
    verdict_text=(
        "<strong>Every one of these clubs has negative seasons.</strong> What is specific to "
        "Arsenal is not that it has them, but that its two negative seasons land exactly on "
        "the two everyone already calls bottles. For the other three, the good and bad "
        "pressure seasons do not map nearly so neatly onto their own reputations."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Who actually handles pressure best?", anchor=False)

cols = st.columns(4)
for col, team in zip(cols, pri.sort_values("pressure_resilience_index",
                                           ascending=False)["team"]):
    r = pri[pri["team"] == team].iloc[0]
    col.metric(team, f"{r['pressure_resilience_index']:+.3f}", border=True,
               delta=f"95% range {r['ci_low']:+.2f} to {r['ci_high']:+.2f}",
               delta_arrow="off",
               help="Average points-per-match gap in big matches, across contention seasons")

pri_sorted = pri.sort_values("pressure_resilience_index")
fig = go.Figure()
for _, r in pri_sorted.iterrows():
    fig.add_trace(go.Scatter(
        x=[r["ci_low"], r["ci_high"]], y=[r["team"], r["team"]], mode="lines",
        line=dict(color=theme.team_color(r["team"]), width=5), opacity=0.4,
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
theme.apply(fig, height=280, legend=False,
            xaxis=dict(title=dict(text="Average points-per-match gap in big matches")))
ui.chart(
    fig,
    title="Pressure resilience, with 95% uncertainty range",
    verdict_text=(
        "The dot is each club's average, the bar is how uncertain that average is. "
        "<strong>Liverpool, not Arsenal, comes out worst.</strong> Manchester City is the only "
        "one above zero. But look at the bars: every single range crosses zero and they all "
        "overlap each other heavily. On this evidence you cannot actually rank these four "
        "clubs at all."
    ),
    method_text=(
        "Only seasons where a club was genuinely in contention count, defined as at least five "
        "matches clearing the absolute pressure threshold that season. The uncertainty range "
        "comes from bootstrapping: resample that club's contention seasons 10,000 times with "
        "replacement, and take the middle 95% of the resulting averages."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Does the defensive softening happen to everyone?", anchor=False)

fig = make_subplots(rows=1, cols=2, horizontal_spacing=0.13,
                    subplot_titles=["Chances created", "Chances conceded"])
for col, metric in enumerate(["xG", "xGA"], start=1):
    m = tests[tests["metric"] == metric].set_index("team").reindex(TEAMS[::-1])
    for i, team in enumerate(m.index):
        r = m.loc[team]
        worse = (r["difference"] > 0) if metric == "xGA" else (r["difference"] < 0)
        fig.add_trace(go.Scatter(
            x=[r["normal_mean"], r["high_stakes_mean"]], y=[team, team], mode="lines",
            line=dict(color=theme.NEGATIVE if worse else theme.POSITIVE, width=3),
            opacity=0.5, showlegend=False, hoverinfo="skip",
        ), row=1, col=col)
        fig.add_trace(go.Scatter(
            x=[r["normal_mean"]], y=[team], mode="markers", name="Normal matches",
            marker=dict(color=theme.FAINT, size=11, line=dict(width=0)),
            showlegend=(col == 1 and i == 0), legendgroup="normal",
            hovertemplate=f"{team}, normal<br>%{{x:.2f}}<extra></extra>",
        ), row=1, col=col)
        fig.add_trace(go.Scatter(
            x=[r["high_stakes_mean"]], y=[team], mode="markers", name="Big matches",
            marker=dict(color=theme.team_color(team), size=13, line=dict(width=0)),
            showlegend=(col == 1 and i == 0), legendgroup="high",
            hovertemplate=(f"{team}, big matches<br>%{{x:.2f}}"
                           f"<br>p = {r['p_welch']:.3f}<extra></extra>"),
        ), row=1, col=col)
theme.apply(fig, height=390)
theme.style_subplots(fig)
fig.update_xaxes(title=dict(text="Expected goals per match",
                            font=dict(color=theme.MUTED, size=11)))

ars_xga = tests[(tests["team"] == "Arsenal") & (tests["metric"] == "xGA")].iloc[0]
utd_xga = tests[(tests["team"] == "Manchester United") & (tests["metric"] == "xGA")].iloc[0]
ui.chart(
    fig,
    title="Where each club's average moves under pressure",
    verdict_text=(
        "Each line runs from a club's normal-match average to its big-match average. Red means "
        "the move is the bad direction, green the good one. Arsenal's pattern, flat attack and "
        "a leakier defence under pressure, is echoed almost exactly by Manchester United "
        f"(chances conceded p={utd_xga['p_welch']:.3f} against Arsenal's "
        f"p={ars_xga['p_welch']:.3f}). Manchester City and Liverpool show neither effect. "
        "<strong>So it is not an Arsenal trait, but nor is it universal.</strong> Two of four, "
        "on samples this small, is not far from what chance alone would produce."
    ),
    method_text=(
        "Each dot is a simple mean across that club's matches in the relevant bucket, so the "
        "distance between the two dots is the raw effect. The p-value is a Welch's t-test "
        "comparing the two groups. The evidence page shows what happens to these p-values once "
        "they are corrected for the fact that eight of them were computed at once."
    ),
)

with st.expander("Full test results for all four clubs", icon=":material/table_chart:"):
    show = tests.copy()
    show["Measure"] = show["metric"].map({"xG": "Chances created", "xGA": "Chances conceded"})
    st.dataframe(
        show[["team", "Measure", "high_stakes_mean", "normal_mean", "difference",
              "cohens_d", "p_welch", "p_bonferroni"]],
        width="stretch", hide_index=True,
        column_config={
            "team": st.column_config.TextColumn("Team", pinned=True),
            "high_stakes_mean": st.column_config.NumberColumn("Big matches", format="%.3f"),
            "normal_mean": st.column_config.NumberColumn("Normal", format="%.3f"),
            "difference": st.column_config.NumberColumn("Difference", format="%+.3f"),
            "cohens_d": st.column_config.NumberColumn(
                "Effect size", format="%+.3f", help="Cohen's d. 0.2 small, 0.5 medium, 0.8 large"),
            "p_welch": st.column_config.NumberColumn("p, on its own", format="%.3f"),
            "p_bonferroni": st.column_config.NumberColumn("p, corrected", format="%.3f"),
        },
    )

# ---------------------------------------------------------------------------
st.subheader("Big-game bottlers, or flat-track bullies?", anchor=False)

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
theme.apply(fig, height=390, barmode="group",
            yaxis=dict(title=dict(text="Points-per-match gap under pressure"),
                       range=[-0.42, 0.42]))
ui.chart(
    fig,
    title="Pressure gap, split by quality of opponent",
    verdict_text=(
        "Nobody here is a flat-track bully: against weaker opponents every club is flat or "
        "better under pressure. The split shows up against the strongest opponents, and it "
        "belongs to <strong>Liverpool and Manchester United, not Arsenal</strong>. Arsenal and "
        "Manchester City are slightly better against the Big 6 in high-pressure matches than "
        "in ordinary ones."
    ),
)

ui.note(
    "A caveat worth stating plainly: Manchester United's high-pressure matches in its decline "
    "seasons are not the same kind of match as Arsenal's in a title race. Its top quarter by "
    "pressure score in 2024-25 averages 0.364 out of 1, against 0.6 to 0.95 for the others. "
    "The buckets are the same size but not the same intensity.",
    icon=":material/warning:", kind="warn",
)

st.page_link("pages/significance.py", label="Next: none of this has been tested yet",
             icon=":material/arrow_forward:")
