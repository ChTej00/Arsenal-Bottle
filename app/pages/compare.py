"""Is the bottle gap an Arsenal thing, or does every big club have one?"""
import plotly.graph_objects as go
import streamlit as st
from plotly.subplots import make_subplots

from lib import loaders, theme, ui
from src import config

ui.page_header(
    "Is the bottle an Arsenal thing?",
    "Calling something a bottle only means anything if other clubs do not do it. This page "
    "runs the same pressure analysis across the four clubs that defined this era, and the "
    "answer is not flattering to the narrative.",
    eyebrow="Team comparator",
)

pressure = loaders.table("team_pressure.csv")
pri = loaders.table("bootstrap_ci.csv")
tier = loaders.table("opponent_tier.csv")
tests = loaders.table("stakes_tests.csv")
TEAMS = config.TITLE_TEAMS

ui.callout(
    "scope", "These four clubs only.",
    "Arsenal, Liverpool, Manchester City and Manchester United are the ones with full "
    "match-level histories collected for this project. The other sixteen Premier League clubs "
    "appear in the live predictor, which uses league-wide data, but they were never analysed "
    "at this depth, so they are honestly not in this comparison.",
)

# ---------------------------------------------------------------------------
st.subheader("Every team's bottle gap, season by season", anchor=False)

fig = make_subplots(rows=2, cols=2, subplot_titles=TEAMS,
                    vertical_spacing=0.17, horizontal_spacing=0.07)
for i, team in enumerate(TEAMS):
    t = pressure[pressure["team"] == team].sort_values("season")
    fig.add_trace(go.Bar(
        x=[theme.season_label(s) for s in t["season"]], y=t["ppg_gap"],
        marker_color=[theme.judgement_color(v) for v in t["ppg_gap"]],
        text=[f"{v:+.2f}" for v in t["ppg_gap"]], textposition="outside",
        cliponaxis=False, textfont=dict(color=theme.COLOR["muted"], size=10),
        showlegend=False,
        hovertemplate=f"<b>{team} · %{{x}}</b><br>%{{y:+.2f}} points per match"
                      "<extra></extra>",
    ), row=i // 2 + 1, col=i % 2 + 1)
theme.apply(fig, height=500, legend=False, margin=dict(l=4, r=4, t=30, b=4))
theme.style_subplots(fig, legend_below=False)
fig.update_yaxes(zerolinecolor=theme.COLOR["muted"], range=[-0.85, 0.75])
ui.chart(
    fig,
    title="Every club has negative seasons. Arsenal's are just the famous ones.",
    verdict_text=(
        "<strong>Every one of these clubs has negative seasons.</strong> What is specific to "
        "Arsenal is not that it has them, but that its two negative seasons land exactly on "
        "the two everyone already calls bottles. For the other three, the good and bad "
        "pressure seasons do not map nearly so neatly onto their own reputations."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Who actually handles pressure best?", anchor=False)

ui.metric_grid([
    (f"{pri.loc[pri['team'] == t, 'pressure_resilience_index'].iloc[0]:+.2f}", t,
     "Average points-per-match gap in big matches, across contention seasons",
     f"95% range {pri.loc[pri['team'] == t, 'ci_low'].iloc[0]:+.2f} to "
     f"{pri.loc[pri['team'] == t, 'ci_high'].iloc[0]:+.2f}")
    for t in pri.sort_values("pressure_resilience_index", ascending=False)["team"]
])

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
fig.add_vline(x=0, line=dict(color=theme.COLOR["muted"], width=1, dash="dash"))
theme.apply(fig, height=280, legend=False,
            xaxis=dict(title=dict(text="Average points-per-match gap in big matches")))
ui.chart(
    fig,
    title="Every range crosses zero, so none of them can be ranked",
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
            line=dict(color=theme.COLOR["negative"] if worse else theme.COLOR["positive"],
                      width=3),
            opacity=0.5, showlegend=False, hoverinfo="skip",
        ), row=1, col=col)
        fig.add_trace(go.Scatter(
            x=[r["normal_mean"]], y=[team], mode="markers", name="Normal matches",
            marker=dict(color=theme.COLOR["faint"], size=11, line=dict(width=0)),
            showlegend=(col == 1 and i == 0), legendgroup="normal",
            hovertemplate=f"<b>{team}, normal matches</b>"
                          "<br>%{x:.2f} expected goals<extra></extra>",
        ), row=1, col=col)
        fig.add_trace(go.Scatter(
            x=[r["high_stakes_mean"]], y=[team], mode="markers", name="Big matches",
            marker=dict(color=theme.team_color(team), size=13, line=dict(width=0)),
            showlegend=(col == 1 and i == 0), legendgroup="high",
            hovertemplate=(f"<b>{team}, big matches</b><br>%{{x:.2f}} expected goals"
                           f"<br>p = {r['p_welch']:.3f}<extra></extra>"),
        ), row=1, col=col)
theme.apply(fig, height=390)
theme.style_subplots(fig)
fig.update_xaxes(title=dict(text="Expected goals per match",
                            font=dict(color=theme.COLOR["muted"], size=11)))

ars_xga = tests[(tests["team"] == "Arsenal") & (tests["metric"] == "xGA")].iloc[0]
utd_xga = tests[(tests["team"] == "Manchester United") & (tests["metric"] == "xGA")].iloc[0]
ui.chart(
    fig,
    title="Arsenal's pattern repeats at Manchester United, and nowhere else",
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
    ui.table(
        show[["team", "Measure", "high_stakes_mean", "normal_mean", "difference",
              "cohens_d", "p_welch", "p_bonferroni"]],
        {
            "team": st.column_config.TextColumn("Team", pinned=True),
            "high_stakes_mean": st.column_config.NumberColumn("Big matches", format="%.2f"),
            "normal_mean": st.column_config.NumberColumn("Normal", format="%.2f"),
            "difference": st.column_config.NumberColumn("Difference", format="%+.2f"),
            "cohens_d": st.column_config.NumberColumn(
                "Effect size", format="%+.2f",
                help="Cohen's d. 0.2 small, 0.5 medium, 0.8 large"),
            "p_welch": st.column_config.NumberColumn("p, on its own", format="%.3f"),
            "p_bonferroni": st.column_config.NumberColumn("p, corrected", format="%.3f"),
        },
    )

# ---------------------------------------------------------------------------
st.subheader("Big-game bottlers, or flat-track bullies?", anchor=False)

fig = go.Figure()
for tier_name, color in [("Big 6", theme.NAVY), ("Rest", theme.COLOR["annotation"])]:
    t = tier[tier["opponent_tier"] == tier_name].set_index("team").reindex(TEAMS)
    fig.add_trace(go.Bar(
        name=f"against {tier_name}", x=TEAMS, y=t["ppg_gap"], marker_color=color,
        text=[f"{v:+.2f}" for v in t["ppg_gap"]], textposition="outside",
        cliponaxis=False, textfont=dict(color=theme.COLOR["text_primary"], size=11),
        hovertemplate="<b>%{x} against " + tier_name
                      + "</b><br>%{y:+.2f} points per match<extra></extra>",
    ))
fig.add_hline(y=0, line=dict(color=theme.COLOR["muted"], width=1))
theme.apply(fig, height=390, barmode="group",
            yaxis=dict(title=dict(text="Points-per-match gap under pressure"),
                       range=[-0.46, 0.46]))
ui.chart(
    fig,
    title="The big-game drop belongs to Liverpool and United, not Arsenal",
    verdict_text=(
        "Nobody here is a flat-track bully: against weaker opponents every club is flat or "
        "better under pressure. The split shows up against the strongest opponents, and it "
        "belongs to <strong>Liverpool and Manchester United, not Arsenal</strong>. Arsenal and "
        "Manchester City are slightly better against the Big 6 in high-pressure matches than "
        "in ordinary ones."
    ),
)

ui.callout(
    "caveat", "The buckets are the same size but not the same intensity.",
    "Manchester United's high-pressure matches in its decline seasons are not the same kind "
    "of match as Arsenal's in a title race. Its top quarter by pressure score in 2024-25 "
    "averages 0.364 out of 1, against 0.6 to 0.95 for the others.",
)

ui.prev_next("pages/compare.py")
