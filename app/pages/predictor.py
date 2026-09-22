"""Act 4: the live 2026-27 simulation, its track record, and a what-if tool."""
import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "Act 4 · The 2026-27 title race",
    "Everything before this page looks backwards. This one runs forwards: it plays out every "
    "remaining fixture of the season now being played, ten thousand times, and counts how "
    "often each club ends up on top.",
)

state = loaders.state()
summary = loaders.baseline_simulation()
final_pts, teams = loaders.baseline_points()
fixtures = loaders.table("fixtures.csv")
odds_hist = loaders.table("odds_history.csv")
bt20 = loaders.table("backtest_gw20.csv")
bt5 = loaders.table("backtest_gw5.csv")
convergence = loaders.table("convergence.csv")

gw = state["last_published_gameweek"]
ui.updated_banner(state)

if gw <= 8:
    ui.note(
        f"<b>Read this before the numbers.</b> Only {gw} gameweeks are played. The model reads "
        "form from each club's last five matches and that window resets every August, so it is "
        "currently working from five matches per club and projecting them across thirty-three. "
        "The backtest further down this page shows exactly how unreliable that is: at this "
        "stage last season the same method gave the eventual champion 29.7%, against 85.7% by "
        "the midpoint. These are the least trustworthy numbers on the site, and they are "
        "published anyway because hiding them would be worse.",
        kind="warn",
    )

# ---------------------------------------------------------------------------
st.markdown("### Who wins the league?")

top = summary.head(8).sort_values("title_prob")
fig = go.Figure(go.Bar(
    x=top["title_prob"] * 100, y=top.index, orientation="h",
    marker_color=[theme.team_color(t) for t in top.index],
    text=[f"{v*100:.1f}%" for v in top["title_prob"]], textposition="outside",
    textfont=dict(color=theme.TEXT, size=12),
    hovertemplate="%{y}<br>wins the league in %{x:.1f}% of simulations<extra></extra>",
))
theme.apply(fig, height=380, legend=False, title="Title probability, top 8",
            xaxis=dict(title=dict(text="Probability of finishing first (%)"),
                       range=[0, top["title_prob"].max() * 118]))
ui.chart(fig, verdict_text=(
    f"<b>{summary.index[0]}</b> finishes top in {summary.iloc[0]['title_prob']*100:.1f}% of "
    f"10,000 simulated seasons, {summary.index[1]} in "
    f"{summary.iloc[1]['title_prob']*100:.1f}%. These are frequencies, not opinions: every "
    "remaining fixture is given a win, draw and loss probability by the model, then a random "
    "number decides each one, and the table is added up at the end."
))

# ---------------------------------------------------------------------------
st.markdown("### How each club's season could end")

pick = st.selectbox("Show the full range of outcomes for", list(summary.index), index=0)
i = teams.index(pick)
pts = final_pts[:, i]
row = summary.loc[pick]

c1, c2, c3, c4 = st.columns(4)
c1.metric("Title", f"{row['title_prob']*100:.1f}%")
c2.metric("Top four", f"{row['top4_prob']*100:.1f}%")
c3.metric("Relegation", f"{row['releg_prob']*100:.1f}%")
c4.metric("Most likely points", f"{row['pts_median']:.0f}")

fig = go.Figure(go.Histogram(
    x=pts, nbinsx=40, marker_color=theme.team_color(pick), opacity=0.85,
    hovertemplate="%{x} points in %{y} simulations<extra></extra>",
))
for val, label, color in [(row["pts_p5"], "5th percentile", theme.MUTED),
                          (row["pts_median"], "median", theme.TEXT),
                          (row["pts_p95"], "95th percentile", theme.MUTED)]:
    fig.add_vline(x=val, line=dict(color=color, width=1.5, dash="dash"),
                  annotation_text=label, annotation_font=dict(color=color, size=10))
theme.apply(fig, height=340, legend=False,
            title=f"{pick}: final points across 10,000 simulated seasons",
            xaxis_title="Final points", yaxis_title="Number of simulations")
ui.chart(fig, verdict_text=(
    f"Not a single prediction but a distribution. {pick} finishes somewhere between "
    f"<b>{row['pts_p5']:.0f} and {row['pts_p95']:.0f} points</b> in 90% of simulations. The "
    "width of that range is the honest measure of how much is still undecided, and this early "
    "in a season it is very wide."
))

# ---------------------------------------------------------------------------
st.markdown("### The whole table")

by_median = summary.sort_values("pts_median")
fig = go.Figure()
for team, r in by_median.iterrows():
    fig.add_trace(go.Scatter(
        x=[r["pts_p5"], r["pts_p95"]], y=[team, team], mode="lines",
        line=dict(color=theme.team_color(team), width=7), opacity=0.4,
        showlegend=False, hoverinfo="skip",
    ))
    fig.add_trace(go.Scatter(
        x=[r["pts_median"]], y=[team], mode="markers",
        marker=dict(color=theme.team_color(team), size=10, line=dict(width=0)),
        showlegend=False,
        hovertemplate=(f"{team}<br>most likely %{{x:.0f}} points"
                       f"<br>90% range {r['pts_p5']:.0f} to {r['pts_p95']:.0f}<extra></extra>"),
    ))
theme.apply(fig, height=640, legend=False,
            title="Projected final points, dot is most likely, bar is the 90% range",
            xaxis_title="Final points")
ui.chart(fig, verdict_text=(
    "Every club's plausible finish. Where bars overlap heavily, the order between those clubs "
    "is genuinely undecided. Notice how much of the table is one indistinguishable block: "
    "outside the top two or three, this season could still finish almost any way round."
))

c1, c2 = st.columns(2)
with c1:
    st.markdown("**Most likely top four**")
    st.dataframe((summary["top4_prob"] * 100).round(1).sort_values(ascending=False).head(8)
                 .rename("Probability (%)"), width="stretch")
with c2:
    st.markdown("**Most at risk of relegation**")
    st.dataframe((summary["releg_prob"] * 100).round(1).sort_values(ascending=False).head(8)
                 .rename("Probability (%)"), width="stretch")

# ---------------------------------------------------------------------------
st.markdown("### Does this thing actually work?")

ui.note(
    "A forecast nobody has checked is worth nothing. Both charts below replay the completed "
    "2025-26 season with a model that was never shown it: freeze the table at a chosen "
    "gameweek, hide every later result, simulate forward, and compare against what really "
    "happened."
)

tab20, tab5 = st.tabs(["Frozen at gameweek 20 (midseason)", "Frozen at gameweek 5 (where we are now)"])
for tab, bt, cutoff in [(tab20, bt20, 20), (tab5, bt5, 5)]:
    with tab:
        b = bt.sort_values("pts_median")
        fig = go.Figure()
        for _, r in b.iterrows():
            fig.add_trace(go.Scatter(
                x=[r["pts_p5"], r["pts_p95"]], y=[r["team"], r["team"]], mode="lines",
                line=dict(color=theme.LINE, width=7), showlegend=False, hoverinfo="skip",
            ))
            inside = r["inside_range"]
            fig.add_trace(go.Scatter(
                x=[r["real_points"]], y=[r["team"]], mode="markers",
                marker=dict(color=theme.POSITIVE if inside else theme.NEGATIVE,
                            size=9, line=dict(width=0)),
                showlegend=False,
                hovertemplate=(f"{r['team']}<br>really got %{{x:.0f}}"
                               f"<br>predicted {r['pts_p5']:.0f} to {r['pts_p95']:.0f}"
                               "<extra></extra>"),
            ))
        theme.apply(fig, height=620, legend=False,
                    title=f"2025-26 actual points against the range predicted at gameweek {cutoff}",
                    xaxis_title="Final points")
        st.plotly_chart(fig, width="stretch")

        n_in = int(bt["inside_range"].sum())
        ars = bt[bt["team"] == "Arsenal"].iloc[0]
        champ = bt.loc[bt["real_points"].idxmax()]
        misses = bt.loc[~bt["inside_range"], "team"].tolist()
        ui.verdict(
            f"Green dots landed inside the predicted range, red missed. <b>{n_in} of 20</b> "
            f"clubs finished inside their 90% range (about 18 is what a well-calibrated 90% "
            f"range should produce). The real champion, {champ['team']}, was given "
            f"<b>{ars['title_prob']*100:.1f}%</b> at this point. "
            + ("Misses: " + ", ".join(misses) + "." if misses else "")
        )

ui.verdict(
    "The comparison between the two tabs is the most useful thing on this page. Given half a "
    "season of form the simulator is genuinely good. Given five matches it is weak, and it is "
    "weak in a specific way: clubs that started badly get written off, because their form "
    "window contains nothing else. <b>That is the single biggest known flaw in this system</b>, "
    "and fixing it, by carrying some of last season's strength into the early weeks, is the "
    "next piece of work on the list."
)

with st.expander("Is 10,000 simulations enough?"):
    fig = go.Figure()
    for team in convergence["team"].unique():
        t = convergence[convergence["team"] == team]
        fig.add_trace(go.Scatter(
            x=t["n_runs"], y=t["title_prob"] * 100, mode="lines+markers", name=team,
            line=dict(color=theme.team_color(team), width=2),
            hovertemplate="%{fullData.name}<br>%{x:,} runs: %{y:.1f}%<extra></extra>",
        ))
    theme.apply(fig, height=330, title="Title probability against number of simulations",
                xaxis=dict(title=dict(text="Simulations"), type="log"),
                yaxis_title="Title probability (%)")
    st.plotly_chart(fig, width="stretch")
    st.markdown(
        "Running more simulations reduces random noise in the answer, at the cost of time. "
        "The lines flatten well before 10,000, and the ordering never changes between 1,000 "
        "and 50,000, so 10,000 is comfortably enough. Each run takes a couple of seconds."
    )

# ---------------------------------------------------------------------------
st.markdown("### How the odds have moved")

if odds_hist["gameweek"].nunique() < 2:
    ui.note(
        "Only one gameweek has been published so far, so there is no line to draw yet. This "
        "chart fills in as the season progresses: one point per club per gameweek, added "
        "automatically each time a full gameweek completes."
    )
else:
    top_teams = summary.head(6).index.tolist()
    h = odds_hist[odds_hist["team"].isin(top_teams)]
    fig = go.Figure()
    for team in top_teams:
        t = h[h["team"] == team]
        fig.add_trace(go.Scatter(
            x=t["gameweek"], y=t["title_prob"] * 100, mode="lines+markers", name=team,
            line=dict(color=theme.team_color(team), width=2.5),
            hovertemplate="%{fullData.name}<br>GW%{x}: %{y:.1f}%<extra></extra>",
        ))
    theme.apply(fig, height=400, title="Title probability by gameweek",
                xaxis_title="Gameweek", yaxis_title="Title probability (%)")
    ui.chart(fig, verdict_text=(
        "Each point is a republished forecast after a completed gameweek. Steep moves mean a "
        "result changed the picture materially."
    ))

# ---------------------------------------------------------------------------
st.markdown("### Change a result and watch the table move")

st.markdown(
    "Pick up to three remaining fixtures, fix their results, and the whole season is simulated "
    "again from scratch around them."
)

fx = fixtures.sort_values(["gameweek", "date"]).copy()
fx["label"] = ("GW" + fx["gameweek"].astype(str) + ": " + fx["home_team"]
               + " vs " + fx["away_team"])

picked = st.multiselect("Fixtures", fx["label"].tolist(), max_selections=3)
overrides = []
if picked:
    cols = st.columns(len(picked))
    for col, label in zip(cols, picked):
        r = fx[fx["label"] == label].iloc[0]
        with col:
            choice = st.radio(label, [f"{r['home_team']} win", "Draw", f"{r['away_team']} win"],
                              key=f"wi_{r['game_id']}")
            outcome = {f"{r['home_team']} win": 2, "Draw": 1, f"{r['away_team']} win": 0}[choice]
            overrides.append((int(r["game_id"]), outcome))

if st.button("Resimulate", type="primary", disabled=not overrides):
    wi = loaders.whatif_simulation(tuple(sorted(overrides)))
    involved = set()
    for gid, _ in overrides:
        r = fx[fx["game_id"] == gid].iloc[0]
        involved |= {r["home_team"], r["away_team"]}
    show_teams = list(dict.fromkeys(list(summary.head(6).index) + sorted(involved)))

    comp = pd.DataFrame({
        "Before (%)": (summary.loc[show_teams, "title_prob"] * 100).round(1),
        "After (%)": (wi.loc[show_teams, "title_prob"] * 100).round(1),
    })
    comp["Change"] = (comp["After (%)"] - comp["Before (%)"]).round(1)
    comp = comp.sort_values("After (%)", ascending=False)
    st.dataframe(
        comp.style.format({"Change": "{:+.1f}"})
            .background_gradient(subset=["Change"], cmap="RdYlGn"),
        width="stretch",
    )
    ui.verdict(
        "This is a full resimulation, not an adjustment. All 10,000 seasons were replayed with "
        "those results locked in and everything else still random, so the knock-on effects are "
        "included: a fixed result changes the simulated table, which changes how much pressure "
        "every later match carries, which changes those probabilities too."
    )

ui.note(
    "<b>Known limitations, stated plainly.</b> The model predicts win, draw or loss and never a "
    "scoreline, so simulated ties on points are broken at random rather than on goal "
    "difference. Each club's form is frozen at today's value for the rest of the season. "
    "Newly promoted clubs have no Premier League history for the model to read. And the "
    "early-season weakness shown in the backtest above is real and currently unfixed."
)
