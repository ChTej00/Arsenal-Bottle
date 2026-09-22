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
        f"**Read this before the numbers.** Only {gw} gameweeks are played. The model reads "
        "form from each club's last five matches and that window resets every August, so it is "
        "currently working from five matches per club and projecting them across thirty-three. "
        "The backtest further down this page shows exactly how unreliable that is: at this "
        "stage last season the same method gave the eventual champion 29.7%, against 85.7% by "
        "the midpoint. These are the least trustworthy numbers on the site, and they are "
        "published anyway because hiding them would be worse.",
        kind="warn",
    )

# ---------------------------------------------------------------------------
st.subheader("Who wins the league?", anchor=False)

lead = summary.iloc[0]
c1, c2, c3, c4 = st.columns(4)
c1.metric("Favourite", summary.index[0], border=True)
c2.metric("Title probability", f"{lead['title_prob']*100:.1f}%", border=True)
c3.metric("Projected points", f"{lead['pts_median']:.0f}", border=True,
          delta=f"{lead['pts_p5']:.0f} to {lead['pts_p95']:.0f} range", delta_arrow="off")
c4.metric("Gameweeks left", f"{fixtures['gameweek'].nunique()}", border=True)

top = summary.head(8).sort_values("title_prob")
fig = go.Figure(go.Bar(
    x=top["title_prob"] * 100, y=top.index, orientation="h",
    marker=dict(color=[theme.team_color(t) for t in top.index], line=dict(width=0)),
    text=[f"{v*100:.1f}%" for v in top["title_prob"]], textposition="outside",
    textfont=dict(color=theme.TEXT, size=12),
    hovertemplate="%{y}<br>wins the league in %{x:.1f}% of simulations<extra></extra>",
))
theme.apply(fig, height=370, legend=False,
            xaxis=dict(title=dict(text="Probability of finishing first (%)"),
                       range=[0, float(top["title_prob"].max()) * 118]))
ui.chart(
    fig,
    title="Title probability, top 8",
    verdict_text=(
        f"<strong>{summary.index[0]}</strong> finishes top in "
        f"{summary.iloc[0]['title_prob']*100:.1f}% of 10,000 simulated seasons, "
        f"{summary.index[1]} in {summary.iloc[1]['title_prob']*100:.1f}%. These are "
        "frequencies, not opinions: every remaining fixture is given a win, draw and loss "
        "probability by the model, then a random number decides each one, and the table is "
        "added up at the end."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("How each club's season could end", anchor=False)

pick = st.selectbox("Show the full range of outcomes for", list(summary.index), index=0)
i = teams.index(pick)
pts = final_pts[:, i]
row = summary.loc[pick]

c1, c2, c3, c4 = st.columns(4)
c1.metric("Title", f"{row['title_prob']*100:.1f}%", border=True)
c2.metric("Top four", f"{row['top4_prob']*100:.1f}%", border=True)
c3.metric("Relegation", f"{row['releg_prob']*100:.1f}%", border=True)
c4.metric("Most likely points", f"{row['pts_median']:.0f}", border=True)

fig = go.Figure(go.Histogram(
    x=pts, nbinsx=40, marker_color=theme.team_color(pick), opacity=0.85,
    hovertemplate="%{x} points in %{y} simulations<extra></extra>",
))
for val, label in [(row["pts_p5"], "5th pct"), (row["pts_median"], "median"),
                   (row["pts_p95"], "95th pct")]:
    fig.add_vline(x=val, line=dict(color=theme.MUTED, width=1.5, dash="dash"),
                  annotation_text=label, annotation_font=dict(color=theme.MUTED, size=10))
theme.apply(fig, height=330, legend=False,
            xaxis=dict(title=dict(text="Final points")),
            yaxis=dict(title=dict(text="Number of simulations")))
ui.chart(
    fig,
    title=f"{pick}: final points across 10,000 simulated seasons",
    verdict_text=(
        f"Not a single prediction but a distribution. {pick} finishes somewhere between "
        f"<strong>{row['pts_p5']:.0f} and {row['pts_p95']:.0f} points</strong> in 90% of "
        "simulations. The width of that range is the honest measure of how much is still "
        "undecided, and this early in a season it is very wide."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("The whole table", anchor=False)

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
theme.apply(fig, height=620, legend=False,
            xaxis=dict(title=dict(text="Final points")))
ui.chart(
    fig,
    title="Projected final points: dot is most likely, bar is the 90% range",
    verdict_text=(
        "Every club's plausible finish. Where bars overlap heavily, the order between those "
        "clubs is genuinely undecided. Notice how much of the table is one indistinguishable "
        "block: outside the top two or three, this season could still finish almost any way "
        "round."
    ),
)

c1, c2 = st.columns(2, gap="medium")
with c1:
    st.markdown("##### Most likely top four")
    ui.prob_table(summary["top4_prob"].head(8), "Top-4 probability", theme.POSITIVE)
with c2:
    st.markdown("##### Most at risk of relegation")
    ui.prob_table(summary["releg_prob"].sort_values(ascending=False).head(8),
                  "Relegation probability", theme.NEGATIVE)

# ---------------------------------------------------------------------------
st.subheader("Does this thing actually work?", anchor=False)

ui.note(
    "A forecast nobody has checked is worth nothing. Both tabs below replay the completed "
    "2025-26 season with a model that was never shown it: freeze the table at a chosen "
    "gameweek, hide every later result, simulate forward, and compare against what really "
    "happened."
)

tab20, tab5 = st.tabs(["Frozen at gameweek 20 (midseason)",
                       "Frozen at gameweek 5 (where we are now)"])
for tab, bt, cutoff in [(tab20, bt20, 20), (tab5, bt5, 5)]:
    with tab:
        n_in = int(bt["inside_range"].sum())
        champ = bt.loc[bt["real_points"].idxmax()]
        c1, c2, c3 = st.columns(3)
        c1.metric("Clubs inside their range", f"{n_in} of 20", border=True,
                  help="A well-calibrated 90% range should catch about 18")
        c2.metric(f"{champ['team']} title probability",
                  f"{champ['title_prob']*100:.1f}%", border=True,
                  help="The club that actually won")
        c3.metric("Its projected points", f"{champ['pts_median']:.0f}", border=True,
                  delta=f"really got {champ['real_points']:.0f}", delta_arrow="off")

        b = bt.sort_values("pts_median")
        fig = go.Figure()
        for _, r in b.iterrows():
            fig.add_trace(go.Scatter(
                x=[r["pts_p5"], r["pts_p95"]], y=[r["team"], r["team"]], mode="lines",
                line=dict(color=theme.LINE, width=7), showlegend=False, hoverinfo="skip",
            ))
            fig.add_trace(go.Scatter(
                x=[r["real_points"]], y=[r["team"]], mode="markers",
                marker=dict(color=theme.POSITIVE if r["inside_range"] else theme.NEGATIVE,
                            size=9, line=dict(width=0)),
                showlegend=False,
                hovertemplate=(f"{r['team']}<br>really got %{{x:.0f}}"
                               f"<br>predicted {r['pts_p5']:.0f} to {r['pts_p95']:.0f}"
                               "<extra></extra>"),
            ))
        theme.apply(fig, height=600, legend=False,
                    xaxis=dict(title=dict(text="Final points")))
        st.plotly_chart(fig, width="stretch")

        misses = bt.loc[~bt["inside_range"], "team"].tolist()
        ui.verdict(
            f"Green dots landed inside the predicted range, red missed. <strong>{n_in} of "
            f"20</strong> clubs finished inside their 90% range. "
            + (f"Misses: {', '.join(misses)}." if misses else "")
        )

ui.verdict(
    "The comparison between the two tabs is the most useful thing on this page. Given half a "
    "season of form the simulator is genuinely good. Given five matches it is weak, and it is "
    "weak in a specific way: clubs that started badly get written off, because their form "
    "window contains nothing else. <strong>That is the single biggest known flaw in this "
    "system</strong>, and fixing it, by carrying some of last season's strength into the early "
    "weeks, is the next piece of work on the list."
)

with st.expander("Is 10,000 simulations enough?", icon=":material/functions:"):
    fig = go.Figure()
    for team in convergence["team"].unique():
        t = convergence[convergence["team"] == team]
        fig.add_trace(go.Scatter(
            x=t["n_runs"], y=t["title_prob"] * 100, mode="lines+markers", name=team,
            line=dict(color=theme.team_color(team), width=2),
            hovertemplate="%{fullData.name}<br>%{x:,} runs: %{y:.1f}%<extra></extra>",
        ))
    theme.apply(fig, height=320,
                xaxis=dict(title=dict(text="Simulations"), type="log"),
                yaxis=dict(title=dict(text="Title probability (%)")))
    st.plotly_chart(fig, width="stretch")
    st.markdown(
        "More simulations reduce random noise in the answer, at the cost of time. The lines "
        "flatten well before 10,000 and the ordering never changes between 1,000 and 50,000, "
        "so 10,000 is comfortably enough. Each run takes a couple of seconds."
    )

# ---------------------------------------------------------------------------
st.subheader("How the odds have moved", anchor=False)

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
    theme.apply(fig, height=390, xaxis=dict(title=dict(text="Gameweek")),
                yaxis=dict(title=dict(text="Title probability (%)")))
    ui.chart(fig, title="Title probability by gameweek", verdict_text=(
        "Each point is a republished forecast after a completed gameweek. Steep moves mean a "
        "result changed the picture materially."
    ))

# ---------------------------------------------------------------------------
st.subheader("Change a result and watch the table move", anchor=False)

st.markdown(
    "Pick up to three remaining fixtures, fix their results, and the whole season is simulated "
    "again from scratch around them."
)

fx = fixtures.sort_values(["gameweek", "date"]).copy()
fx["label"] = ("GW" + fx["gameweek"].astype(str) + ": " + fx["home_team"]
               + " vs " + fx["away_team"])

picked = st.multiselect("Fixtures", fx["label"].tolist(), max_selections=3,
                        placeholder="Choose up to three fixtures")
overrides = []
if picked:
    cols = st.columns(len(picked), gap="small")
    for col, label in zip(cols, picked):
        r = fx[fx["label"] == label].iloc[0]
        with col.container(border=True):
            st.markdown(f"**{label}**")
            choice = st.radio(
                "Result", [f"{r['home_team']} win", "Draw", f"{r['away_team']} win"],
                key=f"wi_{r['game_id']}", label_visibility="collapsed",
            )
            outcome = {f"{r['home_team']} win": 2, "Draw": 1, f"{r['away_team']} win": 0}[choice]
            overrides.append((int(r["game_id"]), outcome))

if st.button("Resimulate", type="primary", disabled=not overrides,
             icon=":material/replay:"):
    wi = loaders.whatif_simulation(tuple(sorted(overrides)))
    involved = set()
    for gid, _ in overrides:
        r = fx[fx["game_id"] == gid].iloc[0]
        involved |= {r["home_team"], r["away_team"]}
    show_teams = list(dict.fromkeys(list(summary.head(6).index) + sorted(involved)))

    comp = pd.DataFrame({
        "Before": (summary.loc[show_teams, "title_prob"] * 100).round(1),
        "After": (wi.loc[show_teams, "title_prob"] * 100).round(1),
    })
    comp["Change"] = (comp["After"] - comp["Before"]).round(1)
    comp = comp.sort_values("After", ascending=False)
    st.dataframe(
        comp, width="stretch",
        column_config={
            "Before": st.column_config.NumberColumn("Before", format="%.1f%%"),
            "After": st.column_config.ProgressColumn(
                "After", min_value=0, max_value=100, format="%.1f%%", color=theme.RED),
            "Change": st.column_config.NumberColumn("Change", format="%+.1f pts"),
        },
    )
    ui.verdict(
        "This is a full resimulation, not an adjustment. All 10,000 seasons were replayed with "
        "those results locked in and everything else still random, so the knock-on effects are "
        "included: a fixed result changes the simulated table, which changes how much pressure "
        "every later match carries, which changes those probabilities too."
    )

ui.note(
    "**Known limitations, stated plainly.** The model predicts win, draw or loss and never a "
    "scoreline, so simulated ties on points are broken at random rather than on goal "
    "difference. Each club's form is frozen at today's value for the rest of the season. Newly "
    "promoted clubs have no Premier League history for the model to read. And the early-season "
    "weakness shown in the backtest above is real and currently unfixed.",
    kind="warn",
)
