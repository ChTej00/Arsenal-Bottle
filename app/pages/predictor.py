"""Act 4: the live 2026-27 simulation, its track record, and a what-if tool."""
import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "The 2026-27 title race",
    "Everything before this page looks backwards. This one runs forwards: it plays out every "
    "remaining fixture of the season now being played, ten thousand times, and counts how "
    "often each club ends up on top.",
    eyebrow="Act 4 · The Predictor",
)
ui.stepper("pages/predictor.py")
st.write("")

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
    ui.callout(
        "caveat", "Read this before the numbers.",
        f"Only {gw} gameweeks are played. The model reads form from each club's last five "
        "matches and that window resets every August, so it is currently working from five "
        "matches per club and projecting them across thirty-three. The backtest further down "
        "this page shows exactly how unreliable that is: at this stage last season the same "
        "method gave the eventual champion 29.7%, against 85.7% by the midpoint. These are the "
        "least trustworthy numbers on the site, and they are published anyway because hiding "
        "them would be worse.",
    )

# ---------------------------------------------------------------------------
st.subheader("Who wins the league?", anchor=False)

lead = summary.iloc[0]
ui.metric_grid([
    (summary.index[0], "Favourite", "Most likely champion across 10,000 simulations",
     "leads the race"),
    (f"{lead['title_prob']*100:.1f}%", "Title probability",
     "Share of simulations this club finished top", "of 10,000 runs"),
    (f"{lead['pts_median']:.0f}", "Projected points", "Median finish across simulations",
     f"{lead['pts_p5']:.0f} to {lead['pts_p95']:.0f} range"),
    (f"{fixtures['gameweek'].nunique()}", "Gameweeks left",
     "Remaining rounds still to be simulated", "still to play"),
])

top = summary.head(8).sort_values("title_prob")
fig = go.Figure(go.Bar(
    x=top["title_prob"] * 100, y=top.index, orientation="h",
    marker=dict(color=[theme.team_color(t) for t in top.index], line=dict(width=0)),
    text=[f"{v*100:.1f}%" for v in top["title_prob"]], textposition="outside",
    cliponaxis=False, textfont=dict(color=theme.COLOR["text_primary"], size=12),
    hovertemplate="<b>%{y}</b><br>Wins the league in %{x:.1f}% of simulations<extra></extra>",
))
theme.apply(fig, height=370, legend=False,
            xaxis=dict(title=dict(text="Probability of finishing first (%)"),
                       range=[0, float(top["title_prob"].max()) * 132]))
ui.chart(
    fig,
    title=f"{summary.index[0]} leads, {summary.index[1]} is the only real challenger",
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

@st.fragment
def club_detail() -> None:
    """A fragment so choosing a club reruns this block alone, not the page, and
    so the 10,000-run baseline simulation is not touched. The choice is mirrored
    into the URL, which makes a club's view shareable as a link."""
    clubs = list(summary.index)
    from_url = st.query_params.get("club")
    start = clubs.index(from_url) if from_url in clubs else 0

    pick = st.selectbox("Show the full range of outcomes for", clubs, index=start,
                        key="club_pick")
    if st.query_params.get("club") != pick:
        st.query_params["club"] = pick

    i = teams.index(pick)
    pts = final_pts[:, i]
    row = summary.loc[pick]

    ui.metric_grid([
        (f"{row['title_prob']*100:.1f}%", "Title",
         "Share of simulations finishing first", "wins the league"),
        (f"{row['top4_prob']*100:.1f}%", "Top four",
         "Share of simulations finishing in the top four", "Champions League"),
        (f"{row['releg_prob']*100:.1f}%", "Relegation",
         "Share of simulations finishing in the bottom three", "goes down"),
        (f"{row['pts_median']:.0f}", "Most likely points",
         "Median points across simulations",
         f"{row['pts_p5']:.0f} to {row['pts_p95']:.0f} range"),
    ])

    fig = go.Figure(go.Histogram(
        x=pts, nbinsx=40, marker_color=theme.team_color(pick), opacity=0.85,
        hovertemplate="<b>%{x} points</b><br>in %{y} of 10,000 simulations<extra></extra>",
    ))
    for val, label in [(row["pts_p5"], "5th pct"), (row["pts_median"], "median"),
                       (row["pts_p95"], "95th pct")]:
        fig.add_vline(x=val, line=dict(color=theme.COLOR["annotation"], width=1.5,
                                       dash="dash"),
                      annotation_text=label,
                      annotation_font=dict(color=theme.COLOR["annotation"], size=10))
    theme.apply(fig, height=330, legend=False,
                xaxis=dict(title=dict(text="Final points")),
                yaxis=dict(title=dict(text="Number of simulations")))
    ui.chart(
        fig,
        title=f"{pick} finishes anywhere from {row['pts_p5']:.0f} to "
              f"{row['pts_p95']:.0f} points",
        verdict_text=(
            f"Not a single prediction but a distribution. {pick} finishes somewhere between "
            f"<strong>{row['pts_p5']:.0f} and {row['pts_p95']:.0f} points</strong> in 90% of "
            "simulations. The width of that range is the honest measure of how much is still "
            "undecided, and this early in a season it is very wide."
        ),
        source_note="This selection is stored in the page address, so the link shares it.",
    )


club_detail()

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
        hovertemplate=(f"<b>{team}</b><br>Most likely %{{x:.0f}} points"
                       f"<br>90% range {r['pts_p5']:.0f} to {r['pts_p95']:.0f}<extra></extra>"),
    ))
theme.apply(fig, height=620, legend=False,
            xaxis=dict(title=dict(text="Final points")))
ui.chart(
    fig,
    title="Outside the top three, the table is still undecided",
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

ui.callout(
    "definition", "A forecast nobody has checked is worth nothing.",
    "Both tabs below replay the completed 2025-26 season with a model that was never shown "
    "it: freeze the table at a chosen gameweek, hide every later result, simulate forward, "
    "and compare against what really happened.",
)

tab20, tab5 = st.tabs(["Frozen at gameweek 20 (midseason)",
                       "Frozen at gameweek 5 (where we are now)"])
for tab, bt, cutoff in [(tab20, bt20, 20), (tab5, bt5, 5)]:
    with tab:
        n_in = int(bt["inside_range"].sum())
        champ = bt.loc[bt["real_points"].idxmax()]
        ui.metric_grid([
            (f"{n_in} of 20", "Clubs inside range",
             "A well-calibrated 90% range should catch about 18", "of 20 clubs"),
            (f"{champ['title_prob']*100:.1f}%", "Champion's odds",
             f"What the model gave {champ['team']}, the club that actually won",
             f"given to {champ['team']}"),
            (f"{champ['pts_median']:.0f}", "Its projected points",
             "Median simulated points for the eventual champion",
             f"really got {champ['real_points']:.0f}"),
        ])

        b = bt.sort_values("pts_median")
        fig = go.Figure()
        for _, r in b.iterrows():
            fig.add_trace(go.Scatter(
                x=[r["pts_p5"], r["pts_p95"]], y=[r["team"], r["team"]], mode="lines",
                line=dict(color=theme.COLOR["grid"], width=7),
                showlegend=False, hoverinfo="skip",
            ))
            fig.add_trace(go.Scatter(
                x=[r["real_points"]], y=[r["team"]], mode="markers",
                marker=dict(color=theme.COLOR["positive"] if r["inside_range"]
                            else theme.COLOR["negative"], size=9, line=dict(width=0)),
                showlegend=False,
                hovertemplate=(f"<b>{r['team']}</b><br>Really got %{{x:.0f}} points"
                               f"<br>Predicted {r['pts_p5']:.0f} to {r['pts_p95']:.0f}"
                               "<extra></extra>"),
            ))
        theme.apply(fig, height=600, legend=False,
                    xaxis=dict(title=dict(text="Final points")))
        ui.bare_chart(fig, key=f"bt{cutoff}")

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
            hovertemplate="<b>%{fullData.name}</b><br>%{x:,} runs: %{y:.1f}%<extra></extra>",
        ))
    theme.apply(fig, height=320,
                xaxis=dict(title=dict(text="Simulations"), type="log"),
                yaxis=dict(title=dict(text="Title probability (%)")))
    ui.bare_chart(fig)
    st.markdown(
        "More simulations reduce random noise in the answer, at the cost of time. The lines "
        "flatten well before 10,000 and the ordering never changes between 1,000 and 50,000, "
        "so 10,000 is comfortably enough. Each run takes a couple of seconds."
    )

# ---------------------------------------------------------------------------
st.subheader("How the odds have moved", anchor=False)

if odds_hist["gameweek"].nunique() < 2:
    ui.callout(
        "scope", "Only one gameweek has been published so far,",
        "so there is no line to draw yet. This chart fills in as the season progresses: one "
        "point per club per gameweek, added automatically each time a full gameweek completes.",
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
            hovertemplate="<b>%{fullData.name} · gameweek %{x}</b>"
                          "<br>%{y:.1f}% title probability<extra></extra>",
        ))
    theme.apply(fig, height=390, xaxis=dict(title=dict(text="Gameweek")),
                yaxis=dict(title=dict(text="Title probability (%)")))
    ui.chart(fig, title="How the odds have moved, gameweek by gameweek", verdict_text=(
        "Each point is a republished forecast after a completed gameweek. Steep moves mean a "
        "result changed the picture materially."
    ))

# ---------------------------------------------------------------------------
st.subheader("Change a result and watch the table move", anchor=False)

st.markdown(
    "Pick up to three remaining fixtures, fix their results, and the whole season is simulated "
    "again from scratch around them."
)

@st.fragment
def what_if() -> None:
    """A fragment so picking fixtures and resimulating reruns this block alone.
    Without it, every radio click would rerun the whole page and re-read the
    baseline simulation."""
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
                outcome = {f"{r['home_team']} win": 2, "Draw": 1,
                           f"{r['away_team']} win": 0}[choice]
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
        ui.table(comp, hide_index=False, config={
            "Before": st.column_config.NumberColumn("Before", format="%.1f%%",
                                                    width="small"),
            "After": st.column_config.ProgressColumn(
                "After", min_value=0, max_value=100, format="%.1f%%",
                color=theme.COLOR["brand"], width="medium"),
            "Change": st.column_config.NumberColumn("Change", format="%+.1f pts",
                                                    width="small"),
        })
        ui.verdict(
            "This is a full resimulation, not an adjustment. All 10,000 seasons were replayed "
            "with those results locked in and everything else still random, so the knock-on "
            "effects are included: a fixed result changes the simulated table, which changes "
            "how much pressure every later match carries, which changes those probabilities "
            "too."
        )


what_if()

ui.callout(
    "caveat", "Known limitations, stated plainly.",
    "The model predicts win, draw or loss and never a scoreline, so simulated ties on points "
    "are broken at random rather than on goal difference. Each club's form is frozen at "
    "today's value for the rest of the season. Newly promoted clubs have no Premier League "
    "history for the model to read. And the early-season weakness shown in the backtest above "
    "is real and currently unfixed.",
)

ui.prev_next("pages/predictor.py")
