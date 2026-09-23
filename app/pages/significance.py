"""Every claim so far, put through a proper test. Almost nothing survives."""
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "Does any of it survive a test?",
    "Everything on the previous pages is descriptive. Patterns in data look convincing even "
    "when they are noise, and the only way to know the difference is to test them. This page "
    "does that, and the short version is that the bottle does not survive.",
    eyebrow="The evidence",
)

tests = loaders.table("stakes_tests.csv")
normality = loaders.table("normality.csv")
perm = loaders.table("permutation.csv")
wilcoxon = loaders.table("wilcoxon.csv")
overlap = loaders.table("stakes_overlap.csv")
bias = loaders.table("stakes_bias_check.csv")
kruskal = loaders.table("kruskal.csv")
power = loaders.table("power.csv")

ui.callout(
    "definition", "What a p-value is, in one line.",
    "It is the probability of seeing a pattern at least this strong purely by chance, if there "
    "were really nothing there. Below 0.05 is the conventional bar for calling something real. "
    "The catch, which drives this entire page, is that if you run twenty tests, one of them "
    "will clear that bar by luck alone.",
)

# ---------------------------------------------------------------------------
st.subheader("The problem with testing eight things at once", anchor=False)

t = tests.copy()
t["Measure"] = t["metric"].map({"xG": "Chances created", "xGA": "Chances conceded"})
order = t.sort_values("p_welch")
labels = [f"{r['team']} · {r['Measure']}" for _, r in order.iterrows()]

fig = go.Figure()
fig.add_trace(go.Bar(
    y=labels, x=order["p_welch"], orientation="h", name="Tested on its own",
    marker_color=theme.COLOR["brand"], opacity=0.9,
    hovertemplate="<b>%{y}</b><br>p = %{x:.3f} tested alone<extra></extra>",
))
fig.add_trace(go.Bar(
    y=labels, x=order["p_bonferroni"], orientation="h", name="Corrected for all 8 tests",
    marker_color=theme.COLOR["faint"],
    hovertemplate="<b>%{y}</b><br>p = %{x:.3f} after correction<extra></extra>",
))
fig.add_vline(x=0.05, line=dict(color=theme.COLOR["annotation"], width=1.5, dash="dash"),
              annotation_text="significance threshold", annotation_position="top right",
              annotation_font=dict(color=theme.COLOR["annotation"], size=11))
theme.apply(fig, height=420, barmode="group",
            xaxis=dict(title=dict(text="p-value (lower means stronger evidence)"),
                       range=[0, 1.04], tickformat=".2f"))
n_raw = int(t["significant_raw"].sum())
n_corr = int(t["significant_corrected"].sum())

ui.metric_grid([
    (f"{n_raw} of 8", "Significant on their own",
     "Tested one at a time, ignoring that eight were run", "before correction"),
    (f"{n_corr} of 8", "Significant after correction",
     "Corrected for the fact that all eight were tested together", "nothing survives"),
])

ui.chart(
    fig,
    title="Correcting for eight tests leaves nothing significant",
    verdict_text=(
        f"Tested one at a time, <strong>{n_raw} of the 8 look significant</strong>: Arsenal's "
        "and Manchester United's defensive numbers, the exact result the previous pages leaned "
        "on. Corrected for the fact that all 8 were tested together, "
        f"<strong>{n_corr} survive</strong>. Nothing crosses the line. The defensive softening "
        "is the kind of result that appears when you look at eight things and report the two "
        "that stood out."
    ),
    method_text=(
        "Each test is a Welch's t-test comparing a team's high-pressure matches against its "
        "normal ones, which does not assume the two groups have equal variance. The correction "
        "is Bonferroni: multiply each p-value by the number of tests in the family. "
        "Benjamini-Hochberg, a less conservative correction, is also computed and reaches the "
        "same verdict. Mann-Whitney U, which assumes nothing about the shape of the "
        "distributions, agrees too."
    ),
)

# ---------------------------------------------------------------------------
st.subheader("Testing the bottle gap directly", anchor=False)

sig = perm[perm["significant_raw"]]
fig = go.Figure()
for _, r in perm.iterrows():
    label = f"{r['team'][:11]} {theme.season_label(r['season'])}"
    is_ars = r["team"] == "Arsenal"
    fig.add_trace(go.Scatter(
        x=[r["null_p5"], r["null_p95"]], y=[label] * 2, mode="lines",
        line=dict(color=theme.COLOR["grid"], width=6), showlegend=False, hoverinfo="skip",
    ))
    fig.add_trace(go.Scatter(
        x=[r["observed_gap"]], y=[label], mode="markers",
        marker=dict(color=theme.COLOR["brand"] if is_ars else theme.COLOR["faint"],
                    size=10 if is_ars else 7, line=dict(width=0)),
        showlegend=False,
        hovertemplate=(f"<b>{r['team']} {theme.season_label(r['season'])}</b>"
                       f"<br>Gap %{{x:+.2f}} points per match"
                       f"<br>p = {r['p_value']:.3f}<extra></extra>"),
    ))
fig.add_vline(x=0, line=dict(color=theme.COLOR["muted"], width=1, dash="dash"))
theme.apply(fig, height=600, legend=False,
            xaxis=dict(title=dict(text="Points-per-match gap in big matches")))
ui.chart(
    fig,
    title="One of 28 team-seasons beats chance, and it is not Arsenal",
    verdict_text=(
        "The grey bar is the range you would get by picking ten matches at random from that "
        "season instead of the ten biggest. The red dots are Arsenal. "
        f"<strong>{len(sig)} of the 28 team-seasons falls outside its own chance range</strong>, "
        "and it is Liverpool in 2019-20, not any Arsenal season. None of Arsenal's four title "
        "challenges come close: their p-values run from 0.64 to 1.00, meaning their real gaps "
        "are entirely ordinary."
    ),
    method_text=(
        "A permutation test. For each team-season, take that season's actual 38 results, "
        "shuffle which ten of them count as high-pressure, recompute the gap, and repeat "
        "10,000 times. That builds the distribution of gaps you would see if pressure had no "
        "effect whatsoever. The p-value is how often the shuffled gap was at least as extreme "
        "as the real one."
    ),
)

pooled = wilcoxon[wilcoxon["scope"] == "Pooled (all 4 teams)"].iloc[0]
ui.callout(
    "finding", "A second, independent test agrees.",
    "Treating each team-season as a single observation rather than 38 correlated matches, a "
    "Wilcoxon signed-rank test asks whether the gaps lean consistently one way across seasons. "
    f"Pooled across all four clubs, **p = {pooled['p_value']:.2f}**. Every club individually "
    "is above 0.37. Two quite different tests, the same answer.",
)

# ---------------------------------------------------------------------------
st.subheader("A finding of ours that turned out to be wrong", anchor=False)

ui.callout(
    "correction", "This section documents a mistake.",
    "An earlier version of this analysis reported a striking result: that high-pressure "
    "matches overlap a team's best results at more than double the rate chance would predict, "
    "which would mean the pressure metric was structurally biased. Rebuilding it for this "
    "site, it turned out not to be true.",
)

chron = overlap[overlap["tiebreak"] == "chronological"].iloc[0]
rev = overlap[overlap["tiebreak"] == "reverse_chronological"].iloc[0]
rnd = overlap[overlap["tiebreak"] == "random"].iloc[0]

buckets = ["Worst results", "Middle results", "Best results"]
keys = ["worst", "middle", "best"]
fig = go.Figure()
for row, name, color in [(chron, "Ties broken by date (the original)",
                          theme.COLOR["brand"]),
                         (rev, "Ties broken by reverse date",
                          theme.CLUB["Manchester City"]),
                         (rnd, "Ties broken at random", theme.COLOR["positive"])]:
    fig.add_trace(go.Bar(
        name=name, x=buckets, y=[row[k] for k in keys], marker_color=color,
        hovertemplate=f"<b>{name}</b>"
                      "<br>%{x}: %{y:.2f} of 10 matches<extra></extra>",
    ))
fig.add_trace(go.Scatter(
    x=buckets, y=[chron["chance_worst"], chron["chance_middle"], chron["chance_best"]],
    mode="markers", name="What chance predicts",
    marker=dict(symbol="line-ew", size=46,
                line=dict(color=theme.COLOR["text_primary"], width=2.5)),
    hovertemplate="<b>Chance baseline</b><br>%{y:.2f} of 10 matches<extra></extra>",
))
theme.apply(fig, height=400, barmode="group",
            yaxis=dict(title=dict(text="High-pressure matches in this group (of 10)")))
ui.chart(
    fig,
    title="Change how ties are broken and the finding disappears",
    verdict_text=(
        "A team's points can only be 0, 1 or 3, so in a 38-match season the best ten results "
        "is mostly decided by how you order matches that are tied. Break ties by date and "
        f"high-pressure matches look like they cluster among the best results "
        f"({chron['best']:.2f} against a chance baseline of {chron['chance_best']:.2f}). Break "
        f"them in the opposite order and the same data gives {rev['best']:.2f}. <strong>Break "
        f"them at random and it sits on the chance baseline ({rnd['best']:.2f}).</strong> The "
        "original finding was measuring its own sorting rule, not the football."
    ),
    method_text=(
        "Why the date ordering produced it: high-pressure matches fall late in a season by "
        f"construction (average gameweek "
        f"{bias[bias['team'] == 'All four teams'].iloc[0]['mean_gameweek_high_stakes']:.1f} "
        f"against {bias[bias['team'] == 'All four teams'].iloc[0]['mean_gameweek_normal']:.1f} "
        "for normal matches). A stable sort leaves tied matches in date order, so among a "
        "season's many tied wins, the late ones land at the best end. The high-pressure "
        "matches were being sorted into the best bucket by their date, not by their result."
    ),
)

allf = bias[bias["team"] == "All four teams"].iloc[0]
ui.metric_grid([
    (f"{allf['win_rate_high_stakes']*100:.1f}%", "Win rate, big matches",
     "Share of high-pressure matches won, all four clubs", "no sorting involved"),
    (f"{allf['win_rate_normal']*100:.1f}%", "Win rate, normal matches",
     "Share of ordinary matches won, all four clubs", "no sorting involved"),
    (f"{allf['win_rate_diff']*100:+.1f} pts", "Difference",
     "If the metric were biased toward good results, this would be large",
     "essentially nothing"),
])

ui.callout(
    "finding", "Asked without any ties to break, the bias is not there.",
    "Do teams simply win more of their high-pressure matches? Across all four clubs the "
    f"difference is **{allf['win_rate_diff']*100:+.1f} percentage points**. The pressure "
    "metric is not biased toward a team's good results, and the conclusions drawn from the "
    "original version of this finding have been removed from the rest of the site.",
)

# ---------------------------------------------------------------------------
st.subheader("Can these four clubs even be told apart?", anchor=False)

kw = kruskal.iloc[0]
c1, c2 = st.columns([1, 2], gap="medium")
with c1:
    st.metric("Kruskal-Wallis H", f"{kw['H_statistic']:.2f}", border=True, height=104,
              help="The test statistic. Larger means the groups differ more")
    st.metric("p-value", f"{kw['p_value']:.2f}", border=True, height=104,
              help="Nowhere near the 0.05 threshold")
with c2:
    st.markdown(
        "A Kruskal-Wallis test asks whether several groups differ at all, without assuming "
        "anything about the shape of their distributions. Across the four clubs' season-level "
        f"pressure gaps it returns p = {kw['p_value']:.3f}, which is about as far from "
        "significant as a result can get.\n\nCombined with the overlapping uncertainty ranges "
        "on the comparator page, the honest conclusion is that **these four clubs are "
        "statistically indistinguishable from one another** on this measure."
    )

# ---------------------------------------------------------------------------
st.subheader("Why none of this means the bottle is fake", anchor=False)

mdes = float(power["min_detectable_d"].iloc[0])
obs = power.copy()
obs["label"] = obs["team"].str[:11] + "<br>" + obs["metric"].map(
    {"xG": "created", "xGA": "conceded", "points": "points"})

fig = go.Figure(go.Bar(
    x=obs["label"], y=obs["observed_cohens_d"],
    marker_color=theme.COLOR["brand"], opacity=0.85,
    hovertemplate="<b>%{x}</b><br>Effect size %{y:.2f}<extra></extra>",
))
fig.add_hline(y=mdes, line=dict(color=theme.COLOR["annotation"], width=2, dash="dash"),
              annotation_text=f"smallest effect this sample could detect (d={mdes:.2f})",
              annotation_position="top left",
              annotation_font=dict(color=theme.COLOR["annotation"], size=11))
theme.apply(fig, height=400, legend=False,
            yaxis=dict(title=dict(text="Effect size (Cohen's d)"), range=[0, mdes * 1.2]))
ui.chart(
    fig,
    title="Every real effect is far below what this sample could detect",
    verdict_text=(
        "With ten big matches a season against twenty-eight normal ones, a difference has to "
        f"be <strong>enormous</strong> (d = {mdes:.2f}) before this sample could reliably "
        f"detect it. Every effect actually observed is between "
        f"{obs['observed_cohens_d'].min():.2f} and {obs['observed_cohens_d'].max():.2f}. "
        "<strong>The tests above were never capable of finding a real football-sized "
        "effect.</strong> A null result from an underpowered test is not evidence of absence, "
        "it is an absence of evidence."
    ),
    method_text=(
        "A power analysis run backwards. Fixing the sample sizes at what is actually available "
        "(n=10 and n=28), the significance threshold at 0.05 and the desired power at 80%, it "
        "solves for the smallest effect size that setup could reliably catch. Cohen's d is the "
        "difference between two group means expressed in standard deviations; 0.2 is "
        "conventionally small, 0.5 medium, 0.8 large."
    ),
)

st.subheader("So what is the answer?", anchor=False)
ui.cards([
    ("Not proven", "No finding in this project survives a properly corrected significance "
                   "test. Anyone claiming the data proves Arsenal bottled it is overreading it."),
    ("Not disproven either", "The sample is far too small to detect an effect of the size "
                             "football actually produces. These tests could not have found it "
                             "even if it were there."),
    ("What would settle it", "More seasons, not cleverer tests. The limit is ten big matches "
                             "a year, and no amount of statistical technique creates "
                             "information the data does not contain."),
])

with st.expander("Distribution checks behind these tests", icon=":material/functions:"):
    ui.table(normality, {
        "variable": st.column_config.TextColumn("Variable", width="medium"),
        "shapiro_W": st.column_config.NumberColumn("Shapiro-Wilk W", format="%.3f"),
        "p_value": st.column_config.NumberColumn("p-value", format="%.2e"),
        "normal_at_05": st.column_config.CheckboxColumn("Normal?"),
        "n": st.column_config.NumberColumn("n"),
    })
    st.markdown(
        "Points can only be 0, 1 or 3, so it is nowhere near a normal distribution and fails "
        "badly, exactly as expected. This is why every parametric test here is paired with a "
        "non-parametric equivalent that assumes nothing about distribution shape."
    )

ui.prev_next("pages/significance.py")
