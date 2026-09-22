"""Every claim so far, put through a proper test. Almost nothing survives."""
import numpy as np
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "Is it real?",
    "Everything on the previous pages is descriptive. Patterns in data look convincing even "
    "when they are noise, and the only way to know the difference is to test them. This page "
    "does that, and the short version is that the bottle does not survive.",
)

tests = loaders.table("stakes_tests.csv")
normality = loaders.table("normality.csv")
perm = loaders.table("permutation.csv")
wilcoxon = loaders.table("wilcoxon.csv")
overlap = loaders.table("stakes_overlap.csv")
bias = loaders.table("stakes_bias_check.csv")
kruskal = loaders.table("kruskal.csv")
power = loaders.table("power.csv")

ui.note(
    "<b>What a p-value is, in one line.</b> It is the probability of seeing a pattern at least "
    "this strong purely by chance, if there were really nothing there. Below 0.05 is the "
    "conventional bar for calling something real. The catch, which drives this entire page, is "
    "that if you run twenty tests, one of them will clear that bar by luck alone."
)

# ---------------------------------------------------------------------------
st.markdown("### The problem with testing eight things at once")

t = tests.copy()
t["Team"] = t["team"]
t["Measure"] = t["metric"].map({"xG": "Chances created", "xGA": "Chances conceded"})
order = t.sort_values("p_welch")

fig = go.Figure()
labels = [f"{r['Team']} · {r['Measure']}" for _, r in order.iterrows()]
fig.add_trace(go.Bar(
    y=labels, x=order["p_welch"], orientation="h", name="On its own",
    marker_color=theme.RED, opacity=0.9,
    hovertemplate="%{y}<br>p = %{x:.3f} tested alone<extra></extra>",
))
fig.add_trace(go.Bar(
    y=labels, x=order["p_bonferroni"], orientation="h", name="Corrected for all 8 tests",
    marker_color=theme.FAINT,
    hovertemplate="%{y}<br>p = %{x:.3f} after correction<extra></extra>",
))
fig.add_vline(x=0.05, line=dict(color=theme.POSITIVE, width=1.5, dash="dash"),
              annotation_text="significance threshold", annotation_position="top",
              annotation_font=dict(color=theme.POSITIVE, size=11))
theme.apply(fig, height=430, barmode="group",
            title="The same eight tests, before and after correcting for multiplicity",
            xaxis=dict(title=dict(text="p-value (lower means stronger evidence)"),
                       range=[0, 1.04], tickformat=".2f"))
n_raw = int(t["significant_raw"].sum())
n_corr = int(t["significant_corrected"].sum())
ui.chart(fig, verdict_text=(
    f"Tested one at a time, <b>{n_raw} of the 8 look significant</b>: Arsenal's and Manchester "
    "United's defensive numbers, the exact result the previous pages leaned on. Corrected for "
    f"the fact that all 8 were tested together, <b>{n_corr} survive</b>. Nothing crosses the "
    "line. The defensive softening is the kind of result that appears when you look at eight "
    "things and report the two that stood out."
), method_text=(
    "Each test is a Welch's t-test comparing a team's high-pressure matches against its normal "
    "ones, which does not assume the two groups have equal variance. The correction is "
    "Bonferroni: multiply each p-value by the number of tests in the family. Benjamini-Hochberg, "
    "a less conservative correction, is also computed and reaches the same verdict. "
    "Mann-Whitney U, which assumes nothing about the shape of the distributions, agrees too."
))

with st.expander("Full test results, including non-parametric and effect sizes"):
    show = t[["Team", "Measure", "high_stakes_mean", "normal_mean", "difference",
              "cohens_d", "p_welch", "p_mannwhitney", "p_bonferroni",
              "p_benjamini_hochberg"]].rename(columns={
        "high_stakes_mean": "Big matches", "normal_mean": "Normal", "difference": "Difference",
        "cohens_d": "Effect size (d)", "p_welch": "p (t-test)",
        "p_mannwhitney": "p (Mann-Whitney)", "p_bonferroni": "p (Bonferroni)",
        "p_benjamini_hochberg": "p (BH)",
    })
    st.dataframe(show.style.format({c: "{:.3f}" for c in show.columns if c not in
                                    ["Team", "Measure"]}),
                 width="stretch", hide_index=True)

# ---------------------------------------------------------------------------
st.markdown("### Testing the bottle gap directly")

sig = perm[perm["significant_raw"]]
fig = go.Figure()
for _, r in perm.iterrows():
    is_ars = r["team"] == "Arsenal"
    fig.add_trace(go.Scatter(
        x=[r["null_p5"], r["null_p95"]], y=[f"{r['team'][:11]} {theme.season_label(r['season'])}"] * 2,
        mode="lines", line=dict(color=theme.LINE, width=5), showlegend=False, hoverinfo="skip",
    ))
    fig.add_trace(go.Scatter(
        x=[r["observed_gap"]],
        y=[f"{r['team'][:11]} {theme.season_label(r['season'])}"],
        mode="markers",
        marker=dict(color=theme.RED if is_ars else theme.FAINT,
                    size=9 if is_ars else 6, line=dict(width=0)),
        showlegend=False,
        hovertemplate=(f"{r['team']} {theme.season_label(r['season'])}"
                       f"<br>gap %{{x:+.3f}}<br>p = {r['p_value']:.3f}<extra></extra>"),
    ))
fig.add_vline(x=0, line=dict(color=theme.MUTED, width=1, dash="dash"))
theme.apply(fig, height=620, legend=False,
            title="Each team-season's real gap against what chance alone produces",
            xaxis_title="Points-per-match gap in big matches")
ui.chart(fig, verdict_text=(
    "The grey bar is the range you would get by picking ten matches at random from that season "
    f"instead of the ten biggest. The dot is what actually happened. <b>{len(sig)} of the 28 "
    "team-seasons falls outside its own chance range</b>, and it is Liverpool in 2019-20, not "
    "any Arsenal season. None of Arsenal's four title challenges come close: their p-values run "
    "from 0.64 to 1.00, meaning their real gaps are entirely ordinary."
), method_text=(
    "A permutation test. For each team-season, take that season's actual 38 results, shuffle "
    "which ten of them count as high-pressure, recompute the gap, and repeat 10,000 times. "
    "That builds the distribution of gaps you would see if pressure had no effect whatsoever. "
    "The p-value is how often the shuffled gap was at least as extreme as the real one."
))

pooled = wilcoxon[wilcoxon["scope"] == "Pooled (all 4 teams)"].iloc[0]
ui.note(
    "A second, independent test agrees. Treating each team-season as a single observation "
    "rather than 38 correlated matches, a Wilcoxon signed-rank test asks whether the gaps lean "
    f"consistently one way across seasons. Pooled across all four clubs, <b>p = "
    f"{pooled['p_value']:.2f}</b>. Every club individually is above 0.37. Two quite different "
    "tests, the same answer."
)

# ---------------------------------------------------------------------------
st.markdown("### A finding of ours that turned out to be wrong")

ui.note(
    "<b>This section documents a mistake.</b> An earlier version of this analysis reported a "
    "striking result: that high-pressure matches overlap a team's best results at more than "
    "double the rate chance would predict, which would mean the pressure metric was structurally "
    "biased. Rebuilding it for this site, it turned out not to be true.",
    kind="flag",
)

chron = overlap[overlap["tiebreak"] == "chronological"].iloc[0]
rev = overlap[overlap["tiebreak"] == "reverse_chronological"].iloc[0]
rnd = overlap[overlap["tiebreak"] == "random"].iloc[0]

fig = go.Figure()
buckets = ["Worst results", "Middle results", "Best results"]
cols = ["worst", "middle", "best"]
for row, name, color in [(chron, "Ties broken by date (the original)", theme.RED),
                         (rev, "Ties broken by reverse date", theme.SKY),
                         (rnd, "Ties broken at random", theme.POSITIVE)]:
    fig.add_trace(go.Bar(
        name=name, x=buckets, y=[row[c] for c in cols], marker_color=color,
        hovertemplate="%{x}<br>%{y:.2f} of 10 matches<extra></extra>",
    ))
fig.add_trace(go.Scatter(
    x=buckets, y=[chron["chance_worst"], chron["chance_middle"], chron["chance_best"]],
    mode="markers", name="what chance predicts",
    marker=dict(symbol="line-ew", size=42, line=dict(color=theme.TEXT, width=2.5)),
    hovertemplate="chance baseline %{y:.2f}<extra></extra>",
))
theme.apply(fig, height=420, barmode="group",
            title="The same measurement, three ways of breaking ties",
            yaxis_title="High-pressure matches landing in this group (of 10)")
ui.chart(fig, verdict_text=(
    f"A team's points can only be 0, 1 or 3, so in a 38-match season the 'best ten results' is "
    f"mostly decided by how you order matches that are tied. Break ties by date and "
    f"high-pressure matches look like they cluster among the best results "
    f"({chron['best']:.2f} against a chance baseline of {chron['chance_best']:.2f}). "
    f"Break them in the opposite order and the same data gives {rev['best']:.2f}. "
    f"<b>Break them at random and it sits on the chance baseline ({rnd['best']:.2f}).</b> "
    "The original finding was measuring its own sorting rule, not the football."
), method_text=(
    "Why the date ordering produced it: high-pressure matches fall late in a season by "
    f"construction (average gameweek "
    f"{bias[bias['team'] == 'All four teams'].iloc[0]['mean_gameweek_high_stakes']:.1f} against "
    f"{bias[bias['team'] == 'All four teams'].iloc[0]['mean_gameweek_normal']:.1f} for normal "
    "matches). A stable sort leaves tied matches in date order, so among a season's many tied "
    "wins, the late ones land at the 'best' end. The high-pressure matches were being sorted "
    "into the best bucket by their date, not by their result."
))

allf = bias[bias["team"] == "All four teams"].iloc[0]
ui.note(
    "The same question asked in a way that has no ties to break: do teams simply win more of "
    f"their high-pressure matches? Across all four clubs they win "
    f"<b>{allf['win_rate_high_stakes']*100:.1f}%</b> of big matches against "
    f"<b>{allf['win_rate_normal']*100:.1f}%</b> of normal ones, a difference of "
    f"{allf['win_rate_diff']*100:+.1f} percentage points. Essentially nothing. The pressure "
    "metric is not biased toward a team's good results, and the conclusions drawn from the "
    "original version of this finding have been removed from the rest of the site."
)

# ---------------------------------------------------------------------------
st.markdown("### Can these four clubs even be told apart?")

kw = kruskal.iloc[0]
c1, c2 = st.columns([1, 2])
with c1:
    st.metric("Kruskal-Wallis H", f"{kw['H_statistic']:.3f}")
    st.metric("p-value", f"{kw['p_value']:.3f}")
with c2:
    ui.note(
        "A Kruskal-Wallis test asks whether several groups differ at all, without assuming "
        f"anything about the shape of their distributions. Across the four clubs' season-level "
        f"pressure gaps it returns p = {kw['p_value']:.3f}, which is about as far from "
        "significant as a result can get. Combined with the overlapping uncertainty ranges on "
        "the comparator page, the honest conclusion is that <b>these four clubs are "
        "statistically indistinguishable from one another</b> on this measure."
    )

# ---------------------------------------------------------------------------
st.markdown("### Why none of this means the bottle is fake")

mdes = power["min_detectable_d"].iloc[0]
obs = power[power["metric"].isin(["xG", "xGA", "points"])]

fig = go.Figure()
fig.add_trace(go.Bar(
    x=[f"{r['team'][:11]}<br>{r['metric']}" for _, r in obs.iterrows()],
    y=obs["observed_cohens_d"], marker_color=theme.RED, opacity=0.85,
    name="effect actually observed",
    hovertemplate="%{x}<br>d = %{y:.3f}<extra></extra>",
))
fig.add_hline(y=mdes, line=dict(color=theme.POSITIVE, width=2, dash="dash"),
              annotation_text=f"smallest effect this sample could detect (d={mdes:.2f})",
              annotation_position="top left",
              annotation_font=dict(color=theme.POSITIVE, size=11))
theme.apply(fig, height=420, legend=False,
            title="Observed effects against what the sample size can actually detect",
            yaxis_title="Effect size (Cohen's d)")
ui.chart(fig, verdict_text=(
    f"With ten big matches a season against twenty-eight normal ones, a difference has to be "
    f"<b>enormous</b> (d = {mdes:.2f}) before this sample could reliably detect it. Every effect "
    f"actually observed is between {obs['observed_cohens_d'].min():.2f} and "
    f"{obs['observed_cohens_d'].max():.2f}. <b>The tests above were never capable of finding a "
    "real football-sized effect.</b> A null result from an underpowered test is not evidence of "
    "absence, it is an absence of evidence."
), method_text=(
    "A power analysis run backwards. Fixing the sample sizes at what is actually available "
    "(n=10 and n=28), the significance threshold at 0.05 and the desired power at 80%, it "
    "solves for the smallest effect size that setup could reliably catch. Cohen's d is the "
    "difference between two group means expressed in standard deviations; 0.2 is conventionally "
    "small, 0.5 medium, 0.8 large."
))

st.markdown("### So what is the answer?")
ui.cards([
    ("Not proven", "No finding in this project survives a properly corrected significance "
                   "test. Anyone claiming the data proves Arsenal bottled it is overreading it."),
    ("Not disproven either", "The sample is far too small to detect an effect of the size "
                             "football actually produces. These tests could not have found it "
                             "even if it were there."),
    ("What would settle it", "More seasons, not cleverer tests. The limit here is ten big "
                             "matches a year, and no amount of statistical technique creates "
                             "information that the data does not contain."),
])

st.page_link("pages/model_page.py", label="Next: what a model can predict instead",
             icon=":material/arrow_forward:")
