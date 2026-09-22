"""The model behind the live predictor: what it is, how good it is, and what failed."""
import numpy as np
import plotly.graph_objects as go
import streamlit as st

from lib import loaders, theme, ui

ui.page_header(
    "The model",
    "The statistics could not settle the bottle question, so the project changes approach: "
    "instead of testing a hypothesis on ten matches a season, train a model on every match "
    "in the league and see what it learns. This page is that model, held to an honest standard.",
)

comparison = loaders.table("model_comparison.csv")
coefs = loaders.table("coefficients.csv")
shap_vals = loaders.table("shap.csv")
calib = loaders.table("calibration.csv")
confusion = loaders.table("confusion.csv")
ablation = loaders.table("ablation.csv")
walk = loaders.table("walk_forward.csv")
experiments = loaders.table("experiments.csv")

lr = comparison[comparison["model"] == "Logistic regression"].iloc[0]
dummy = comparison[comparison["model"] == "Always predict the base rate"].iloc[0]

ui.stats([
    ("Win / Draw / Loss", "what it predicts"),
    ("11", "features"),
    ("4,894", "training matches"),
    (f"{lr['log_loss']:.4f}", "log loss"),
    (f"{lr['accuracy']*100:.1f}%", "accuracy"),
    (f"{dummy['log_loss']:.4f}", "baseline to beat"),
])

ui.note(
    "<b>Why accuracy is the wrong headline.</b> A model that says 'home win, 40% confident' "
    "for every match can look accurate while being useless. What a simulation needs is "
    "well-calibrated probabilities, not confident guesses. <b>Log loss</b> measures that: it "
    "punishes a model for being confidently wrong far more than for being uncertain. Lower is "
    "better, and it is the number every decision on this page was made on."
)

# ---------------------------------------------------------------------------
st.markdown("### Four models, one identical test")

comp = comparison.sort_values("log_loss")
fig = go.Figure()
fig.add_trace(go.Bar(
    y=comp["model"], x=comp["log_loss"], orientation="h",
    marker_color=[theme.RED if m == "Logistic regression" else theme.FAINT
                  for m in comp["model"]],
    text=[f"{v:.4f}" for v in comp["log_loss"]], textposition="outside",
    textfont=dict(color=theme.TEXT, size=12),
    hovertemplate="%{y}<br>log loss %{x:.4f}<extra></extra>",
))
theme.apply(fig, height=330, legend=False,
            title="Log loss on the 2025-26 holdout season (lower is better)",
            xaxis=dict(title=dict(text="Log loss"),
                       range=[0.9, comp["log_loss"].max() * 1.06]))
ui.chart(fig, verdict_text=(
    "<b>The simplest model won.</b> Logistic regression beat both a random forest and XGBoost, "
    "which are far more flexible and are usually expected to win this kind of comparison. With "
    "roughly 4,900 training rows and eleven features, there is not enough data for that "
    "flexibility to pay for itself, and the tree models spend it on patterns that do not "
    "generalise."
), method_text=(
    "All four models are trained on seasons 2019-20 through 2024-25 and tested on 2025-26, a "
    "strictly forward-in-time split. Random k-fold cross-validation is never used anywhere in "
    "this project, because shuffling matches would let a model train on the future and test on "
    "the past. The baseline predicts the historical rate of wins, draws and losses for every "
    "match, using no information at all."
))

with st.expander("Full comparison, including accuracy and Brier score"):
    st.dataframe(
        comparison[["model", "log_loss", "accuracy", "brier", "note"]].rename(columns={
            "model": "Model", "log_loss": "Log loss", "accuracy": "Accuracy",
            "brier": "Brier score", "note": "What it is",
        }).style.format({"Log loss": "{:.4f}", "Accuracy": "{:.3f}", "Brier score": "{:.4f}"}),
        width="stretch", hide_index=True)

# ---------------------------------------------------------------------------
st.markdown("### Does it hold up across every season, not just one?")

fig = go.Figure()
labels = [theme.season_label(s) for s in walk["test_season"]]
fig.add_trace(go.Bar(name="The model", x=labels, y=walk["log_loss"],
                     marker_color=theme.RED,
                     hovertemplate="%{x}<br>log loss %{y:.4f}<extra></extra>"))
fig.add_trace(go.Bar(name="Baseline (no features)", x=labels, y=walk["dummy_log_loss"],
                     marker_color=theme.FAINT,
                     hovertemplate="%{x}<br>log loss %{y:.4f}<extra></extra>"))
theme.apply(fig, height=380, barmode="group",
            title="Retrained and retested at every season boundary",
            yaxis=dict(title=dict(text="Log loss"), range=[0.9, 1.15]))
n_beat = int(walk["beats_dummy"].sum())
ui.chart(fig, verdict_text=(
    f"Train on everything before a season, test on that season, repeat. The model beats the "
    f"no-information baseline in <b>{n_beat} of {len(walk)}</b> season boundaries tested, so "
    "the headline result is not one lucky split. It is a modest edge, consistently present."
))

# ---------------------------------------------------------------------------
st.markdown("### What the model actually pays attention to")

c1, c2 = st.columns(2)
with c1:
    win = coefs[coefs["outcome"] == "Win"].sort_values("coefficient")
    fig = go.Figure(go.Bar(
        y=win["feature"], x=win["coefficient"], orientation="h",
        marker_color=[theme.POSITIVE if v > 0 else theme.NEGATIVE for v in win["coefficient"]],
        customdata=np.stack([win["p_value"], win["description"]], axis=-1),
        hovertemplate=("%{y}<br>weight %{x:+.3f}<br>p = %{customdata[0]:.3f}"
                       "<br>%{customdata[1]}<extra></extra>"),
    ))
    fig.add_vline(x=0, line=dict(color=theme.MUTED, width=1))
    theme.apply(fig, height=420, legend=False,
                title="Weight on a home win", xaxis_title="Model weight")
    st.plotly_chart(fig, width="stretch")
with c2:
    sv = shap_vals.sort_values("mean_abs_shap")
    fig = go.Figure(go.Bar(
        y=sv["feature"], x=sv["mean_abs_shap"], orientation="h",
        marker_color=theme.SKY,
        customdata=sv["description"],
        hovertemplate="%{y}<br>importance %{x:.4f}<br>%{customdata}<extra></extra>",
    ))
    theme.apply(fig, height=420, legend=False,
                title="Importance to XGBoost (SHAP)", xaxis_title="Mean absolute SHAP value")
    st.plotly_chart(fig, width="stretch")

stakes_lin = coefs[(coefs["outcome"] == "Win") & (coefs["feature"] == "stakes_intensity")].iloc[0]
stakes_shap_rank = int(shap_vals.reset_index().query("feature == 'stakes_intensity'").index[0]) + 1
ui.verdict(
    "Both charts describe the same eleven features, and they disagree about the one that "
    f"matters most to this project. In the linear model, <b>the pressure metric is not "
    f"statistically significant</b> once opponent quality, home advantage and form are included "
    f"(p = {stakes_lin['p_value']:.3f}). To XGBoost it ranks "
    f"<b>{stakes_shap_rank}{'st' if stakes_shap_rank == 1 else 'th'} of 11</b> by importance. "
    "This is left as an open disagreement rather than resolved, because the two methods are "
    "measuring genuinely different things: one asks whether the relationship is linear and "
    "reliable, the other how much the model leans on the feature in practice."
)

# ---------------------------------------------------------------------------
st.markdown("### What each feature is worth")

abl = ablation[ablation["removed_feature"] != "(nothing removed)"].sort_values("log_loss_cost")
fig = go.Figure(go.Bar(
    y=abl["removed_feature"], x=abl["log_loss_cost"], orientation="h",
    marker_color=[theme.POSITIVE if v > 0 else theme.NEGATIVE for v in abl["log_loss_cost"]],
    customdata=abl["description"],
    hovertemplate="Removing %{y}<br>changes log loss by %{x:+.4f}<br>%{customdata}<extra></extra>",
))
fig.add_vline(x=0, line=dict(color=theme.MUTED, width=1))
theme.apply(fig, height=420, legend=False,
            title="Damage done by removing each feature",
            xaxis_title="Increase in log loss when this feature is dropped")
best = abl.iloc[-1]
ui.chart(fig, verdict_text=(
    f"Drop one feature, refit, retest. Bars to the right are features the model misses. "
    f"<b>{best['removed_feature']}</b> costs the most to lose. Features at or below zero are "
    "earning nothing, and a couple actively improve the model by their absence, which is a "
    "normal sign that eleven features is close to as many as this much data can support."
))

# ---------------------------------------------------------------------------
st.markdown("### Can you trust the probabilities?")

c1, c2 = st.columns([3, 2])
with c1:
    fig = go.Figure()
    fig.add_trace(go.Scatter(
        x=[0, 1], y=[0, 1], mode="lines", name="perfect calibration",
        line=dict(color=theme.MUTED, width=1, dash="dash"), hoverinfo="skip",
    ))
    fig.add_trace(go.Scatter(
        x=calib["predicted_probability"], y=calib["actual_frequency"],
        mode="markers+lines", name="this model",
        line=dict(color=theme.RED, width=2.5),
        marker=dict(size=calib["n_matches"] / calib["n_matches"].max() * 18 + 6,
                    color=theme.RED),
        customdata=calib["n_matches"],
        hovertemplate=("predicted %{x:.0%}<br>actually won %{y:.0%}"
                       "<br>%{customdata} matches<extra></extra>"),
    ))
    theme.apply(fig, height=400,
                title="When the model says 70%, does it win 70% of the time?",
                xaxis=dict(title=dict(text="Predicted win probability"), tickformat=".0%"),
                yaxis=dict(title=dict(text="Actually won"), tickformat=".0%"))
    st.plotly_chart(fig, width="stretch")
with c2:
    cm = confusion.pivot(index="actual", columns="predicted", values="count")
    cm = cm.reindex(index=["Win", "Draw", "Loss"], columns=["Win", "Draw", "Loss"])
    fig = go.Figure(go.Heatmap(
        z=cm.values, x=cm.columns, y=cm.index, colorscale="Reds", showscale=False,
        text=cm.values, texttemplate="%{text}",
        textfont=dict(size=15, color=theme.TEXT),
        hovertemplate="Actually %{y}, predicted %{x}: %{z}<extra></extra>",
    ))
    theme.apply(fig, height=400, legend=False, title="What it predicted against what happened",
                xaxis_title="Predicted", yaxis_title="Actual")
    st.plotly_chart(fig, width="stretch")

draws_called = int(confusion[(confusion["actual"] == "Draw") &
                             (confusion["predicted"] == "Draw")]["count"].iloc[0])
ui.verdict(
    "The calibration line tracking the diagonal is what matters for the simulation: when this "
    "model says 60%, roughly 60% of those matches really are won. The grid on the right shows "
    f"its blind spot. It correctly called <b>{draws_called} draws</b>. Draws are genuinely the "
    "hardest outcome to predict from form alone, and the next section is what happened when we "
    "tried to fix that."
)

# ---------------------------------------------------------------------------
st.markdown("### Fourteen things we tried, and the one that worked")

exp = experiments.copy()
exp["Result"] = np.where(exp["kept"], "Kept", "Rejected")
show = exp[["experiment", "notebook", "log_loss", "accuracy", "Result", "verdict"]].rename(
    columns={"experiment": "Idea", "notebook": "Stage", "log_loss": "Log loss",
             "accuracy": "Accuracy", "verdict": "What happened"})
st.dataframe(
    show.style.format({"Log loss": "{:.4f}", "Accuracy": "{:.4f}"}, na_rep="—")
        .map(lambda v: f"color: {theme.POSITIVE}" if v == "Kept" else f"color: {theme.MUTED}",
             subset=["Result"]),
    width="stretch", hide_index=True, height=520,
)
ui.verdict(
    "Every one of these was implemented and measured on the identical test set rather than "
    "argued about. <b>Only head-to-head record earned a place.</b> The most instructive failures "
    "are the ones aimed at the draw problem: forcing the model to predict more draws raised "
    "draw recall every time and made both log loss and accuracy worse every time. Rebalancing "
    "does not create information, it just moves confidence away from the calls the model can "
    "actually make."
)
ui.note(
    "These numbers are transcribed from the notebooks' own recorded runs, not recomputed here. "
    "Several of them take hours, and one involved roughly 1,100 model fits. Everything else on "
    "this page is recomputed from the raw data every time the exports are rebuilt."
)

st.markdown("### The honest summary")
ui.cards([
    ("It beats the baseline, consistently",
     "Log loss 1.0362 against 1.0984 for a model with no information, and it wins at every "
     "season boundary tested. That edge is real."),
    ("It is nowhere near solving football",
     "49% accuracy on a three-way outcome. Predicting individual matches is genuinely hard, "
     "and anyone claiming much better from public data is usually measuring something easier."),
    ("The ceiling is the data, not the method",
     "Fourteen documented attempts to improve it failed. Getting meaningfully further needs "
     "player-level data, injuries or betting odds, not a cleverer algorithm."),
])

st.page_link("pages/predictor.py", label="Next: point it at the season being played now",
             icon=":material/arrow_forward:")
