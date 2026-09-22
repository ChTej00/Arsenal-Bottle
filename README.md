# Arsenal Bottle

**[Live site → arsenal-bottle.streamlit.app](https://arsenal-bottle.streamlit.app/)**

Seven seasons of Premier League data pointed at one question: did Arsenal bottle the title,
and can the numbers actually prove it?

The short answer is no, and working out why not is the interesting part. This repository
contains the full analysis, the statistical testing that dismantles most of its own findings,
a match-outcome model, and a Monte Carlo simulator that predicts the season currently being
played. The site rebuilds itself after every gameweek with no manual steps.

---

## What this is

Arsenal led the Premier League for long stretches of three consecutive seasons (2022-23,
2023-24, 2024-25) and won none of them, then won the league in 2025-26. The popular
explanation is that they choke under pressure. This project tests that.

**What the data says:** Arsenal create chances at the same rate in their biggest matches as
their ordinary ones. What falls away is the points they take from those matches. The gap
between performance and results is real and it lines up exactly with the two seasons that get
called bottles.

**What the statistics say:** none of it survives a properly corrected significance test. Ten
high-pressure matches per season is far too small a sample. A power analysis shows the tests
would need an enormous effect (Cohen's d = 1.06) to detect anything reliably, and the effects
actually observed are between 0.05 and 0.19. The honest conclusion is not "the bottle is
fake", it is "this data cannot settle it either way."

**One finding was wrong, and it stayed on the site.** An earlier version reported that
high-pressure matches cluster among a team's best results at more than double the chance rate,
which would have meant the pressure metric was structurally biased. Rebuilding the analysis
showed it was an artifact of how tied results were sorted: points can only be 0, 1 or 3, so
"the best ten results" of a season is decided by tie-breaking, and a chronological tie-break
sorts late-season matches (which is when high-pressure matches happen) into the best bucket by
construction. Break ties randomly and the effect vanishes. The correction is documented
in the app rather than quietly deleted.

---

## The live predictor

The 2026-27 season is simulated 10,000 times from the current table. Every remaining fixture
gets win/draw/loss probabilities from the model, a random draw decides each one, and the final
table is counted up. Outputs are title, top-four and relegation probabilities plus a projected
points range for all 20 clubs.

It has a published track record. Replaying the completed 2025-26 season with a model that never
saw it:

| Frozen at | Champion's title probability | Clubs inside their 90% range |
|---|---|---|
| Gameweek 20 | 85.7% | 17 of 20 |
| Gameweek 5 | 29.7% | 14 of 20 |

Given half a season of form it works well. Given five matches it is weak, because the model
reads form from a five-match window that resets every August. That limitation is stated on the
site rather than hidden, and fixing it is the next piece of work.

---

## How it is built

```
notebooks/     seven analysis notebooks, the source of truth
src/           that logic as importable modules, hand-verified against the notebooks
data/app/      small committed tables, the only thing the hosted app reads
app/           the Streamlit site, nine pages
.github/       a daily job that republishes when a gameweek completes
```

**Pipeline.** Shot-level data from [Understat](https://understat.com) via `soccerdata`.
Features include rolling five-match form, a continuous opponent-quality measure, head-to-head
record, and a custom 0-to-1 pressure score built from league position, time of season, form
and rivalry.

**Model.** Multinomial logistic regression, 11 features, 4,894 training matches. It was
compared against a random forest, XGBoost and a no-information baseline on an identical
forward-in-time holdout, and won on log loss (1.0362 against a 1.0984 baseline). The simplest
model beating both tree models is a real result: there is not enough data for that flexibility
to pay for itself.

**Fourteen documented dead ends.** Class weighting, a two-stage draw classifier, a Poisson
goals model, hyperparameter tuning, match-level retraining, aggressive recency weighting, an
`is_promoted` flag, separate attack/defence terms, and a combined prototype involving roughly
1,100 model fits. Only head-to-head record earned a place. The negative results are on the
site because they are worth as much as the model that won.

**Automation.** A GitHub Actions job runs daily, checks whether the club with the fewest
matches played has played another one (which is what "a full gameweek finished" actually
means, and handles postponements gracefully), and only then refits, resimulates and commits.
Streamlit Cloud redeploys on that commit. Most days it does nothing.

---

## Guarding against leakage

Every rolling feature is shifted one match before averaging, so a match is never described
using its own result. Rolling windows are grouped by team and season, so May's form never
bleeds into August. Train/test splits are always forward in time and random k-fold is never
used. The backtest explicitly masks every result after its cutoff before computing any feature,
including head-to-head records.

One documented exception: the historical pressure score is computed from a league table that
includes the match being scored, while the simulation correctly uses the table before it.
Measured cost is about 0.001 log loss, so it was left alone and recorded rather than hidden.

---

## Verification

Every number on the site is recomputed from raw data by the same code that runs the live
pipeline. Two check suites run whenever the data is rebuilt and compare fresh output against
the project's recorded values, covering every p-value, the cross-team tests, the effect-size
analysis, named individual matches, holdout log loss and accuracy, the baseline, feature
ablation, training row counts, and both backtests including which specific clubs fell outside
their predicted range.

```bash
python -m src.exports         # analysis tables + 22 statistical checks
python -m src.model_exports   # model artifacts + 13 acceptance checks
python -m src.update          # live pipeline (only publishes if a gameweek completed)
```

---

## Running it locally

```bash
pip install -r requirements.txt
streamlit run app/streamlit_app.py
```

Python 3.11. The hosted app reads only the committed tables in `data/app/`, so it needs no
network access and no scraping at page load. Regenerating those tables from scratch needs the
raw scrapes, which are gitignored because they are reproducible.

---

## Limitations

The model predicts win, draw or loss and never a scoreline, so simulated ties on points are
broken at random rather than on goal difference. Each club's form is frozen at its current
value for the rest of the season. Newly promoted clubs have no Premier League history to read.
Early-season predictions are the weakest part of the system. The pressure analysis covers four
clubs (Arsenal, Liverpool, Manchester City, Manchester United); the other sixteen appear in the
predictor, which uses league-wide data, but were never analysed at that depth and the site says
so rather than implying otherwise.

Getting meaningfully better at predicting individual matches would need genuinely new
information (player-level data, injuries, betting odds), not a cleverer algorithm. Fourteen
documented attempts support that conclusion.

---

Data from [Understat](https://understat.com). Built with pandas, scikit-learn, Plotly and
Streamlit.
