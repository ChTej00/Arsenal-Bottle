"""Fitting and evaluating the match outcome model.

Mirrors notebooks/07_monte_carlo_simulation.ipynb Section F/G: one multinomial
logistic regression, StandardScaler'd inputs, recency_weight as sample_weight.
"""
import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.dummy import DummyClassifier
from sklearn.metrics import log_loss, accuracy_score

from . import config


@dataclass
class FittedModel:
    model: LogisticRegression
    scaler: StandardScaler
    features: list


def prepare_rows(df: pd.DataFrame, features=None) -> pd.DataFrame:
    """Rows with a real result and no missing feature, plus the y label.
    Matches the notebook's model_df/prod_df construction."""
    features = features or config.FEATURES_FINAL
    out = df.dropna(subset=features + ["result"]).copy()
    out["y"] = out["result"].map(config.Y_MAP)
    return out


def fit(train_df: pd.DataFrame, features=None) -> FittedModel:
    features = features or config.FEATURES_FINAL
    scaler = StandardScaler()
    X = scaler.fit_transform(train_df[features])
    model = LogisticRegression(max_iter=2000)
    model.fit(X, train_df["y"], sample_weight=train_df["recency_weight"])
    return FittedModel(model=model, scaler=scaler, features=features)


def evaluate(fitted: FittedModel, test_df: pd.DataFrame) -> dict:
    X = fitted.scaler.transform(test_df[fitted.features])
    proba = fitted.model.predict_proba(X)
    preds = fitted.model.predict(X)
    return {
        "log_loss": log_loss(test_df["y"], proba, labels=[0, 1, 2]),
        "accuracy": accuracy_score(test_df["y"], preds),
        "n": len(test_df),
    }


def save(fitted: FittedModel, path: Path, meta: dict | None = None) -> None:
    """Save coefficients as plain JSON rather than a pickle, so the app never
    depends on the exact scikit-learn version that fit the model."""
    payload = {
        "features": fitted.features,
        "classes": fitted.model.classes_.tolist(),
        "coef": fitted.model.coef_.tolist(),
        "intercept": fitted.model.intercept_.tolist(),
        "scaler_mean": fitted.scaler.mean_.tolist(),
        "scaler_scale": fitted.scaler.scale_.tolist(),
        "meta": meta or {},
    }
    Path(path).write_text(json.dumps(payload, indent=2))


def load(path: Path) -> FittedModel:
    payload = json.loads(Path(path).read_text())
    features = payload["features"]

    scaler = StandardScaler()
    scaler.mean_ = np.array(payload["scaler_mean"])
    scaler.scale_ = np.array(payload["scaler_scale"])
    scaler.n_features_in_ = len(features)
    scaler.feature_names_in_ = np.array(features, dtype=object)

    model = LogisticRegression()
    model.classes_ = np.array(payload["classes"])
    model.coef_ = np.array(payload["coef"])
    model.intercept_ = np.array(payload["intercept"])
    model.n_features_in_ = len(features)

    return FittedModel(model=model, scaler=scaler, features=features)


def dummy_baseline(train_df: pd.DataFrame, test_df: pd.DataFrame, features=None) -> dict:
    features = features or config.FEATURES_FINAL
    dummy = DummyClassifier(strategy="prior").fit(train_df[features], train_df["y"])
    proba = dummy.predict_proba(test_df[features])
    preds = dummy.predict(test_df[features])
    return {
        "log_loss": log_loss(test_df["y"], proba, labels=[0, 1, 2]),
        "accuracy": accuracy_score(test_df["y"], preds),
    }
