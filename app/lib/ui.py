"""Reusable page furniture, so every page looks the same without repeating HTML.

The convention the whole app follows: a chart is never left to speak for itself.
Each one gets a verdict() underneath saying in plain English what it shows, and
where the computation is not obvious, a method() expander saying how it was
worked out. That split is what lets a casual reader and a technical reader both
get what they came for.
"""
import html

import streamlit as st

from . import theme


def page_header(title: str, intro: str) -> None:
    theme.inject_css()
    st.markdown(f"## {title}")
    st.markdown(f'<div class="pageintro">{intro}</div>', unsafe_allow_html=True)


def hero(eyebrow: str, title: str, sub: str) -> None:
    st.markdown(
        f'<div class="hero"><div class="eyebrow">{eyebrow}</div>'
        f"<h1>{title}</h1><div class='sub'>{sub}</div></div>",
        unsafe_allow_html=True,
    )


def stats(items: list) -> None:
    """items: list of (value, label). Renders as one compact strip."""
    cells = "".join(
        f'<div class="stat"><div class="v">{html.escape(str(v))}</div>'
        f'<div class="k">{html.escape(str(k))}</div></div>'
        for v, k in items
    )
    st.markdown(f'<div class="statgrid">{cells}</div>', unsafe_allow_html=True)


def cards(items: list) -> None:
    """items: list of (heading, body)."""
    cells = "".join(
        f'<div class="card"><h4>{html.escape(h)}</h4><p>{b}</p></div>'
        for h, b in items
    )
    st.markdown(f'<div class="cardgrid">{cells}</div>', unsafe_allow_html=True)


def verdict(text: str) -> None:
    """The plain-English reading of the chart directly above."""
    st.markdown(f'<div class="verdict">{text}</div>', unsafe_allow_html=True)


def note(text: str, kind: str = "") -> None:
    cls = f"note {kind}".strip()
    st.markdown(f'<div class="{cls}">{text}</div>', unsafe_allow_html=True)


def method(text: str, label: str = "How this was computed") -> None:
    with st.expander(label):
        st.markdown(text)


def chart(fig, verdict_text: str | None = None, method_text: str | None = None,
          key: str | None = None) -> None:
    """The standard unit of this app: figure, then its plain reading, then
    optionally the method behind it."""
    st.plotly_chart(fig, width="stretch", key=key)
    if verdict_text:
        verdict(verdict_text)
    if method_text:
        method(method_text)


def updated_banner(state: dict) -> None:
    st.caption(
        f"Live data updated after gameweek {state['last_published_gameweek']} of 2026-27 · "
        f"last checked {state['updated_at'][:10]} · model trained on "
        f"{state['training_rows']:,} matches"
    )
