"""Page furniture, built on native Streamlit components.

The convention every page follows: a chart is never left to speak for itself.
ui.chart() renders the title as a real page heading, then the figure, then a
plain-English reading, then optionally a collapsed note on how it was computed.
That split lets a casual reader and a technical reader use the same page.
"""
import streamlit as st

from . import theme


def page_header(title: str, intro: str) -> None:
    theme.inject_css()
    st.header(title, anchor=False)
    st.markdown(intro)
    st.write("")


def hero(eyebrow: str, title: str, sub: str) -> None:
    theme.inject_css()
    st.markdown(f'<div class="eyebrow">{eyebrow}</div>'
                f'<div class="display-title">{title}</div>', unsafe_allow_html=True)
    st.markdown(sub)


def stats(items: list) -> None:
    """items: list of (value, label), optionally (value, label, help).

    Laid out as a wrapping flex row rather than fixed columns: six metrics
    forced into one row collapse to about 58px each on a narrow screen, which
    clips both the value and the label.
    """
    with st.container(horizontal=True, wrap=True, gap="small"):
        for item in items:
            value, label = item[0], item[1]
            helptext = item[2] if len(item) > 2 else None
            st.metric(label, value, border=True, help=helptext, width=160)


def cards(items: list) -> None:
    """items: list of (heading, body). One bordered column each."""
    for col, (heading, body) in zip(st.columns(len(items), gap="small"), items):
        with col.container(border=True):
            st.markdown(f"**{heading}**")
            st.markdown(f'<span style="color:{theme.MUTED};font-size:0.87rem;'
                        f'line-height:1.6">{body}</span>', unsafe_allow_html=True)


def verdict(text: str) -> None:
    """The plain-English reading of the chart directly above."""
    st.markdown(f'<div class="verdict">{text}</div>', unsafe_allow_html=True)


def note(text: str, kind: str = "info", icon: str | None = None) -> None:
    """Native Streamlit callout. kind: info, warn, error, success."""
    fn = {"info": st.info, "warn": st.warning, "error": st.error,
          "success": st.success}[kind]
    defaults = {"info": ":material/lightbulb:", "warn": ":material/warning:",
                "error": ":material/report:", "success": ":material/check_circle:"}
    fn(text, icon=icon or defaults[kind])


def method(text: str, label: str = "How this was computed") -> None:
    with st.expander(label, icon=":material/functions:"):
        st.markdown(text)


def chart(fig, title: str | None = None, verdict_text: str | None = None,
          method_text: str | None = None, key: str | None = None) -> None:
    """The standard unit of this app. The title is a page heading rather than a
    figure title, which is what keeps it clear of legends and subplot labels."""
    if title:
        st.markdown(f"##### {title}")
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


def prob_table(series, label: str, color: str = theme.RED, height="content"):
    """A probability column rendered as a native progress bar rather than a
    number, which reads far faster."""
    df = (series * 100).round(1).sort_values(ascending=False).rename(label).to_frame()
    st.dataframe(
        df, width="stretch", height=height,
        column_config={
            label: st.column_config.ProgressColumn(
                label, min_value=0, max_value=100, format="%.1f%%", color=color,
            )
        },
    )
