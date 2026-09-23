"""Page furniture, built on native Streamlit components.

The convention every page follows: a chart is never left to speak for itself.
chart() renders the title as a page heading, then the figure, then a plain
reading, then optionally a collapsed note on how it was computed.

Callouts have exactly five kinds and one visual treatment each:

    definition  something the reader needs explained before the next chart
    caveat      a limit on what follows, stated before they trust it
    correction  something this project got wrong and fixed
    scope       what this page does and does not cover
    finding     a result given weight, usually the payoff of a section

The brief specified four. `finding` exists because three callouts on the
evidence and predictor pages are results, not warnings, and filing the
tie-free win-rate check under `caveat` would misrepresent the strongest
positive evidence on that page.
"""
import streamlit as st

from . import theme

# kind -> (streamlit renderer, default icon)
_CALLOUTS = {
    "definition": (st.info, ":material/school:"),
    "caveat": (st.warning, ":material/warning:"),
    "correction": (st.error, ":material/error:"),
    "scope": (st.info, ":material/crop_free:"),
    "finding": (st.success, ":material/check_circle:"),
}


# ---------------------------------------------------------------------------
# Page frame
# ---------------------------------------------------------------------------

def page_header(title: str, intro: str, eyebrow: str | None = None) -> None:
    """Serif display line over a red kicker, the same brand voice on every page."""
    theme.inject_css()
    if eyebrow:
        st.markdown(f'<div class="eyebrow">{eyebrow}</div>', unsafe_allow_html=True)
    st.markdown(f'<div class="display-title">{title}</div>', unsafe_allow_html=True)
    st.markdown(intro)
    st.write("")


def hero(eyebrow: str, title: str, sub: str) -> None:
    theme.inject_css()
    st.markdown(f'<div class="eyebrow">{eyebrow}</div>'
                f'<div class="display-title lg">{title}</div>', unsafe_allow_html=True)
    st.markdown(sub)


# ---------------------------------------------------------------------------
# Callouts
# ---------------------------------------------------------------------------

def callout(kind: str, title: str, body: str, icon: str | None = None) -> None:
    """One treatment per kind. Replaces every direct st.info/warning/error."""
    if kind not in _CALLOUTS:
        raise ValueError(f"unknown callout kind {kind!r}; "
                         f"expected one of {sorted(_CALLOUTS)}")
    render, default_icon = _CALLOUTS[kind]
    render(f"**{title}** {body}" if title else body, icon=icon or default_icon)


def note(text: str, kind: str = "info", icon: str | None = None) -> None:
    """Deprecated: use callout(). Kept so unconverted pages keep rendering
    while the migration runs page by page."""
    mapped = {"info": "definition", "warn": "caveat",
              "error": "correction", "success": "finding"}.get(kind, "definition")
    render, default_icon = _CALLOUTS[mapped]
    render(text, icon=icon or default_icon)


# ---------------------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------------------

def _row_size(n: int) -> int:
    """Row width that never leaves a row holding a single orphan card."""
    if n <= 4:
        return n
    return {5: 3, 6: 3, 7: 4, 8: 4, 9: 3, 10: 4, 11: 4, 12: 4}.get(n, 4)


def metric_grid(items: list) -> None:
    """items: (value, label) or (value, label, help) or (value, label, help, sub).

    Wraps so a row is never left with an orphan, and fixes one height for the
    whole grid so cards align whether or not they carry a sub-label. `sub`
    renders grey via delta_color="off"; st.metric's delta is a change
    indicator and must never be used to carry a subtitle, because it renders
    green as though the value had gone up.
    """
    if not items:
        return
    per_row = _row_size(len(items))
    has_sub = any(len(i) > 3 and i[3] for i in items)
    height = 132 if has_sub else 104

    for start in range(0, len(items), per_row):
        chunk = items[start:start + per_row]
        cols = st.columns(per_row, gap="small")
        for col, item in zip(cols, chunk):
            value, label = item[0], item[1]
            helptext = item[2] if len(item) > 2 else None
            sub = item[3] if len(item) > 3 else None
            col.metric(label, value, border=True, help=helptext, height=height,
                       delta=sub, delta_color="off" if sub else "normal")


def stats(items: list) -> None:
    """Deprecated: use metric_grid()."""
    metric_grid(items)


def cards(items: list) -> None:
    """items: (heading, body). Equal-height bordered columns."""
    cols = st.columns(len(items), gap="small")
    for col, (heading, body) in zip(cols, items):
        with col.container(border=True, height=230):
            st.markdown(f"**{heading}**")
            st.markdown(f'<span style="color:{theme.COLOR["muted"]};font-size:0.87rem;'
                        f'line-height:1.6">{body}</span>', unsafe_allow_html=True)


# ---------------------------------------------------------------------------
# Charts and tables
# ---------------------------------------------------------------------------

def verdict(text: str) -> None:
    st.markdown(f'<div class="verdict">{text}</div>', unsafe_allow_html=True)


def method(text: str, label: str = "How this was computed") -> None:
    with st.expander(label, icon=":material/functions:"):
        st.markdown(text)


def chart(fig, title: str | None = None, verdict_text: str | None = None,
          method_text: str | None = None, source_note: str | None = None,
          key: str | None = None) -> None:
    """The standard unit of this app. One title, rendered as a page heading
    rather than a figure title, which keeps it clear of legends and subplot
    labels. Modebar is always off."""
    if title:
        st.markdown(f"##### {title}")
    st.plotly_chart(fig, width="stretch", config=theme.PLOTLY_CONFIG, key=key)
    if source_note:
        st.markdown(f'<div class="source-note">{source_note}</div>',
                    unsafe_allow_html=True)
    if verdict_text:
        verdict(verdict_text)
    if method_text:
        method(method_text)


def bare_chart(fig, title: str | None = None, key: str | None = None) -> None:
    """A figure inside a column or tab, where the verdict belongs to the group
    rather than this one figure. Same title and modebar treatment."""
    if title:
        st.markdown(f"##### {title}")
    st.plotly_chart(fig, width="stretch", config=theme.PLOTLY_CONFIG, key=key)


def table(df, config: dict | None = None, rows: int | None = None,
          hide_index: bool = True) -> None:
    """One table treatment. Height is sized from the row count so a short table
    never gets its own scrollbar nested inside the page."""
    n = len(df) if rows is None else rows
    height = min(38 * n + 42, 560)
    st.dataframe(df, width="stretch", hide_index=hide_index,
                 height=height, column_config=config or {})


def prob_table(series, label: str, color: str | None = None) -> None:
    """A probability column as a native progress bar, which reads far faster
    than a number."""
    df = (series * 100).round(1).sort_values(ascending=False).rename(label).to_frame()
    table(df, hide_index=False, config={
        label: st.column_config.ProgressColumn(
            label, min_value=0, max_value=100, format="%.1f%%",
            color=color or theme.COLOR["brand"], width="medium",
        )
    })


# ---------------------------------------------------------------------------
# Navigation
# ---------------------------------------------------------------------------

STORY = [
    ("pages/rise.py", "Act 1 · The Rise"),
    ("pages/bottle.py", "Act 2 · The Bottle"),
    ("pages/breakthrough.py", "Act 3 · The Breakthrough"),
    ("pages/predictor.py", "Act 4 · The Predictor"),
]

ORDER = [
    ("pages/overview.py", "Overview"),
    ("pages/rise.py", "Act 1 · The Rise"),
    ("pages/bottle.py", "Act 2 · The Bottle"),
    ("pages/breakthrough.py", "Act 3 · The Breakthrough"),
    ("pages/compare.py", "Team comparator"),
    ("pages/significance.py", "Is it real?"),
    ("pages/model.py", "The model"),
    ("pages/predictor.py", "Act 4 · The Predictor"),
    ("pages/method.py", "Method & glossary"),
]


def stepper(current: str) -> None:
    """Act 1 to 4 progress, shown on the story pages."""
    cols = st.columns(len(STORY), gap="small")
    for col, (path, label) in zip(cols, STORY):
        act, name = label.split(" · ")
        if path == current:
            col.markdown(f"**{act}** · {name}")
        else:
            col.page_link(path, label=f"{act} · {name}")


def prev_next(current: str) -> None:
    """Both neighbours in reading order, so no page is a dead end."""
    paths = [p for p, _ in ORDER]
    if current not in paths:
        return
    i = paths.index(current)
    st.divider()
    left, right = st.columns(2, gap="small")
    if i > 0:
        left.page_link(ORDER[i - 1][0], label=f"Back to {ORDER[i - 1][1]}",
                       icon=":material/arrow_back:")
    if i < len(ORDER) - 1:
        with right:
            st.page_link(ORDER[i + 1][0], label=f"Next: {ORDER[i + 1][1]}",
                         icon=":material/arrow_forward:")


def updated_banner(state: dict) -> None:
    st.caption(
        f"Live data updated after gameweek {state['last_published_gameweek']} of 2026-27 · "
        f"last checked {state['updated_at'][:10]} · model trained on "
        f"{state['training_rows']:,} matches"
    )
