"""Colour tokens, the Plotly template every chart uses, and a small amount of CSS.

Deliberately light on CSS. Layout, cards, metrics, callouts and tables are all
native Streamlit components (see lib/ui.py); the stylesheet here only does the
things Streamlit has no API for: web fonts, hiding dev chrome, page width, and
heading rhythm.

Chart titles are NOT set on the figure. They are rendered above the chart by
ui.chart() as real page headings, which keeps them out of the plot area (where
they used to collide with legends and subplot labels) and lets them be found by
search and screen readers.
"""
import plotly.graph_objects as go
import plotly.io as pio
import streamlit as st

INK = "#0B0E13"
SURFACE = "#151A22"
SURFACE_2 = "#1C222C"
LINE = "#252C38"
TEXT = "#E8EAED"
MUTED = "#8A94A3"
FAINT = "#5A6472"

RED = "#EF0107"
NAVY = "#4C7DBF"
GOLD = "#C0A263"
TEAL = "#00B2A9"
SKY = "#6CABDD"
AMBER = "#FFB81C"

POSITIVE = "#18A999"
NEGATIVE = "#E5484D"

TEAM_COLORS = {
    "Arsenal": RED,
    "Liverpool": TEAL,
    "Manchester City": SKY,
    "Manchester United": AMBER,
}
NEUTRAL = "#6B7480"

SEQUENCE = [RED, SKY, TEAL, AMBER, "#B084F5", "#5BC98B", "#F5A05A", "#7D8BF7"]

SEASON_LABELS = {
    "1718": "17-18", "1819": "18-19", "1920": "19-20", "2021": "20-21",
    "2122": "21-22", "2223": "22-23", "2324": "23-24", "2425": "24-25",
    "2526": "25-26", "2627": "26-27",
}


def team_color(team: str) -> str:
    return TEAM_COLORS.get(team, NEUTRAL)


def season_label(code) -> str:
    return SEASON_LABELS.get(str(code), str(code))


AXIS = dict(
    gridcolor=LINE, zerolinecolor=LINE, linecolor=LINE,
    tickfont=dict(color=MUTED, size=12),
    title=dict(font=dict(color=MUTED, size=12), standoff=10),
    # Without this, the tight margins below clip long category labels (club
    # names on a horizontal bar chart) straight into the plot area.
    automargin=True,
)

TEMPLATE = go.layout.Template(
    layout=dict(
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="rgba(0,0,0,0)",
        font=dict(family="Inter, system-ui, sans-serif", size=13, color=TEXT),
        xaxis=AXIS, yaxis=AXIS,
        legend=dict(
            font=dict(color=MUTED, size=12), bgcolor="rgba(0,0,0,0)",
            orientation="h", yanchor="bottom", y=1.0, xanchor="left", x=0,
        ),
        colorway=SEQUENCE,
        # Top margin leaves room for the legend strip only; the title lives in
        # the page, not the figure.
        margin=dict(l=4, r=4, t=44, b=4),
        hoverlabel=dict(bgcolor=SURFACE_2, bordercolor=LINE,
                        font=dict(color=TEXT, family="Inter, system-ui, sans-serif")),
        separators=".,",
    )
)
pio.templates["arsenal"] = TEMPLATE


def apply(fig, height: int | None = None, legend: bool = True, **layout):
    """Every figure goes through this so nothing drifts. Pass legend=False for
    single-series charts, which then lose the reserved top margin too."""
    fig.update_layout(template="arsenal", showlegend=legend, **layout)
    if not legend and "margin" not in layout:
        fig.update_layout(margin=dict(l=4, r=4, t=12, b=4))
    if height:
        fig.update_layout(height=height)
    return fig


def style_subplots(fig, title_size: int = 12, legend_below: bool = True):
    """Subplot titles arrive as annotations pinned to the top of each cell,
    which is exactly where the default top legend sits. On a subplot figure the
    legend goes underneath instead, and the axis styling has to be reapplied
    because make_subplots creates its own axes after the template is set."""
    fig.update_annotations(font=dict(size=title_size, color=TEXT))
    fig.update_xaxes(**{k: v for k, v in AXIS.items() if k != "title"})
    fig.update_yaxes(**{k: v for k, v in AXIS.items() if k != "title"})
    if legend_below:
        # Far enough down to clear the x-axis titles, which sit just under the
        # plot area and collide with a legend placed closer than about -0.25.
        fig.update_layout(
            legend=dict(orientation="h", yanchor="top", y=-0.28, xanchor="left", x=0),
            margin=dict(l=4, r=4, t=30, b=74),
        )
    return fig


CSS = """
<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&display=swap');

html, body, [data-testid="stAppViewContainer"], [class*="st-emotion"] {
    font-family: 'Inter', system-ui, sans-serif;
}

/* The rule above is broad enough to catch Streamlit's icon spans, which render
   ligatures and turn into literal text ("trending_up", "functions") without
   their own font. Every icon element carries "Icon" in its test id. */
[data-testid*="Icon"], [class*="material-symbols"], .material-symbols-rounded {
    font-family: 'Material Symbols Rounded' !important;
}

/* Dev chrome we never want a visitor to see */
[data-testid="stHeader"] { background: transparent; }
[data-testid="stAppDeployButton"], [data-testid="stStatusWidget"] { display: none !important; }
footer, #MainMenu { visibility: hidden; }

.block-container { padding-top: 2.4rem; padding-bottom: 5rem; max-width: 1160px; }

/* Heading rhythm. Streamlit's defaults are too tight above and too loose below. */
h1, h2, h3, h4, h5 { letter-spacing: -0.018em; }
h2 { font-size: 1.5rem !important; margin-top: 2.4rem !important; padding-bottom: 0 !important; }
h3 { font-size: 1.15rem !important; margin-top: 1.6rem !important; }
h5 { font-size: 0.95rem !important; font-weight: 600 !important; color: #E8EAED;
     margin: 0.6rem 0 0.1rem 0 !important; }

/* The one display face on the site */
.display-title {
    font-family: 'Instrument Serif', Georgia, serif;
    font-size: 3.5rem; font-weight: 400; line-height: 1.03;
    letter-spacing: -0.012em; margin: 0.2rem 0 0.8rem 0;
}
.eyebrow {
    font-size: 0.72rem; letter-spacing: 0.16em; text-transform: uppercase;
    color: #EF0107; font-weight: 600;
}

/* The plain-English reading under each chart */
.verdict {
    border-left: 2px solid #EF0107; padding: 0.1rem 0 0.1rem 0.85rem;
    color: #BFC6D0; font-size: 0.9rem; line-height: 1.65;
    margin: 0.2rem 0 0.6rem 0;
}
.verdict strong { color: #E8EAED; font-weight: 600; }

/* Native components, lightly tuned */
[data-testid="stMetric"] { padding: 0.75rem 0.9rem; }
[data-testid="stMetricValue"] { font-size: 1.5rem; letter-spacing: -0.02em; }
[data-testid="stMetricLabel"] p { font-size: 0.72rem !important; color: #8A94A3;
                                  text-transform: uppercase; letter-spacing: 0.04em; }
[data-testid="stSidebar"] { border-right: 1px solid #252C38; }

hr { border-color: #252C38; }

@media (max-width: 640px) {
    .display-title { font-size: 2.3rem; }
    .block-container { padding-left: 1rem; padding-right: 1rem; }
}
</style>
"""


def inject_css() -> None:
    st.markdown(CSS, unsafe_allow_html=True)
