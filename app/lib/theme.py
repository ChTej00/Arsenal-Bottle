"""One visual system for the whole app: colour tokens, a Plotly template every
chart uses, and the CSS that turns Streamlit's defaults into something styled.

Import order matters slightly: call inject_css() once per page run, before any
content, and pass TEMPLATE to every figure via apply().
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
NAVY = "#063672"
GOLD = "#9C824A"
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

# Categorical ramp for charts that need many distinguishable series.
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


TEMPLATE = go.layout.Template(
    layout=dict(
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="rgba(0,0,0,0)",
        font=dict(family="Inter, system-ui, sans-serif", size=13, color=TEXT),
        title=dict(font=dict(size=15, color=TEXT), x=0, xanchor="left", pad=dict(b=14)),
        xaxis=dict(gridcolor=LINE, zerolinecolor=LINE, linecolor=LINE,
                   tickfont=dict(color=MUTED, size=12),
                   title=dict(font=dict(color=MUTED, size=12))),
        yaxis=dict(gridcolor=LINE, zerolinecolor=LINE, linecolor=LINE,
                   tickfont=dict(color=MUTED, size=12),
                   title=dict(font=dict(color=MUTED, size=12))),
        legend=dict(font=dict(color=MUTED, size=12), bgcolor="rgba(0,0,0,0)",
                    orientation="h", yanchor="bottom", y=1.02, xanchor="left", x=0),
        colorway=SEQUENCE,
        margin=dict(l=8, r=8, t=50, b=8),
        hoverlabel=dict(bgcolor=SURFACE_2, bordercolor=LINE,
                        font=dict(color=TEXT, family="Inter, system-ui, sans-serif")),
        separators=".,",
    )
)
pio.templates["arsenal"] = TEMPLATE


def apply(fig, height: int | None = None, legend: bool = True, **layout):
    """Every figure in the app goes through this, so nothing drifts."""
    fig.update_layout(template="arsenal", showlegend=legend, **layout)
    if height:
        fig.update_layout(height=height)
    return fig


CSS = """
<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&display=swap');

html, body, [class*="css"], [data-testid="stAppViewContainer"] {
    font-family: 'Inter', system-ui, sans-serif;
}

[data-testid="stHeader"] { background: transparent; }
[data-testid="stToolbar"] { right: 1rem; }
[data-testid="stAppDeployButton"], [data-testid="stStatusWidget"] { display: none !important; }
footer, #MainMenu { visibility: hidden; }

.block-container { padding-top: 2.6rem; padding-bottom: 5rem; max-width: 1180px; }

h1, h2, h3 { letter-spacing: -0.02em; font-weight: 650; }
h1 { font-size: 2.1rem !important; }
h2 { font-size: 1.45rem !important; margin-top: 2.2rem !important; }
h3 { font-size: 1.1rem !important; color: #E8EAED; }

a { color: #EF0107 !important; text-decoration: none; }
a:hover { text-decoration: underline; }

/* Hero */
.hero { padding: 0.5rem 0 1.5rem 0; border-bottom: 1px solid #252C38; margin-bottom: 1.6rem; }
.hero .eyebrow {
    font-size: 0.72rem; letter-spacing: 0.16em; text-transform: uppercase;
    color: #EF0107; font-weight: 600; margin-bottom: 0.7rem;
}
.hero h1 {
    font-family: 'Instrument Serif', Georgia, serif !important;
    font-size: 3.4rem !important; font-weight: 400 !important;
    line-height: 1.04; letter-spacing: -0.015em; margin: 0 0 0.7rem 0 !important;
}
.hero .sub { color: #8A94A3; font-size: 1.02rem; max-width: 40rem; line-height: 1.6; }

/* Page intro used on every non-landing page */
.pageintro { color: #8A94A3; font-size: 0.98rem; line-height: 1.65; max-width: 44rem;
             margin-bottom: 0.4rem; }

/* Stat strip */
.statgrid {
    display: grid; grid-template-columns: repeat(auto-fit, minmax(125px, 1fr));
    gap: 0.6rem; margin: 1.1rem 0 0.4rem 0;
}
.stat {
    background: #151A22; border: 1px solid #252C38;
    border-radius: 10px; padding: 0.85rem 0.95rem;
}
.stat .v { font-size: 1.55rem; font-weight: 680; letter-spacing: -0.02em; line-height: 1.15; }
.stat .k { font-size: 0.72rem; color: #8A94A3; text-transform: uppercase;
           letter-spacing: 0.08em; margin-top: 0.25rem; }

/* Finding cards */
.cardgrid {
    display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
    gap: 0.7rem; margin: 0.6rem 0 0.2rem 0;
}
.card {
    background: #151A22; border: 1px solid #252C38;
    border-left: 3px solid #EF0107; border-radius: 10px; padding: 1rem 1.1rem;
}
.card h4 { margin: 0 0 0.45rem 0; font-size: 0.95rem; font-weight: 640; color: #E8EAED; }
.card p { margin: 0; font-size: 0.87rem; color: #8A94A3; line-height: 1.55; }

/* Chart caption, the plain-English verdict under every figure */
.verdict {
    border-left: 2px solid #EF0107; padding: 0.15rem 0 0.15rem 0.8rem;
    color: #C3C9D2; font-size: 0.89rem; line-height: 1.6; margin: -0.4rem 0 1.5rem 0;
}
.verdict b { color: #E8EAED; font-weight: 600; }

/* Callouts */
.note {
    background: #151A22; border: 1px solid #252C38; border-radius: 10px;
    padding: 0.85rem 1rem; color: #8A94A3; font-size: 0.87rem; line-height: 1.6;
    margin: 0.4rem 0 1.4rem 0;
}
.note.warn { border-color: #5A3B1E; background: #1B140B; color: #D6B182; }
.note.flag { border-color: #5A2224; background: #1B0E0F; color: #E0A0A2; }
.note b { color: #E8EAED; }
.note.warn b { color: #FFCC8A; }
.note.flag b { color: #F5B7B9; }

/* Tables */
[data-testid="stDataFrame"] { border: 1px solid #252C38; border-radius: 10px; }

/* Expanders */
[data-testid="stExpander"] { border: 1px solid #252C38; border-radius: 10px;
                             background: #10141B; }
[data-testid="stExpander"] summary { font-size: 0.86rem; color: #8A94A3; }

/* Metric */
[data-testid="stMetric"] {
    background: #151A22; border: 1px solid #252C38; border-radius: 10px;
    padding: 0.8rem 0.95rem;
}
[data-testid="stMetricLabel"] { color: #8A94A3; }

/* Sidebar */
[data-testid="stSidebar"] { background: #0D1117; border-right: 1px solid #252C38; }
[data-testid="stSidebarNav"] { padding-top: 0.5rem; }

hr { border-color: #252C38; margin: 2.2rem 0 1.4rem 0; }

@media (max-width: 640px) {
    .hero h1 { font-size: 2.3rem !important; }
    .block-container { padding-left: 1rem; padding-right: 1rem; }
}
</style>
"""


def inject_css() -> None:
    st.markdown(CSS, unsafe_allow_html=True)
