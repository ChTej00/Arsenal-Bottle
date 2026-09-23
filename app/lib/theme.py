"""The design system: semantic colour tokens, the Plotly template, and CSS.

Two rules make the palette readable:

1. **One colour, one meaning.** Semantic tokens describe a judgement (positive,
   negative, annotation). Club tokens describe a club. A chart uses one family
   or the other, never both, so red never means "Arsenal" and "bad" in the same
   figure.
2. **Club colours are chosen for legibility, not club identity.** Arsenal,
   Liverpool and Manchester United all play in red, so using real kit colours
   would make the four-club charts unreadable. Arsenal keeps red because it is
   the subject of the project; the other three get distinguishable hues. The
   Method page states this.

The semantic diverging pair is green/orange rather than red/green, so the
heatmaps are legible with the most common forms of colour blindness, and so
that neither end of the scale collides with Arsenal's brand red.
"""
import plotly.graph_objects as go
import plotly.io as pio
import streamlit as st

# ---------------------------------------------------------------------------
# Semantic tokens. Every colour used anywhere resolves to one of these.
# ---------------------------------------------------------------------------
COLOR = {
    "brand": "#EF0107",        # Arsenal, and brand accents. Never means "bad".
    "positive": "#2E9E6B",     # better than the baseline
    "negative": "#E0603C",     # worse than the baseline
    "neutral": "#6B7480",      # a club or series with no judgement attached
    "muted": "#8A94A3",        # secondary text, axis ticks
    "faint": "#5A6472",        # de-emphasised marks, reference series
    "annotation": "#C0A263",   # reference lines and callout marks on charts
    "grid": "#252C38",         # gridlines, borders
    "surface": "#151A22",      # card background
    "surface_alt": "#1C222C",  # hover cards, nested surfaces
    "ink": "#0B0E13",          # page background
    "text_primary": "#E8EAED",
    "text_secondary": "#8A94A3",
}

# Club colours. Legibility, not identity. See the module docstring.
CLUB = {
    "Arsenal": "#EF0107",            # red, the subject
    "Liverpool": "#00B2A9",          # teal
    "Manchester City": "#6CABDD",    # sky
    "Manchester United": "#FFB81C",  # amber
}

# Categorical ramp for charts needing many distinguishable series.
SEQUENCE = ["#EF0107", "#6CABDD", "#00B2A9", "#FFB81C",
            "#B084F5", "#5BC98B", "#F5A05A", "#7D8BF7"]

# Translucent brand, for area fills under a brand-coloured line.
BRAND_FILL = "rgba(239, 1, 7, 0.10)"

# One diverging scale for every "bad to good" figure on the site, so the two
# Act 2 heatmaps speak the same language. 0 is bad, 1 is good.
SCALE_DIVERGING = [
    [0.0, "#E0603C"],
    [0.5, "#252C38"],
    [1.0, "#2E9E6B"],
]
# Single-hue ramp for pure magnitude (counts, importances). Deliberately not
# red, so magnitude never reads as "Arsenal" or as "bad".
SCALE_SEQUENTIAL = [
    [0.0, "#151A22"],
    [0.5, "#33566F"],
    [1.0, "#6CABDD"],
]

# Readable names for the model's raw feature columns. The raw name stays
# available on hover, so the charts are still traceable back to the code.
FEATURE_LABELS = {
    "xG_roll5": "Chances created, last 5",
    "xGA_roll5": "Chances conceded, last 5",
    "pts_roll5": "Points per match, last 5",
    "win_rate_roll5": "Win rate, last 5",
    "is_home": "Playing at home",
    "is_big6_opp": "Big 6 opponent",
    "opp_xgd_roll5": "Opponent quality",
    "parity_gap": "How evenly matched",
    "stakes_intensity": "Pressure score",
    "is_non_big6_rivalry": "Derby, outside the Big 6",
    "h2h_pts_avg3": "Head-to-head record",
}


def feature_label(name: str) -> str:
    return FEATURE_LABELS.get(name, name)


SEASON_LABELS = {
    "1718": "17-18", "1819": "18-19", "1920": "19-20", "2021": "20-21",
    "2122": "21-22", "2223": "22-23", "2324": "23-24", "2425": "24-25",
    "2526": "25-26", "2627": "26-27",
}

# --- backward-compatible aliases -------------------------------------------
# Pages are migrated one at a time; these keep the unconverted ones working.
INK = COLOR["ink"]
SURFACE = COLOR["surface"]
SURFACE_2 = COLOR["surface_alt"]
LINE = COLOR["grid"]
TEXT = COLOR["text_primary"]
MUTED = COLOR["muted"]
FAINT = COLOR["faint"]
RED = COLOR["brand"]
POSITIVE = COLOR["positive"]
NEGATIVE = COLOR["negative"]
NEUTRAL = COLOR["neutral"]
GOLD = COLOR["annotation"]
NAVY = "#4C7DBF"
TEAL = CLUB["Liverpool"]
SKY = CLUB["Manchester City"]
AMBER = CLUB["Manchester United"]
TEAM_COLORS = CLUB


def team_color(team: str) -> str:
    return CLUB.get(team, COLOR["neutral"])


def season_label(code) -> str:
    return SEASON_LABELS.get(str(code), str(code))


def judgement_color(value: float, good_is_high: bool = True) -> str:
    """Positive/negative token for a signed value, so no page hard-codes it."""
    good = value > 0 if good_is_high else value < 0
    return COLOR["positive"] if good else COLOR["negative"]


# ---------------------------------------------------------------------------
# Plotly template. Registered as the default, so a figure is styled even if it
# never passes through apply().
# ---------------------------------------------------------------------------
AXIS = dict(
    gridcolor=COLOR["grid"], gridwidth=1,
    zerolinecolor=COLOR["grid"], linecolor=COLOR["grid"],
    tickfont=dict(color=COLOR["muted"], size=12),
    title=dict(font=dict(color=COLOR["muted"], size=12), standoff=12),
    # Expands the margin to fit tick and axis labels, so no title can clip
    # however tight the margins below are.
    automargin=True,
)

TEMPLATE = go.layout.Template(
    layout=dict(
        paper_bgcolor="rgba(0,0,0,0)",
        plot_bgcolor="rgba(0,0,0,0)",
        font=dict(family="Inter, system-ui, sans-serif", size=13,
                  color=COLOR["text_primary"]),
        xaxis=AXIS, yaxis=AXIS,
        legend=dict(
            font=dict(color=COLOR["muted"], size=12), bgcolor="rgba(0,0,0,0)",
            orientation="h", yanchor="bottom", y=1.0, xanchor="left", x=0,
        ),
        colorway=SEQUENCE,
        # Titles live in the page, not the figure, so the top margin only has
        # to clear the legend strip.
        margin=dict(l=8, r=8, t=46, b=8),
        hoverlabel=dict(bgcolor=COLOR["surface_alt"], bordercolor=COLOR["grid"],
                        font=dict(color=COLOR["text_primary"],
                                  family="Inter, system-ui, sans-serif")),
        bargap=0.28, bargroupgap=0.08,
        separators=".,",
    )
)
pio.templates["arsenal"] = TEMPLATE
pio.templates.default = "arsenal"

# Passed to st.plotly_chart everywhere. Kills the modebar.
PLOTLY_CONFIG = {"displayModeBar": False, "displaylogo": False,
                 "staticPlot": False, "scrollZoom": False}


def apply(fig, height: int | None = None, legend: bool = True, **layout):
    """Every figure goes through this. legend=False also reclaims the top
    margin that would otherwise be reserved for the legend strip."""
    fig.update_layout(template="arsenal", showlegend=legend, **layout)
    if not legend and "margin" not in layout:
        fig.update_layout(margin=dict(l=8, r=8, t=12, b=8))
    if height:
        fig.update_layout(height=height)
    return fig


def style_subplots(fig, title_size: int = 12, legend_below: bool = True):
    """Subplot titles arrive as annotations pinned to the top of each cell,
    which is where the default top legend sits. On a subplot figure the legend
    moves underneath, far enough down to clear the x-axis titles. Either way the
    top margin has to be deep enough to hold the titles: apply(legend=False)
    reclaims it down to 12px, which cuts the top off every subplot title."""
    fig.update_annotations(font=dict(size=title_size, color=COLOR["text_primary"]))
    fig.update_xaxes(**{k: v for k, v in AXIS.items() if k != "title"})
    fig.update_yaxes(**{k: v for k, v in AXIS.items() if k != "title"})
    if legend_below:
        fig.update_layout(
            legend=dict(orientation="h", yanchor="top", y=-0.28, xanchor="left", x=0),
            margin=dict(l=8, r=8, t=30, b=74),
        )
    elif (fig.layout.margin.t or 0) < 30:
        fig.update_layout(margin_t=30)
    return fig


def annotate(fig, x, y, text: str, ax: int = 0, ay: int = -34, **kwargs):
    """One consistent annotation treatment across the site."""
    fig.add_annotation(
        x=x, y=y, text=text, ax=ax, ay=ay,
        showarrow=True, arrowhead=0, arrowwidth=1.2,
        arrowcolor=COLOR["annotation"],
        font=dict(color=COLOR["text_primary"], size=11),
        bgcolor=COLOR["surface"], bordercolor=COLOR["grid"], borderwidth=1,
        borderpad=5, opacity=0.96, **kwargs,
    )
    return fig


# ---------------------------------------------------------------------------
# CSS. Only what Streamlit has no API for: web fonts, dev chrome, page width,
# heading rhythm, and the two custom marks (display title, chart verdict).
# ---------------------------------------------------------------------------
CSS = """
<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&display=swap');

html, body, [data-testid="stAppViewContainer"], [class*="st-emotion"] {
    font-family: 'Inter', system-ui, sans-serif;
}

/* The rule above is broad enough to catch Streamlit's icon spans, which render
   ligatures and turn into literal text ("trending_up") without their own font.
   Every icon element carries "Icon" in its test id. */
[data-testid*="Icon"], [class*="material-symbols"], .material-symbols-rounded {
    font-family: 'Material Symbols Rounded' !important;
}

/* Dev chrome a visitor should never see. toolbarMode="minimal" in config.toml
   handles the rest. */
[data-testid="stHeader"] { background: transparent; }
[data-testid="stAppDeployButton"], [data-testid="stStatusWidget"] { display: none !important; }
footer, #MainMenu { visibility: hidden; }

.block-container { padding-top: 2.4rem; padding-bottom: 5rem; max-width: 1160px; }

h1, h2, h3, h4, h5 { letter-spacing: -0.018em; }
h2 { font-size: 1.5rem !important; margin-top: 2.4rem !important; padding-bottom: 0 !important; }
h3 { font-size: 1.15rem !important; margin-top: 1.6rem !important; }
h5 { font-size: 0.95rem !important; font-weight: 600 !important; color: #E8EAED;
     margin: 0.6rem 0 0.1rem 0 !important; }

/* The brand voice: a serif display line over a red kicker, on every page. */
.display-title {
    font-family: 'Instrument Serif', Georgia, serif;
    font-size: 2.9rem; font-weight: 400; line-height: 1.05;
    letter-spacing: -0.012em; margin: 0.2rem 0 0.7rem 0;
}
.display-title.lg { font-size: 3.5rem; }
.eyebrow {
    font-size: 0.72rem; letter-spacing: 0.16em; text-transform: uppercase;
    color: #EF0107; font-weight: 600;
}

/* The plain-English reading under each chart. */
.verdict {
    border-left: 2px solid #EF0107; padding: 0.1rem 0 0.1rem 0.85rem;
    color: #BFC6D0; font-size: 0.9rem; line-height: 1.65;
    margin: 0.2rem 0 0.6rem 0;
}
.verdict strong { color: #E8EAED; font-weight: 600; }

.source-note { color: #5A6472; font-size: 0.78rem; margin: -0.2rem 0 1rem 0; }

[data-testid="stMetric"] { padding: 0.75rem 0.9rem; }
[data-testid="stMetricValue"] { font-size: 1.45rem; letter-spacing: -0.02em; }
[data-testid="stMetricLabel"] p { font-size: 0.72rem !important; color: #8A94A3;
                                  text-transform: uppercase; letter-spacing: 0.04em; }
[data-testid="stSidebar"] { border-right: 1px solid #252C38; }

hr { border-color: #252C38; }

@media (max-width: 640px) {
    .display-title, .display-title.lg { font-size: 2.2rem; }
    .block-container { padding-left: 1rem; padding-right: 1rem; }
}
</style>
"""


def inject_css() -> None:
    st.markdown(CSS, unsafe_allow_html=True)
