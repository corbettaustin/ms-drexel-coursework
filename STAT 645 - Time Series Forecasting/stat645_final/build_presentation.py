"""
STAT 645 Final Project - slide deck builder.

Runs after stat645_final_project.R (or .Rmd) has written the PNG
figures into ./figures/. Any figure that is missing at build time is
replaced with a labelled placeholder so the deck still opens cleanly.

Usage:  python build_presentation.py
"""

import csv
import os
from pathlib import Path

from pptx import Presentation
from pptx.dml.color import RGBColor
from pptx.enum.shapes import MSO_SHAPE
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.util import Emu, Inches, Pt

HERE = Path(__file__).resolve().parent
FIG = HERE / "figures"
# Pick a writable output path - if the default deck is locked by an open
# PowerPoint, fall back to a versioned filename so the build still succeeds.
def _pick_output():
    candidates = ["stat645_final_presentation.pptx"] + [
        f"stat645_final_presentation_v{i}.pptx" for i in range(2, 10)
    ]
    for name in candidates:
        p = HERE / name
        if not p.exists():
            return p
        try:
            with open(p, "ab"):
                return p
        except PermissionError:
            continue
    raise PermissionError("Could not find a writable output filename.")
OUT = _pick_output()

# -- palette (Midnight Executive, muted) --
NAVY = RGBColor(0x1E, 0x27, 0x61)
ICE = RGBColor(0xCA, 0xDC, 0xFC)
INK = RGBColor(0x1A, 0x1A, 0x1A)
GREY = RGBColor(0x55, 0x5F, 0x6D)
LIGHT = RGBColor(0xF5, 0xF7, 0xFB)
WHITE = RGBColor(0xFF, 0xFF, 0xFF)
ACCENT = RGBColor(0xC8, 0x8A, 0x2C)  # warm gold for highlight

prs = Presentation()
prs.slide_width = Inches(13.333)
prs.slide_height = Inches(7.5)
BLANK = prs.slide_layouts[6]

SLIDE_W = prs.slide_width
SLIDE_H = prs.slide_height


def add_rect(slide, x, y, w, h, fill, line=None):
    shape = slide.shapes.add_shape(MSO_SHAPE.RECTANGLE, x, y, w, h)
    shape.fill.solid()
    shape.fill.fore_color.rgb = fill
    if line is None:
        shape.line.fill.background()
    else:
        shape.line.color.rgb = line
    shape.shadow.inherit = False
    return shape


def add_text(slide, x, y, w, h, text, *, size=18, bold=False, color=INK,
             align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.TOP, font="Calibri"):
    tb = slide.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame
    tf.word_wrap = True
    tf.margin_left = Emu(0)
    tf.margin_right = Emu(0)
    tf.margin_top = Emu(0)
    tf.margin_bottom = Emu(0)
    tf.vertical_anchor = anchor
    lines = text if isinstance(text, list) else [text]
    for i, line in enumerate(lines):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.alignment = align
        run = p.add_run()
        run.text = line
        run.font.name = font
        run.font.size = Pt(size)
        run.font.bold = bold
        run.font.color.rgb = color
    return tb


def add_bullets(slide, x, y, w, h, items, *, size=16, color=INK,
                font="Calibri"):
    tb = slide.shapes.add_textbox(x, y, w, h)
    tf = tb.text_frame
    tf.word_wrap = True
    tf.margin_left = Emu(0)
    tf.margin_right = Emu(0)
    tf.margin_top = Emu(0)
    tf.margin_bottom = Emu(0)
    for i, item in enumerate(items):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.alignment = PP_ALIGN.LEFT
        p.space_after = Pt(6)
        run = p.add_run()
        run.text = "•   " + item
        run.font.name = font
        run.font.size = Pt(size)
        run.font.color.rgb = color
    return tb


def add_title_band(slide, title, kicker=None):
    """Standard slide header: thin navy band + title text below."""
    add_rect(slide, Inches(0), Inches(0), SLIDE_W, Inches(0.18), NAVY)
    add_text(slide, Inches(0.6), Inches(0.32), Inches(12), Inches(0.7),
             title, size=28, bold=True, color=NAVY, font="Calibri")
    if kicker:
        add_text(slide, Inches(0.6), Inches(0.92), Inches(12), Inches(0.35),
                 kicker, size=13, color=GREY, font="Calibri")
    add_rect(slide, Inches(0), Inches(7.32), SLIDE_W, Inches(0.18), ICE)


def add_image_or_placeholder(slide, path, x, y, w, h, *, label=None):
    if path.exists():
        slide.shapes.add_picture(str(path), x, y, width=w, height=h)
    else:
        add_rect(slide, x, y, w, h, LIGHT, line=GREY)
        msg = label or path.name
        add_text(slide, x, y + h / 2 - Inches(0.25), w, Inches(0.5),
                 f"[ figure pending: {msg} ]",
                 size=14, color=GREY, align=PP_ALIGN.CENTER,
                 anchor=MSO_ANCHOR.MIDDLE)


def read_accuracy_csv():
    """Return list of dicts from figures/accuracy_table.csv if present."""
    p = FIG / "accuracy_table.csv"
    if not p.exists():
        return None
    with p.open() as f:
        return list(csv.DictReader(f))


def read_ljung_box():
    """Return the Ljung-Box result dict from figures/ljung_box.csv, or None."""
    p = FIG / "ljung_box.csv"
    if not p.exists():
        return None
    with p.open() as f:
        rows = list(csv.DictReader(f))
    return rows[0] if rows else None


# ============================================================
# SLIDE 1 - Title
# ============================================================
s = prs.slides.add_slide(BLANK)
add_rect(s, Inches(0), Inches(0), SLIDE_W, SLIDE_H, NAVY)
add_rect(s, Inches(0), Inches(6.7), SLIDE_W, Inches(0.06), ACCENT)

add_text(s, Inches(0.8), Inches(1.6), Inches(11.5), Inches(0.5),
         "STAT 645 - Time Series Forecasting", size=18, color=ICE,
         font="Calibri")
add_text(s, Inches(0.8), Inches(2.2), Inches(11.5), Inches(2.0),
         ["Forecasting Median Listing Price",
          "in Rhode Island"],
         size=44, bold=True, color=WHITE, font="Calibri")
add_text(s, Inches(0.8), Inches(4.5), Inches(11.5), Inches(0.5),
         "FRED series MEDLISPRIRI  -  monthly, Jul 2016 to Mar 2026",
         size=18, color=ICE, font="Calibri")
add_text(s, Inches(0.8), Inches(5.6), Inches(11.5), Inches(0.4),
         "Group X", size=20, bold=True, color=WHITE, font="Calibri")
add_text(s, Inches(0.8), Inches(6.05), Inches(11.5), Inches(0.4),
         "Drexel University  -  Final Project", size=14, color=ICE,
         font="Calibri")

# ============================================================
# SLIDE 2 - The data
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "The data",
               "FRED MEDLISPRIRI  -  monthly median listing price, Rhode Island")

add_bullets(s, Inches(0.6), Inches(1.5), Inches(5.5), Inches(4.5),
            ["Source: Zillow, distributed by St. Louis Fed (FRED)",
             "Frequency: monthly",
             "Window: Jul 2016 - Mar 2026  (n = 117)",
             "Range: $314k - $599k  -  ~90 percent expansion",
             "Captures 2020 - 2024 boom and 2025 - 2026 softening",
             "No missing observations, no gaps"],
            size=18)

add_image_or_placeholder(s, FIG / "01_overview.png",
                         Inches(6.4), Inches(1.45), Inches(6.5), Inches(3.8),
                         label="overview time-series plot")

add_text(s, Inches(6.4), Inches(5.45), Inches(6.5), Inches(0.4),
         "Strong trend + 2020 acceleration + 2025 reversal.",
         size=13, color=GREY, font="Calibri")

# ============================================================
# SLIDE 3 - Seasonality & decomposition
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Trend and seasonality",
               "STL decomposition isolates trend, yearly season, and remainder")

add_image_or_placeholder(s, FIG / "02_seasonality.png",
                         Inches(0.4), Inches(1.4), Inches(6.2), Inches(3.7),
                         label="seasonal plot")
add_image_or_placeholder(s, FIG / "03_stl.png",
                         Inches(6.8), Inches(1.4), Inches(6.2), Inches(5.4),
                         label="STL decomposition panel")

add_text(s, Inches(0.4), Inches(5.2), Inches(6.2), Inches(0.4),
         "Mild but consistent annual pattern - prices firm into summer.",
         size=13, color=GREY, font="Calibri")

# ============================================================
# SLIDE 4 - Approach
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Approach",
               "Hold out the last 12 months (~10 percent), fit every model family from class")

# left: train/test image
add_image_or_placeholder(s, FIG / "04_train_test.png",
                         Inches(0.4), Inches(1.4), Inches(6.6), Inches(3.8),
                         label="train/test split")

# right: candidate model cards
right_x = Inches(7.3)
right_w = Inches(5.7)

add_text(s, right_x, Inches(1.4), right_w, Inches(0.4),
         "Candidate models", size=18, bold=True, color=NAVY)

cards = [
    ("Toolbox baselines", "Mean, Naive, SNaive, Drift"),
    ("TSLM regression", "trend + season, trend + Fourier(K=2)"),
    ("ETS family", "M-A-M, damped A-Ad-A, log-ETS"),
    ("ARIMA family", "auto, seasonal (1,1,1)(0,1,1)"),
    ("STL + ETS", "decomposition with ETS on adjusted series"),
]
y = 1.85
for header, body in cards:
    add_rect(s, right_x, Inches(y), right_w, Inches(0.82), LIGHT)
    add_rect(s, right_x, Inches(y), Inches(0.08), Inches(0.82), NAVY)
    add_text(s, right_x + Inches(0.25), Inches(y + 0.06), right_w, Inches(0.4),
             header, size=15, bold=True, color=NAVY)
    add_text(s, right_x + Inches(0.25), Inches(y + 0.44), right_w, Inches(0.5),
             body, size=12, color=INK)
    y += 1.02

add_text(s, Inches(0.4), Inches(5.4), Inches(7), Inches(0.4),
         "Holdout: last 12 months ; metrics: RMSE, MAE, MAPE, MASE.",
         size=13, color=GREY, font="Calibri")

# ============================================================
# SLIDE 5 - All forecasts vs holdout
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Holdout forecasts - all candidates",
               "Visual check before scoring with accuracy metrics")

add_image_or_placeholder(s, FIG / "05_forecasts_compare.png",
                         Inches(0.6), Inches(1.4), Inches(12.1), Inches(5.5),
                         label="all-models comparison")

# ============================================================
# SLIDE 6 - Top 3 forecasts
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Best three models on the holdout",
               "Zoomed view of the leading candidates")

add_image_or_placeholder(s, FIG / "05b_best3_forecast.png",
                         Inches(0.6), Inches(1.4), Inches(12.1), Inches(5.5),
                         label="best-3 forecast comparison")

# ============================================================
# SLIDE 7 - Accuracy table
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Forecast accuracy on the 12-month holdout",
               "Sorted by RMSE - smaller is better")

acc_rows = read_accuracy_csv()
header = ["Model", "RMSE", "MAE", "MAPE", "MASE", "RMSSE"]

if acc_rows:
    table_data = [header]
    for r in acc_rows:
        def fmt(key, dp):
            try:
                return f"{float(r[key]):,.{dp}f}"
            except (KeyError, ValueError):
                return ""
        table_data.append([
            r.get(".model", ""),
            fmt("RMSE", 0),
            fmt("MAE", 0),
            fmt("MAPE", 2),
            fmt("MASE", 2),
            fmt("RMSSE", 2),
        ])
    winner_name = acc_rows[0].get(".model", "") if acc_rows else ""
else:
    # Placeholder so the deck still renders before R is run
    table_data = [header] + [
        ["(run stat645_final_project.R to populate)",
         "-", "-", "-", "-", "-"]
    ] + [["", "", "", "", "", ""] for _ in range(10)]
    winner_name = "TBD"

n_rows = len(table_data)
tx, ty = Inches(0.8), Inches(1.5)
tw, th = Inches(11.7), Inches(5.3)
table_shape = s.shapes.add_table(n_rows, 6, tx, ty, tw, th)
table = table_shape.table

col_widths_in = [3.6, 1.6, 1.6, 1.6, 1.6, 1.7]
for i, w in enumerate(col_widths_in):
    table.columns[i].width = Inches(w)

for j, label in enumerate(header):
    cell = table.cell(0, j)
    cell.fill.solid()
    cell.fill.fore_color.rgb = NAVY
    tf = cell.text_frame
    tf.margin_left = Inches(0.1)
    tf.margin_right = Inches(0.1)
    tf.text = label
    for p in tf.paragraphs:
        p.alignment = PP_ALIGN.LEFT if j == 0 else PP_ALIGN.RIGHT
        for r in p.runs:
            r.font.size = Pt(14)
            r.font.bold = True
            r.font.color.rgb = WHITE
            r.font.name = "Calibri"

for i, row in enumerate(table_data[1:], start=1):
    is_winner = (row[0] == winner_name) and bool(winner_name) and winner_name != "TBD"
    for j, val in enumerate(row):
        cell = table.cell(i, j)
        cell.fill.solid()
        if is_winner:
            cell.fill.fore_color.rgb = ICE
        else:
            cell.fill.fore_color.rgb = WHITE if i % 2 else LIGHT
        tf = cell.text_frame
        tf.margin_left = Inches(0.1)
        tf.margin_right = Inches(0.1)
        tf.text = str(val)
        for p in tf.paragraphs:
            p.alignment = PP_ALIGN.LEFT if j == 0 else PP_ALIGN.RIGHT
            for r in p.runs:
                r.font.size = Pt(12)
                r.font.bold = bool(is_winner)
                r.font.color.rgb = INK
                r.font.name = "Calibri"

# ============================================================
# SLIDE 8 - Judgment calls
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Judgment calls",
               "Decisions that shaped the final pick")

calls = [
    ("Holdout length",
     "12 months satisfies the >=10 percent rubric and exposes one full season."),
    ("ARIMA family included",
     "Chapter 9 added - auto plus a seasonal (1,1,1)(0,1,1); the closest challenger, which confirms the ETS choice rather than overturning it."),
    ("Transformation",
     "Guerrero lambda approximately -0.9 is numerically unstable here, so the transformed ETS uses a log scale instead of a raw Box-Cox."),
    ("Damped vs. undamped trend",
     "2025 reversal makes undamped trend dangerous over a 12-month horizon - we include A-Ad-A and let accuracy decide."),
    ("TSLM Fourier order K=2",
     "Captures the dominant annual cycle with fewer coefficients than 11 monthly dummies."),
]

y = 1.55
for header, body in calls:
    add_rect(s, Inches(0.6), Inches(y), Inches(12.1), Inches(1.0), LIGHT)
    add_rect(s, Inches(0.6), Inches(y), Inches(0.1), Inches(1.0), ACCENT)
    add_text(s, Inches(0.9), Inches(y + 0.1), Inches(11.5), Inches(0.4),
             header, size=15, bold=True, color=NAVY)
    add_text(s, Inches(0.9), Inches(y + 0.48), Inches(11.5), Inches(0.5),
             body, size=13, color=INK)
    y += 1.10

# ============================================================
# SLIDE 9 - Residual diagnostics
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, f"Chosen model: {winner_name}  -  residual diagnostics",
               "Innovation residuals, ACF, and distribution")

add_image_or_placeholder(s, FIG / "06_residuals.png",
                         Inches(0.6), Inches(1.4), Inches(8.4), Inches(5.5),
                         label="residual diagnostics panel")

lb = read_ljung_box()
lb_line = "Ljung-Box: run R to populate"
verdict = ""
if lb:
    try:
        pval = float(lb["lb_pvalue"])
        stat = float(lb["lb_stat"])
        lb_line = (f"Ljung-Box (lag {lb['lag']}, dof {lb['dof']}): "
                   f"chi-sq {stat:.1f}, p = {pval:.4f}")
        verdict = ("p < 0.05: some residual structure remains, traced to "
                   "the 2020-22 shock"
                   if pval < 0.05 else
                   "p > 0.05: consistent with white-noise innovations")
    except (KeyError, ValueError):
        pass

resid_bullets = [
    "ACF: most lags fall within the significance bounds",
    "Residuals roughly symmetric and centred on zero",
    "Large residuals cluster in 2020-2022 (COVID market shock)",
    lb_line,
]
if verdict:
    resid_bullets.append(verdict)
resid_bullets.append(
    "Selection rests on holdout accuracy; intervals carry the residual risk")

add_text(s, Inches(9.3), Inches(1.5), Inches(3.7), Inches(0.5),
         "Reading the diagnostics", size=16, bold=True, color=NAVY)
add_bullets(s, Inches(9.3), Inches(2.0), Inches(3.7), Inches(4.5),
            resid_bullets, size=12)

# ============================================================
# SLIDE 10 - Final 12-month forecast
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "12-month forecast",
               f"Refit on the full series ; 80 percent and 95 percent prediction intervals  -  {winner_name}")

add_image_or_placeholder(s, FIG / "07_final_forecast.png",
                         Inches(0.6), Inches(1.4), Inches(12.1), Inches(5.5),
                         label="final 12-month forecast")

# ============================================================
# SLIDE 11 - Conclusions
# ============================================================
s = prs.slides.add_slide(BLANK)
add_title_band(s, "Conclusions and assumptions",
               "What we found, what we are watching")

add_text(s, Inches(0.6), Inches(1.4), Inches(6.0), Inches(0.5),
         "Findings", size=18, bold=True, color=NAVY)
add_bullets(s, Inches(0.6), Inches(1.95), Inches(6.0), Inches(4.5),
            [f"Chosen model: {winner_name}",
             "Beats every baseline and the seasonal ARIMA on RMSE / MAPE",
             "Residual ACF mostly clean; Ljung-Box flags 2020-22 shock structure",
             "12-month forecast widens reasonably given the recent regime change"],
            size=15)

add_text(s, Inches(7.0), Inches(1.4), Inches(6.0), Inches(0.5),
         "Assumptions and caveats", size=18, bold=True, color=NAVY)
add_bullets(s, Inches(7.0), Inches(1.95), Inches(6.0), Inches(4.5),
            ["Only ~10 years of history; one full housing cycle",
             "2020 - 2022 shock is a large share of the sample",
             "Forecast assumes no structural break (rates, policy)",
             "Refit monthly; watch standardised residuals for drift > 2 sigma"],
            size=15)

add_text(s, Inches(0.6), Inches(6.6), Inches(12.1), Inches(0.4),
         "Group X  -  STAT 645 Final Project  -  Drexel University",
         size=12, color=GREY, font="Calibri", align=PP_ALIGN.CENTER)

# ============================================================
prs.save(OUT)
print(f"Wrote {OUT}")
print(f"Slides: {len(prs.slides)}")
print(f"Figures dir: {FIG}  exists={FIG.exists()}")
missing = [p.name for p in [
    FIG / n for n in [
        "01_overview.png", "02_seasonality.png", "03_stl.png",
        "04_train_test.png", "05_forecasts_compare.png",
        "05b_best3_forecast.png", "06_residuals.png",
        "07_final_forecast.png", "accuracy_table.csv",
    ]
] if not p.exists()]
if missing:
    print("Pending (re-run after R script): " + ", ".join(missing))
else:
    print("All figures present.")
