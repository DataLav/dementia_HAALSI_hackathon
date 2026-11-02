

# HAALSI HACKATHON - QUESTION 5
# Hypertension, diabetes, and functional disability
#
# PART 1 OF 2
# Setup, supplied results, formatting, and forest plots
#
# IMPORTANT:
# Results are transcribed from the supplied R output.
# This script does not refit the models.

from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import font_manager

from docx import Document
from docx.shared import Inches, Pt
from docx.enum.text import WD_ALIGN_PARAGRAPH



# 1. PATHS AND OUTPUT FOLDER

BASE_DIR = Path(r"")
OUT_DIR = BASE_DIR / "HAALSI_APA_Outputs"

if not BASE_DIR.exists():
    raise FileNotFoundError(
        f"Project folder not found: {BASE_DIR}\n"
        "Please check the folder path."
    )

OUT_DIR.mkdir(parents=True, exist_ok=True)



# 2. VISUAL STYLE - FONT AND CHARACTER RENDERING FIX


# Use Arial if installed. Otherwise, use DejaVu Sans, which
# supports a broad range of characters and is bundled with
# Matplotlib in standard installations.

available_fonts = {
    font.name for font in font_manager.fontManager.ttflist
}

if "Arial" in available_fonts:
    PLOT_FONT = "Arial"
else:
    PLOT_FONT = "DejaVu Sans"

print(f"Matplotlib figure font: {PLOT_FONT}")

NAVY = "#18324B"
TEAL = "#168C86"
GREY = "#777777"
LIGHT_GREY = "#D5DADF"
BLACK = "#222222"

mpl.rcParams.update({
    # Fonts
    "font.family": "sans-serif",
    "font.sans-serif": [PLOT_FONT, "DejaVu Sans"],
    "font.size": 10,
    "axes.titlesize": 12,
    "axes.labelsize": 10,
    "xtick.labelsize": 9,
    "ytick.labelsize": 9,

    # Text rendering
    "axes.unicode_minus": False,
    "text.usetex": False,

    # Figure quality
    "figure.dpi": 150,
    "savefig.dpi": 600,
    "figure.facecolor": "white",
    "savefig.facecolor": "white",
    "axes.facecolor": "white",

    # Axes appearance
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.linewidth": 0.8,
    "axes.edgecolor": BLACK,
    "axes.labelcolor": BLACK,
    "xtick.color": BLACK,
    "ytick.color": BLACK,
    "text.color": BLACK,

    # Embed fonts in PDF and PostScript exports
    "pdf.fonttype": 42,
    "ps.fonttype": 42,

    # Preserve text as text in SVG exports
    "svg.fonttype": "none",
})


# 3. PARTICIPANT COUNTS

group_counts = pd.DataFrame({
    "Comorbidity group": [
        "Neither condition",
        "Hypertension only",
        "Diabetes only",
        "Both conditions",
        "Missing classification",
    ],
    "n": [1564, 2514, 117, 438, 426],
})

classified_n = group_counts.loc[
    group_counts["Comorbidity group"]
    != "Missing classification",
    "n",
].sum()

group_counts["Percentage"] = (
    group_counts["n"] / classified_n * 100
)

group_counts.loc[
    group_counts["Comorbidity group"]
    == "Missing classification",
    "Percentage",
] = np.nan



# 4. REGRESSION RESULTS FROM THE SUPPLIED R OUTPUT


# Values:
# Outcome, comorbidity group, adjusted OR, lower CI,
# upper CI, p-value

results = [
    ("Any ADL difficulty", "Hypertension only",
     1.001, 0.779, 1.294, 0.992),
    ("Any ADL difficulty", "Diabetes only",
     1.357, 0.679, 2.500, 0.355),
    ("Any ADL difficulty", "Both conditions",
     2.018, 1.422, 2.849, 0.000),

    ("Walking difficulty", "Hypertension only",
     1.049, 0.799, 1.387, 0.733),
    ("Walking difficulty", "Diabetes only",
     1.548, 0.750, 2.926, 0.204),
    ("Walking difficulty", "Both conditions",
     2.072, 1.424, 2.999, 0.000),

    ("Dressing difficulty", "Hypertension only",
     0.791, 0.518, 1.223, 0.284),
    ("Dressing difficulty", "Diabetes only",
     1.532, 0.514, 3.691, 0.387),
    ("Dressing difficulty", "Both conditions",
     1.827, 1.035, 3.165, 0.034),

    ("Bathing difficulty", "Hypertension only",
     0.728, 0.491, 1.089, 0.117),
    ("Bathing difficulty", "Diabetes only",
     1.529, 0.567, 3.470, 0.350),
    ("Bathing difficulty", "Both conditions",
     1.231, 0.687, 2.136, 0.471),

    ("Eating difficulty", "Hypertension only",
     0.633, 0.347, 1.165, 0.136),
    ("Eating difficulty", "Diabetes only",
     0.000, 0.000, 2717.725, 0.981),
    ("Eating difficulty", "Both conditions",
     0.606, 0.173, 1.654, 0.373),

    ("Toileting difficulty", "Hypertension only",
     1.129, 0.777, 1.664, 0.532),
    ("Toileting difficulty", "Diabetes only",
     0.778, 0.185, 2.209, 0.681),
    ("Toileting difficulty", "Both conditions",
     2.192, 1.322, 3.600, 0.002),
]

reg = pd.DataFrame(
    results,
    columns=[
        "Outcome",
        "Comorbidity group",
        "OR",
        "CI lower",
        "CI upper",
        "p",
    ],
)


# Interaction model result
interaction = {
    "Interaction": "Hypertension x diabetes",
    "OR": 1.485,
    "CI lower": 0.747,
    "CI upper": 3.153,
    "p": 0.278,
}


# Descriptive prevalence from the supplied R output
prevalence = pd.DataFrame({
    "Outcome": [
        "Walking difficulty",
        "Dressing difficulty",
        "Bathing difficulty",
        "Eating difficulty",
        "Toileting difficulty",
        "Any ADL difficulty",
    ],
    "Proportion": [
        0.077, 0.030, 0.034, 0.013, 0.040, 0.090
    ],
})

prevalence["Percentage"] = prevalence["Proportion"] * 100



# 5. FORMATTING HELPERS


def p_format(p):
    """Format p-values using APA-style conventions."""

    if pd.isna(p):
        return "-"

    if p < 0.001:
        return "< .001"

    return f"{p:.3f}".replace("0.", ".")


def format_or_ci(row):
    """Format odds ratio and 95% confidence interval."""

    return (
        f'{row["OR"]:.2f} '
        f'[{row["CI lower"]:.2f}, '
        f'{row["CI upper"]:.2f}]'
    )


def save_figure(fig, filename):
    """
    Save each figure as high-resolution PNG, PDF, and TIFF.
    Use consistent fonts, white backgrounds, and tight bounds.
    """

    # Apply layout before rendering
    fig.tight_layout()

    # Render the canvas before exporting
    fig.canvas.draw()

    # PNG
    fig.savefig(
        OUT_DIR / f"{filename}.png",
        dpi=600,
        bbox_inches="tight",
        facecolor="white",
        edgecolor="white",
    )

    # PDF
    fig.savefig(
        OUT_DIR / f"{filename}.pdf",
        bbox_inches="tight",
        facecolor="white",
        edgecolor="white",
    )

    # TIFF
    fig.savefig(
        OUT_DIR / f"{filename}.tiff",
        dpi=600,
        bbox_inches="tight",
        facecolor="white",
        edgecolor="white",
    )

    plt.close(fig)


# 6. FIGURE 1 - PRIMARY FOREST PLOT


primary = reg[
    reg["Outcome"] == "Any ADL difficulty"
].copy()

group_order = [
    "Hypertension only",
    "Diabetes only",
    "Both conditions",
]

primary["Comorbidity group"] = pd.Categorical(
    primary["Comorbidity group"],
    categories=group_order,
    ordered=True,
)

primary = primary.sort_values(
    "Comorbidity group"
).reset_index(drop=True)

fig, ax = plt.subplots(figsize=(9, 4.8))

for i, row in primary.iterrows():

    color = (
        TEAL
        if row["Comorbidity group"] == "Both conditions"
        else GREY
    )

    # Draw the odds ratio and confidence interval
    ax.errorbar(
        row["OR"],
        i,
        xerr=np.array([[
            row["OR"] - row["CI lower"]
        ], [
            row["CI upper"] - row["OR"]
        ]]),
        fmt="o",
        color=color,
        ecolor=color,
        markersize=7,
        capsize=4,
        elinewidth=1.7,
    )

    # Use an ordinary hyphen instead of a Unicode en dash
    label = (
        f'{row["OR"]:.2f} '
        f'({row["CI lower"]:.2f} - '
        f'{row["CI upper"]:.2f})'
    )

    ax.annotate(
        label,
        xy=(row["CI upper"], i),
        xytext=(8, 0),
        textcoords="offset points",
        va="center",
        fontsize=9,
        annotation_clip=False,
    )

# Reference line: OR = 1
ax.axvline(
    1,
    color=BLACK,
    linewidth=1,
    linestyle="--",
)

ax.set_xscale("log")
ax.set_xlim(0.4, 6.5)
ax.set_xticks([0.5, 1, 2, 4])
ax.set_xticklabels(["0.5", "1", "2", "4"])

ax.set_yticks(range(len(primary)))
ax.set_yticklabels(
    primary["Comorbidity group"].astype(str)
)
ax.invert_yaxis()

ax.set_xlabel("Adjusted odds ratio (log scale)")
ax.set_ylabel("")

ax.set_title(
    "Figure 1. Comorbidity and any ADL difficulty",
    loc="left",
    fontweight="bold",
    pad=15,
)

ax.text(
    0,
    -0.22,
    (
        "Note. Reference group = neither condition. "
        "Models adjusted for age, sex, education, and wealth. "
        "Points represent adjusted odds ratios; bars represent 95% CIs."
    ),
    transform=ax.transAxes,
    fontsize=8,
    va="top",
    wrap=True,
    clip_on=False,
)

ax.grid(
    axis="x",
    color=LIGHT_GREY,
    linewidth=0.6,
)

ax.set_axisbelow(True)

save_figure(
    fig,
    "Figure_1_Primary_Forest_Plot",
)



# 7. FIGURE 2 - ADL-SPECIFIC FOREST PLOT


both = reg[
    reg["Comorbidity group"] == "Both conditions"
].copy()

outcome_order = [
    "Any ADL difficulty",
    "Walking difficulty",
    "Dressing difficulty",
    "Bathing difficulty",
    "Eating difficulty",
    "Toileting difficulty",
]

both["Outcome"] = pd.Categorical(
    both["Outcome"],
    categories=outcome_order,
    ordered=True,
)

both = both.sort_values(
    "Outcome"
).reset_index(drop=True)

fig, ax = plt.subplots(figsize=(9, 6.2))

for i, row in both.iterrows():

    color = (
        TEAL
        if row["Outcome"] == "Any ADL difficulty"
        else NAVY
    )

    ax.errorbar(
        row["OR"],
        i,
        xerr=np.array([[
            row["OR"] - row["CI lower"]
        ], [
            row["CI upper"] - row["OR"]
        ]]),
        fmt="o",
        color=color,
        ecolor=color,
        markersize=6,
        capsize=3,
        elinewidth=1.5,
    )

    # Use plain ASCII punctuation in the figure labels
    label = (
        f'{row["OR"]:.2f} '
        f'({row["CI lower"]:.2f} - '
        f'{row["CI upper"]:.2f})'
    )

    ax.annotate(
        label,
        xy=(row["CI upper"], i),
        xytext=(8, 0),
        textcoords="offset points",
        va="center",
        fontsize=8,
        annotation_clip=False,
    )

ax.axvline(
    1,
    color=BLACK,
    linewidth=1,
    linestyle="--",
)

ax.set_xscale("log")
ax.set_xlim(0.4, 7.5)
ax.set_xticks([0.5, 1, 2, 4])
ax.set_xticklabels(["0.5", "1", "2", "4"])

ax.set_yticks(range(len(both)))
ax.set_yticklabels(
    both["Outcome"].astype(str)
)
ax.invert_yaxis()

ax.set_xlabel("Adjusted odds ratio (log scale)")
ax.set_ylabel("")

ax.set_title(
    "Figure 2. Comorbidity and individual ADL outcomes",
    loc="left",
    fontweight="bold",
    pad=15,
)

ax.text(
    0,
    -0.19,
    (
        "Note. Both conditions versus neither condition. "
        "Models adjusted for age, sex, education, and wealth. "
        "Secondary outcomes are exploratory; no multiple-testing "
        "correction was applied."
    ),
    transform=ax.transAxes,
    fontsize=8,
    va="top",
    wrap=True,
    clip_on=False,
)

ax.grid(
    axis="x",
    color=LIGHT_GREY,
    linewidth=0.6,
)

ax.set_axisbelow(True)

save_figure(
    fig,
    "Figure_2_ADL_Outcomes_Forest_Plot",
)




# HAALSI HACKATHON - QUESTION 5
# PART 2 OF 2
# APA tables, Excel workbook, Word document, and summary


# 8. TABLE 1 - PARTICIPANT DISTRIBUTION

table1 = group_counts.copy()

table1["n"] = table1["n"].map(
    lambda x: f"{x:,}"
)

table1["Percentage"] = table1["Percentage"].map(
    lambda x: "-" if pd.isna(x) else f"{x:.1f}%"
)

table1 = table1.rename(columns={
    "Comorbidity group": "Comorbidity group",
    "n": "Participants (n)",
    "Percentage": "Percentage",
})

print("\nTABLE 1: PARTICIPANT DISTRIBUTION")
print(table1.to_string(index=False))


# 9. TABLE 2 - PRIMARY REGRESSION RESULTS


primary_results = reg[
    reg["Outcome"] == "Any ADL difficulty"
].copy()

table2 = primary_results[
    [
        "Comorbidity group",
        "OR",
        "CI lower",
        "CI upper",
        "p",
    ]
].copy()

table2["Adjusted OR [95% CI]"] = table2.apply(
    format_or_ci,
    axis=1,
)

table2["p"] = table2["p"].apply(p_format)

table2 = table2[
    [
        "Comorbidity group",
        "Adjusted OR [95% CI]",
        "p",
    ]
]

reference_row = pd.DataFrame([{
    "Comorbidity group": "Neither condition (reference)",
    "Adjusted OR [95% CI]": "1.00 (reference)",
    "p": "-",
}])

table2 = pd.concat(
    [reference_row, table2],
    ignore_index=True,
)

print("\nTABLE 2: PRIMARY REGRESSION RESULTS")
print(table2.to_string(index=False))


# 10. TABLE 3 - ALL ADL OUTCOMES

table3 = reg.copy()

table3["Adjusted OR [95% CI]"] = table3.apply(
    format_or_ci,
    axis=1,
)

table3["p"] = table3["p"].apply(p_format)

table3 = table3[
    [
        "Outcome",
        "Comorbidity group",
        "Adjusted OR [95% CI]",
        "p",
    ]
]

print("\nTABLE 3: ADL-SPECIFIC RESULTS")
print(table3.to_string(index=False))


# 11. COMPACT TABLE


ppt_table = pd.DataFrame({
    "Comorbidity group": [
        "Neither condition",
        "Hypertension only",
        "Diabetes only",
        "Both conditions",
    ],
    "Participants (n)": [
        1564,
        2514,
        117,
        438,
    ],
    "Adjusted OR (95% CI)": [
        "1.00 (reference)",
        "1.00 (0.78 - 1.29)",
        "1.36 (0.68 - 2.50)",
        "2.02 (1.42 - 2.85)",
    ],
    "p": [
        "-",
        ".992",
        ".355",
        "< .001",
    ],
})

ppt_table.to_csv(
    OUT_DIR / "PowerPoint_Compact_Table.csv",
    index=False,
    encoding="utf-8-sig",
)



# 12. EXPORT TABLES AS CSV


table1.to_csv(
    OUT_DIR / "Table_1_Sample_Distribution.csv",
    index=False,
    encoding="utf-8-sig",
)

table2.to_csv(
    OUT_DIR / "Table_2_Primary_Regression.csv",
    index=False,
    encoding="utf-8-sig",
)

table3.to_csv(
    OUT_DIR / "Table_3_ADL_Regression.csv",
    index=False,
    encoding="utf-8-sig",
)

prevalence.to_csv(
    OUT_DIR / "ADL_Descriptive_Prevalence.csv",
    index=False,
    encoding="utf-8-sig",
)

pd.DataFrame([interaction]).to_csv(
    OUT_DIR / "Interaction_Test.csv",
    index=False,
    encoding="utf-8-sig",
)



# 13. EXCEL WORKBOOK


with pd.ExcelWriter(
    OUT_DIR / "HAALSI_APA_Tables.xlsx",
    engine="openpyxl",
) as writer:

    table1.to_excel(
        writer,
        sheet_name="Table 1 Sample",
        index=False,
    )

    table2.to_excel(
        writer,
        sheet_name="Table 2 Primary",
        index=False,
    )

    table3.to_excel(
        writer,
        sheet_name="Table 3 ADLs",
        index=False,
    )

    ppt_table.to_excel(
        writer,
        sheet_name="PowerPoint Table",
        index=False,
    )

    prevalence.to_excel(
        writer,
        sheet_name="ADL Prevalence",
        index=False,
    )

    pd.DataFrame([interaction]).to_excel(
        writer,
        sheet_name="Interaction",
        index=False,
    )

    # Automatically adjust column widths
    for worksheet in writer.sheets.values():

        for column_cells in worksheet.columns:

            max_length = max(
                len(str(cell.value or ""))
                for cell in column_cells
            )

            worksheet.column_dimensions[
                column_cells[0].column_letter
            ].width = min(max_length + 3, 45)



# 14. WORD DOCUMENT - APA-STYLE TABLES


doc = Document()

section = doc.sections[0]
section.top_margin = Inches(0.75)
section.bottom_margin = Inches(0.75)
section.left_margin = Inches(0.8)
section.right_margin = Inches(0.8)

doc.styles["Normal"].font.name = "Arial"
doc.styles["Normal"].font.size = Pt(10)

# Document title
p = doc.add_paragraph()
p.alignment = WD_ALIGN_PARAGRAPH.CENTER

run = p.add_run(
    "Hypertension, Diabetes, and Functional Disability in HAALSI"
)
run.bold = True
run.font.name = "Arial"
run.font.size = Pt(14)

p = doc.add_paragraph()
p.alignment = WD_ALIGN_PARAGRAPH.CENTER

run = p.add_run(
    "Question 5: APA-Style Statistical Tables"
)
run.italic = True
run.font.name = "Arial"
run.font.size = Pt(11)


def add_word_table(number, title, dataframe, note):
    """
    Add a numbered, APA-inspired table to the Word document.
    Uses a clean table layout with horizontal rules.
    """

    # Table number
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(10)
    p.paragraph_format.space_after = Pt(2)

    run = p.add_run(f"Table {number}")
    run.bold = True
    run.font.name = "Arial"
    run.font.size = Pt(11)

    # Italicized title
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(5)

    run = p.add_run(title)
    run.italic = True
    run.font.name = "Arial"
    run.font.size = Pt(11)

    # Build table
    table = doc.add_table(
        rows=1,
        cols=len(dataframe.columns),
    )

    table.style = "Table Grid"
    table.autofit = True

    # Header
    for j, column in enumerate(dataframe.columns):

        cell = table.rows[0].cells[j]
        cell.text = str(column)

        for paragraph in cell.paragraphs:
            for r in paragraph.runs:
                r.bold = True
                r.font.name = "Arial"
                r.font.size = Pt(9)

    # Body
    for _, row in dataframe.iterrows():

        cells = table.add_row().cells

        for j, value in enumerate(row):

            cells[j].text = str(value)

            for paragraph in cells[j].paragraphs:
                paragraph.paragraph_format.space_after = Pt(0)

                for r in paragraph.runs:
                    r.font.name = "Arial"
                    r.font.size = Pt(9)

    # Note below table
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(3)
    p.paragraph_format.space_after = Pt(8)

    run = p.add_run("Note. ")
    run.italic = True
    run.font.name = "Arial"
    run.font.size = Pt(9)

    run = p.add_run(note)
    run.font.name = "Arial"
    run.font.size = Pt(9)

    doc.add_paragraph()


# Table 1
add_word_table(
    1,
    "Distribution of Participants by Comorbidity Status",
    table1,
    (
        f"Percentages use the {classified_n:,} participants with "
        "known comorbidity classification as the denominator. "
        "The 426 participants with missing classification are "
        "listed separately."
    ),
)

# Table 2
add_word_table(
    2,
    "Adjusted Logistic Regression for Any ADL Difficulty",
    table2,
    (
        "Models adjusted for age, sex, education, and wealth. "
        "The reference group is participants with neither condition. "
        "OR = odds ratio; CI = confidence interval. "
        "Values are based on the supplied R output."
    ),
)

# Table 3
add_word_table(
    3,
    "Adjusted Associations Between Comorbidity and ADL Outcomes",
    table3,
    (
        "Each estimate compares the listed group with participants "
        "with neither condition. Models adjusted for age, sex, "
        "education, and wealth. Secondary outcomes are exploratory. "
        "The diabetes-only eating estimate is unstable and should "
        "not be interpreted substantively."
    ),
)



# 15. INTERACTION RESULTS AND LIMITATIONS


doc.add_heading(
    "Interaction analysis",
    level=1,
)

doc.add_paragraph(
    "The hypertension x diabetes interaction odds ratio was "
    "1.49, 95% CI [0.75, 3.15], p = .278. There was no clear "
    "statistical evidence of interaction on the multiplicative "
    "odds scale. This does not prove that interaction is absent "
    "and does not assess additive interaction."
)

doc.add_heading(
    "Interpretation and reporting notes",
    level=1,
)

doc.add_paragraph(
    "Participants with both hypertension and diabetes had "
    "approximately twice the adjusted odds of any ADL difficulty "
    "compared with participants with neither condition "
    "(OR = 2.02, 95% CI [1.42, 2.85], p < .001)."
)

doc.add_paragraph(
    "Because the analysis is cross-sectional, causal conclusions "
    "are not warranted. Odds ratios describe relative odds, not "
    "relative probabilities. Verify model-specific sample sizes, "
    "outcome coding, and missing-data handling before presenting "
    "the findings."
)

doc.add_paragraph(
    "The diabetes-only group contained 117 participants. The "
    "diabetes-only eating-difficulty estimate is highly unstable "
    "and should not be interpreted as evidence of a protective "
    "association."
)

doc.save(
    OUT_DIR / "HAALSI_APA_Tables.docx"
)


# 16. MAIN RESULTS SUMMARY


summary = f"""
HAALSI HACKATHON - QUESTION 5


PRIMARY OUTCOME
Any ADL difficulty.

REFERENCE GROUP
Participants with neither hypertension nor diabetes.

BOTH CONDITIONS
Adjusted OR = 2.018
95% CI = 1.422 - 2.849
p < .001

HYPERTENSION ONLY
Adjusted OR = 1.001
95% CI = 0.779 - 1.294
p = .992

DIABETES ONLY
Adjusted OR = 1.357
95% CI = 0.679 - 2.500
p = .355

INTERACTION
Adjusted interaction OR = 1.485
95% CI = 0.747 - 3.153
p = .278

INTERPRETATION
Participants with both conditions had approximately twice the
adjusted odds of any ADL difficulty compared with participants
with neither condition.

This is a cross-sectional association and does not establish
causality.

There was no clear statistical evidence of multiplicative
interaction.

Before presenting, verify model-specific sample sizes, outcome
coding, and missing-data handling.

The supplied regression estimates have been transcribed from
the provided R output and have not been independently refitted.
"""

(OUT_DIR / "Main_Results_Summary.txt").write_text(
    summary.strip(),
    encoding="utf-8",
)


# 17. COMPLETION MESSAGE


print("\n" + "=" * 60)
print("HAALSI OUTPUT GENERATION COMPLETE")
print("=" * 60)

print(f"\nOutput folder:\n{OUT_DIR}\n")

print("Generated files:")

for path in sorted(OUT_DIR.iterdir()):
    print(f"  - {path.name}")


print("1. Figure_1_Primary_Forest_Plot.png")
print("2. Table_1_Sample_Distribution.csv")
print("3. PowerPoint_Compact_Table.csv")
print("4. Figure_2_ADL_Outcomes_Forest_Plot.png (optional)")
print("5. HAALSI_APA_Tables.docx (full tables and notes)")
print("6. HAALSI_APA_Tables.xlsx (all tables)")
