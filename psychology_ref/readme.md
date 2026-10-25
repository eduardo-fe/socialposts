# REF 2021 × CUG 2027 Psychology: Research Quality and Entry Standards

## Overview

This project combines research assessment results from the **Research Excellence Framework (REF) 2021**, Unit of Assessment 4 (Psychology, Psychiatry and Neuroscience), with subject-level university rankings and indicators from **The Complete University Guide (CUG) 2027 Psychology league table**.

The analysis explores the relationship between the research quality of psychology departments and the entry standards of their students. It also examines whether some institutions achieve higher REF research quality than would be predicted by their entry standards.

The central descriptive question is:

> Which UK psychology departments combine strong research performance with comparatively accessible entry requirements?

The analysis is exploratory and descriptive. The resulting measures should not be interpreted as causal effects, comprehensive measures of teaching quality, or definitive rankings of university value.

## Data sources

### REF 2021

- **Unit of Assessment:** UoA 4, Psychology, Psychiatry and Neuroscience.
- **Source file:** `REF_2021_Results.xlsx`
- **Original filename:** `REF_2021_Results_-_4_-_2022-05-06.xlsx`
- **Source:** UK Research and Innovation (UKRI).
- **Data selection:** Rows corresponding to the overall research profile.

The script reads the Excel file, skipping the first six rows, and extracts institutional names, submitted staff FTE, the percentage of eligible staff submitted, and the proportions of research activity assigned to each REF quality category.

### CUG 2027

- **Subject:** Psychology.
- **Source:** [The Complete University Guide: Psychology rankings](https://www.thecompleteuniversityguide.co.uk/league-tables/rankings/psychology).

The CUG data are entered directly into the R script as a tibble. The variables cover entry standards, student satisfaction, research quality, continuation, graduate outcomes and graduate-on-track indicators.

The CUG research-quality indicator is distinct from the REF GPA constructed in this analysis. The latter is calculated directly from the REF 2021 institutional profile.

## Methodology

### 1. Constructing the REF research-quality GPA

For each institution, the overall REF profile supplies the proportions of research activity assessed at four quality levels:

- \(4^*\): World-leading.
- \(3^*\): Internationally excellent.
- \(2^*\): Recognised internationally.
- \(1^*\): Recognised nationally.

The script constructs a weighted average:

\[
\operatorname{REF\ GPA}_i =
\frac{4p_{i,4^*}+3p_{i,3^*}+2p_{i,2^*}+p_{i,1^*}}
{p_{i,4^*}+p_{i,3^*}+p_{i,2^*}+p_{i,1^*}}
\]

where \(p_{i,k^*}\) denotes the reported quantity for quality category \(k^*\).

If the REF quantities are percentages, the GPA lies between 1 and 4, provided the profile has a positive total. The calculation uses the sum of the reported profile quantities as its denominator.

This is a constructed summary measure. It compresses the distribution of research quality into a single number and does not preserve all the information in the original profile.

### 2. Harmonising institutional names

REF and CUG use different institutional naming conventions. The script maps known discrepancies, including formal legal names, Welsh bilingual names, trailing articles, and institutional rebrands.

Examples include:

- `City, University of London` → `City St George's London`
- `Royal Holloway and Bedford New College` → `Royal Holloway`
- `University of Durham` → `Durham University`
- `University of Wales Trinity Saint David / Prifysgol Cymru Y Drindod Dewi Sant` → `Univ of Wales Trinity Saint David`

The remaining `The ` prefix is removed programmatically.

The cleaned REF data are then left-joined to the CUG data using the harmonised institution name. The script reports CUG institutions without a matched REF record and REF institutions absent from the CUG table.

A missing match should not automatically be interpreted as an institution having made no REF submission. It can also arise from differences in naming, institutional structure, or coverage.

### 3. Entry standards

CUG entry standards are expressed as UCAS tariff points, with a maximum of 205 in the supplied data.

For the first set of plots and the regression analysis, the original tariff-point measure is retained.

The script also calculates:

\[
\operatorname{EntryPct}_i = 100 \times \frac{\operatorname{EntryStd}_i}{205}
\]

This rescaling is included in the merged data but is not used in the main regression.

### 4. Research-quality value index

The script estimates the ordinary least squares regression:

\[
\operatorname{REF\ GPA}_i =
\alpha + \beta\operatorname{EntryStd}_i + \varepsilon_i
\]

The fitted values represent the research GPA predicted by the linear relationship between entry standards and REF research quality in the matched sample.

The residual for institution \(i\) is:

\[
\hat{\varepsilon}_i =
\operatorname{REF\ GPA}_i -
\widehat{\operatorname{REF\ GPA}}_i
\]

A standardised residual-based index is then calculated:

\[
V_i = \frac{\hat{\varepsilon}_i-\bar{\hat{\varepsilon}}}
{s_{\hat{\varepsilon}}}
\]

using R's `scale()` function.

A positive index indicates that an institution's REF GPA is above the value predicted by the fitted linear relationship. A negative index indicates that it is below the prediction.

The index is a **relative, model-dependent measure of research performance conditional on entry standards**. It is not a measure of causal value added, and it does not establish that a university produces better research because of its admissions policy.

### 5. Student satisfaction and research quality

A separate plot compares CUG student satisfaction scores with REF GPA for institutions matched to the REF data and with non-missing satisfaction scores.

Points are coloured by CUG entry standards. A fitted linear trend is added to describe the overall association.

Student satisfaction, research quality and entry standards measure different aspects of university performance. Any association between them should be interpreted descriptively.

### 6. Alternative visualisations

The script explores several versions of the research-quality-versus-entry-standards plot:

1. A scatterplot with a linear regression line.
2. A residual plot highlighting institutions above and below the fitted relationship.
3. An annotated scatterplot with a customised publication-style theme.
4. A scatterplot with a LOESS smoother.
5. Local-polynomial smoothers estimated using `KernSmooth::locpoly()`.

The customised publication-style version restricts the plotting sample to institutions with entry standards of at least 120 UCAS points and REF GPA of at least 3. The local-polynomial alternatives also explore restricting the sample to entry standards of at least 120.

These are alternative exploratory specifications, not independent analyses or robustness checks that automatically validate the results. The choice of sample, smoothing method and bandwidth can affect the apparent pattern.

### 7. Institutional comparisons

Two tables are produced to support further investigation.

**Top 20 by value index**

Institutions are ordered by the standardised residual index, with the highest values first. The table includes entry standards, REF GPA, the 4-star share and submitted staff FTE.

**Research quality among institutions with entry standards of 144 points or less**

The script identifies matched institutions with CUG entry standards no greater than 144 points and ranks them by REF GPA. The threshold is intended as an approximate AAB-or-below entry tier, although UCAS tariff equivalences depend on the qualifications and grades represented.

## Outputs

The script generates the following outputs.

| Output | Description |
|---|---|
| `p1` | Main REF GPA versus CUG entry-standards scatterplot; subsequently overwritten by alternative specifications. |
| `p2` | Residual plot showing research performance relative to the fitted linear relationship. |
| `p3` | Student satisfaction versus REF research quality. |
| `combined` | Merged CUG and REF institutional dataset. |
| `fit_data` | Matched analysis sample, including fitted values, residuals and the value index. |
| `no_ref` | CUG institutions without a matched REF GPA. |
| `ref_unmatched` | REF institutions without a matching CUG entry. |
| `aab_tier` | Matched institutions in the specified entry-standards tier. |
| `scatter_owid_psychology.png` | High-resolution PNG of the customised annotated scatterplot. |

The PNG is saved at 300 dpi, with a nominal size of 11 × 9 inches.

## Requirements

The analysis requires R and the following packages:

- `dplyr`
- `tidyr`
- `readxl`
- `ggplot2`
- `ggrepel`
- `tibble` (used through `tibble::tribble()`)
- `stringr` (used through `stringr::str_remove()` and `stringr::str_trim()`)
- `KernSmooth` (used for the local-polynomial alternatives)

Install any missing packages with:

```r
install.packages(c(
  "dplyr",
  "tidyr",
  "readxl",
  "ggplot2",
  "ggrepel",
  "tibble",
  "stringr",
  "KernSmooth"
))
```

Place the REF Excel file in the working directory, or update the file path in `read_xlsx()`. Run the script in R or RStudio.

## Important implementation and interpretation notes

Before using the results for publication or institutional comparisons, the following points should be checked.

1. **REF GPA denominator.** The script contains an earlier, superseded calculation that divides the weighted sum by four. The second calculation, which divides by the sum of the profile quantities, is the operative version. The denominator must be appropriate to the units and structure of the source data.

2. **Duplicated data preparation.** The REF extraction is performed twice. The first assignment is redundant and can be removed.

3. **REF quality units.** Confirm whether the source columns contain percentages, proportions or another representation. The GPA calculation should use consistent units, and the resulting scale should be verified against the published REF profiles.

4. **Institutional matching.** Inspect both unmatched lists and verify that name harmonisation has not incorrectly merged distinct institutions or left valid matches unresolved.

5. **Missing CUG data.** Several institutions have missing CUG satisfaction or research-quality scores. Missing values should not be interpreted as zero.

6. **CUG research quality.** The CUG research-quality score is entered in the data but is not used in the main regression, which uses the independently constructed REF GPA.

7. **Sample selection.** The regression is estimated only for institutions matched across the two datasets. The customised plot and local-polynomial alternatives impose additional restrictions. Results therefore apply to their respective analysis samples.

8. **Residual interpretation.** The value index depends on the selected regression specification and sample. It is not a causal estimate of university value added, teaching effectiveness or research productivity.

9. **Small and unequal research submissions.** Institutions differ in submitted staff FTE and potentially in the composition of their research submissions. The unweighted regression gives each institution equal weight; it does not adjust for submission size or uncertainty in the estimated research profile.

10. **Reproducibility.** The REF data are read from an external Excel file, whereas the CUG data are hard-coded. Preserve the original source files and document the date and method used to transcribe the CUG values.

## Suggested interpretation

The analysis offers a transparent way to compare REF-assessed psychology research quality with the entry standards of institutions in the CUG 2027 league table.

The most useful interpretation is comparative: institutions above the fitted line have higher REF GPA than the linear relationship with entry standards would predict, while institutions below the line have lower REF GPA than predicted.

These differences can identify interesting cases for further investigation. They do not, by themselves, establish why the differences occur or provide a complete assessment of institutional performance.
