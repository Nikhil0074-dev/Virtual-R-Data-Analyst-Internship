# Week 2: Data Visualization and Insight Communication using R

**Question:** How has life expectancy changed around the world since 1952, and how much is it linked to income?

This project turns the public Gapminder dataset (142 countries, 1952 to 2007) into 12 charts made with R,
and explains what each chart shows in plain English for a non-technical reader.

## What is inside

| Path | What it is |
|---|---|
| `data/gapminder.csv` | Public dataset, 1,704 rows (source: plotly/datasets on GitHub, same values as the R `gapminder` package) |
| `scripts/01_visualizations.R` | One commented script that loads the data and creates every figure and number used in the report |
| `figures/` | The 12 PNG charts and `summary_stats.txt` (every number quoted in the report) |
| `output/` | Console log and R session info from the last run |
| `report/Week2_R_Data_Visualization_Report.docx` | Final report: narrative, embedded R code, charts and interpretation |
| `report/build_report.js` | Script that builds the Word report from the code and figures |

## The 12 visualizations

| # | Chart | Question it answers |
|---|---|---|
| 1 | Histogram | How did the spread of life expectancy change, 1952 vs 2007? |
| 2 | Bar chart | Which continent is ahead or behind in 2007? |
| 3 | Scatter plot (log scale, bubble size) | How are income and life expectancy linked? |
| 4 | Line chart | How did each continent change over time? |
| 5 | Box plot + points | How different are countries inside one continent? |
| 6 | Highlighted line chart | Which countries had unusual drops? |
| 7 | Lollipop chart | Which countries gained the most? |
| 8 | Small multiples | How did the income-health picture move over time? |
| 9 | Heatmap | How did the 15 biggest countries change? |
| 10 | Line chart (log scale) | How did income grow by continent? |
| 11 | Base R scatter + regression | How strong is the income link? (R-squared 0.65) |
| 12 | Lattice box plots | How do distributions change over time, per continent? |

## Key findings

- Average life expectancy of countries rose from 49.1 to 67.0 years; countries at 70+ went from 5 to 83.
- Income and life expectancy are strongly linked (R-squared 0.65 on log income), but not perfectly.
- Asia gained 26.5 years; Africa gained 15.8 years, almost all before 1987.
- 20 countries had lower life expectancy in 2007 than in 1992 (17 in Africa).

## How to run

```r
# 1. Install packages once
install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "ggrepel",
                   "patchwork", "readr", "ragg", "viridis"))
```

```bash
# 2. From the project folder
Rscript scripts/01_visualizations.R
```

The figures appear in `figures/`. (`lattice` ships with standard R installations.)

## Limits (read before quoting results)

- The data ends in 2007 and is measured every 5 years.
- Charts show association, not proof of cause.
- Oceania has only 2 countries.
- Reasons for big drops (war, epidemics, famine) come from outside knowledge, not from this dataset.

## Tools

R 4.3, ggplot2, dplyr, tidyr, scales, ggrepel, readr, ragg, lattice, base R graphics.
