# Replication Package: Working From Home Around the World

Cevat Giray Aksoy, Jose Maria Barrero, Nicholas Bloom, Steven J. Davis, Mathias Dolls, and Pablo Zarate

## Overview

This package replicates all tables and figures from the paper using the Global Survey of Working Arrangements (G-SWA) microdata. The analysis is implemented in R using `fixest` for high-dimensional fixed effects estimation.

## Software Requirements

- **R** (>= 4.1.0)
- Required packages: `haven`, `fixest`, `dplyr`, `ggplot2`, `tidyr`, `modelsummary`, `scales`, `patchwork`

Install all dependencies:

```r
install.packages(c("haven", "fixest", "dplyr", "ggplot2", "tidyr",
                   "modelsummary", "scales", "patchwork"))
```

## Directory Structure

```
├── README.md              This file
├── replicate.R            Master replication script (R)
├── Data/
│   ├── G-SWA.dta          G-SWA microdata (Stata format)
│   └── G-SWA.xlsx         G-SWA microdata (Excel format)
├── Tables/                Generated tables (HTML/CSV)
└── Figures/               Generated figures (PNG)
```

## Instructions

1. Open R and set your working directory to this folder:

```r
setwd("/path/to/Replication file 2")
```

2. Run the replication script:

```r
source("replicate.R")
```

All tables will be saved to `Tables/` and all figures to `Figures/`. Runtime is approximately 2 minutes.

## Output Mapping

### Tables

| Output File     | Paper Reference | Description |
|-----------------|-----------------|-------------|
| Table1.html     | Table 1         | The structure of preferences over WFH |
| Table2.html     | Table 2         | WFH rises with cumulative lockdown stringency |
| Table3A.html    | Table 3, Panel A| Lockdown effects, tertiary+ educated |
| Table3B.html    | Table 3, Panel B| Lockdown effects, graduate educated |
| TableA1.csv     | Table A.1       | G-SWA country-level survey waves |
| TableA2.csv     | Table A.2       | Country-level summary statistics |
| TableA3.csv     | Table A.3       | Country-level percentages |
| TableA4.csv     | Table A.4       | Comparison with Gallup World Poll |
| TableA5.html    | Table A.5       | Adding mask mandate controls |
| TableA6.html    | Table A.6       | Subnational variation |
| TableA7.html    | Table A.7       | Oxford stringency index |

### Figures

| Output File         | Paper Reference | Description |
|---------------------|-----------------|-------------|
| Figure 1.png        | Figure 1        | Days WFH this week, by country |
| Figure 2.png        | Figure 2        | Planned WFH days, by country |
| Figure 3.png        | Figure 3        | Amenity value of WFH option, by country |
| Figure 4.png        | Figure 4        | Commuting time, by country |
| Figure 5.png        | Figure 5        | WFH value by gender (scatter) |
| Figure 6A.png       | Figure 6A       | WFH value: married men w/ vs w/o children |
| Figure 6B.png       | Figure 6B       | WFH value: married women w/ vs w/o children |
| Figure 6C.png       | Figure 6C       | WFH value: single men vs women |
| Figure 8.png        | Figure 8        | Planned WFH vs productivity surprise |
| Figure A2.png       | Figure A.2      | Desired WFH days, by country |
| Figure A3.png       | Figure A.3      | Quit/look for WFH job if forced to return |
| Figure A4_1-3.png   | Figure A.4      | Planned WFH vs productivity, by country |
| Figure A5.png       | Figure A.5      | WFH productivity surprise, by country |
| Figure A6.png       | Figure A.6      | Change in WFH perceptions, by country |
| Figure A7.png       | Figure A.7      | Cumulative lockdown stringency, by country |
| Figure A8.png       | Figure A.8      | Cumulative COVID-19 deaths, by country |
| FigureA.1.png       | Figure A.1      | Histogram of WFH amenity value |

**Note:** Figure 7 requires the categorical variable `home_work_expectations_workers`, which is not included in the replication dataset.

## Data Description

The G-SWA dataset contains 36,078 observations across 27 countries and 2 survey waves. Key variables:

| Variable | Description |
|----------|-------------|
| `original_country` | Country |
| `wave` | Survey wave (1 or 2) |
| `gender` | 1 = Female, 2 = Male, 3 = Other |
| `agegroups` | Age group (20-29, 30-39, 40-49, 50-59) |
| `education` | 1 = Elementary, 2 = Secondary, 3 = Tertiary, 4 = Graduate |
| `industry_job` | Industry of current/most recent job (18 categories) |
| `n_work_home` | Days working from home this week |
| `daysemployer_work_home` | Planned days WFH per week (employer) |
| `daysemployee_work_home` | Desired days WFH per week (employee) |
| `value_WFH_rawpercent25` | Amenity value of WFH 2-3 days/week (% of pay) |
| `commuting_time` | Round trip commute time (minutes) |
| `WFH_expectations1` | WFH productivity during COVID vs expectations |
| `WFHperceptions` | Change index for social acceptance of WFH |
| `return_office` | Response to mandatory 5+ day return |
| `deaths_pc` | COVID-19 deaths per capita |
| `LSI` | Lockdown Stringency Index |
| `gdppc2019` | 2019 GDP per capita (constant 2010 US$) |

## Methodology Notes

- All regressions use `fixest::feols()`, the R equivalent of Stata's `reghdfe`
- Fixed effects are absorbed for gender, age groups, education, industry, and survey wave
- Standard errors are clustered at the country level (Tables) or country-wave level (Figures)
- Country bar charts show regression-adjusted conditional means, with the raw USA mean added back to each country's coefficient
- Figures A.7 and A.8 show weighted country means (equal weight per wave within country) rather than regression-adjusted values
