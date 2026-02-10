# Occupational Licensing and Labor Markets — Replication and Extension in R


This is an independent replication and extension of the empirical analysis on occupational licensing and labor market outcomes, implemented entirely in R. The analysis uses publicly available data to construct state-level regulation indices, clean wage and employment microdata, and estimate the relationship between occupational licensing and labor market outcomes.

---

## Data

The raw data files are publicly available from the **Harvard Dataverse**:

> **https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IZGDCO**

After downloading, place all files into the `raw_data/` folder with the following structure:

```
raw_data/
    soc_state_data.csv
    stlouis_pcedeflannual.dta
    stlouis_pcemonthly.dta
    ipums_monthly.dta
    ipums_census.dta
    occ2010_to_soc2010.dta
    state_2016_election.dta
    state2019.dta
    licensing data/
        MasterData_SOLI2023.dta
    oews/
        state_M2017_dl.xlsx
        state_M2018_dl.xlsx
        state_M2019_dl.xlsx
        state_M2020_dl.xlsx
        state_M2021_dl.xlsx
        state_M2022_dl.xlsx
```

Data files are excluded from this repository due to size (~1.5 GB).

---

## Project Structure

```
Occupational Licensing and Labor Markets/
|-- README.md
|-- code/
|   |-- replication.R          # Master script — run this file
|   |-- utils.R                # Helper functions (winsorize, weighted summary, weighted correlation)
|   |-- 01_build_regulation.R  # Build state x occupation regulation panel
|   |-- 02_clean_oews.R        # Clean BLS OEWS wage/employment data (2017-2022)
|   |-- 03_clean_cps.R         # Clean CPS microdata (demographics, earnings, licensing)
|   |-- 04_clean_census.R      # Clean ACS census data (state x occupation aggregates)
|   |-- 05_tables_figures.R    # Produce all tables and figures
|-- raw_data/                  # Place downloaded data here (see above)
|-- output/                    # Generated tables and figures
```

---

## Replication Pipeline

The full analysis runs from a single master script (`code/replication.R`) that sources each step sequentially:

| Step | Script | What it does |
|------|--------|--------------|
| 1 | `01_build_regulation.R` | Constructs regulatory restriction indices at the state x 2-digit SOC x year level from `soc_state_data.csv` |
| 2 | `02_clean_oews.R` | Reads BLS Occupational Employment and Wage Statistics Excel files (2017--2022), deflates wages by PCE, collapses to state x occupation x year |
| 3 | `03_clean_cps.R` | Cleans IPUMS CPS microdata: demographics, earnings (deflated by monthly PCE), occupation crosswalk (OCC2010 to SOC 2010) |
| 4 | `04_clean_census.R` | Cleans IPUMS ACS data, computes weighted demographic and wage means at the state x occupation level |
| 5 | `05_tables_figures.R` | Merges all datasets, produces descriptive statistics (Table 1), scatter plots with correlations (Figures 1, 3--5), and regression tables using `fixest` (Tables 2--3) |

---

## How to Reproduce

**Requirements:** R (>= 4.0) with the following packages:

```r
install.packages(c("haven", "data.table", "ggplot2", "fixest", "readxl"))
```

**Steps:**

1. Clone this repository
2. Download data from [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IZGDCO) into `raw_data/`
3. Open `code/replication.R` and set `base_dir` to the path of this folder
4. Run:
   ```bash
   Rscript code/replication.R
   ```
5. All outputs are saved to `output/`

---

## Outputs

| File | Description |
|------|-------------|
| `table1_descriptive.csv` | Descriptive statistics by regulation intensity (high vs. low) |
| `table2_oews.tex` | OEWS regressions — employment and wages on log regulation (12 specifications with varying fixed effects) |
| `table3_cps.tex` | CPS individual-level regressions — employment and earnings on regulation and licensing interactions (6 specifications) |
| `fig1a_unbalanced.pdf` | Regulatory restrictions over time (unbalanced panel) |
| `fig1b_balanced.pdf` | Regulatory restrictions over time (balanced panel, 2019--2022) |
| `fig3a_wagegap.pdf` | Regulation growth vs. 75th--25th percentile wage gap |
| `fig3b_wagegrowth.pdf` | Regulation growth vs. median wage growth |
| `fig4a_regpolitics.pdf` | Log regulation level vs. GOP vote share |
| `fig4b_growthpolitics.pdf` | Regulation growth vs. GOP vote share |
| `fig5a_barrier.pdf` | Log regulation vs. SOLI Barrier Score (Trudeau and Timmons 2023) |
| `fig5b_licenses.pdf` | Log regulation vs. SOLI Licenses Score |

---

## Citation

This replication is based on the following paper:

> Makridis, C.A., McLaughlin, P.A. (2025). Re-evaluating the labor market effects of occupational licensing: Longitudinal evidence across states. *Humanities and Social Sciences Communications*, 12, 240. https://doi.org/10.1038/s41599-025-04497-5

**Replication data:**

> Makridis, Christos A.; McLaughlin, Patrick A. (2024). "Replication Data for: Re-evaluating the Labor Market Effects of Occupational Licensing." Harvard Dataverse. https://doi.org/10.7910/DVN/IZGDCO
