# replication.R — master script
# "Re-evaluating the labor market effects of occupational licensing"

library(haven)
library(data.table)
library(ggplot2)
library(fixest)
library(readxl)

base_dir <- "/Users/sid/Desktop/paper/replication"
data_dir <- file.path(base_dir, "raw_data")
out_dir  <- file.path(base_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

theme_set(theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank()))

source(file.path(base_dir, "code", "utils.R"))
source(file.path(base_dir, "code", "01_build_regulation.R"))
source(file.path(base_dir, "code", "02_clean_oews.R"))
source(file.path(base_dir, "code", "03_clean_cps.R"))
source(file.path(base_dir, "code", "04_clean_census.R"))
source(file.path(base_dir, "code", "05_tables_figures.R"))

log_step(9, sprintf("Complete. All outputs saved to: %s", out_dir))
