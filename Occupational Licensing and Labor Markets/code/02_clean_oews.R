# 02_clean_oews.R — clean OEWS from raw Excel files 2017-2022

log_step(2, "Cleaning OEWS data from raw Excel files")

pce_annual <- as.data.table(read_dta(file.path(data_dir, "stlouis_pcedeflannual.dta")))

read_oews_year <- function(yr) {
  path <- file.path(data_dir, "oews", paste0("state_M", yr, "_dl.xlsx"))
  d <- as.data.table(read_excel(path, sheet = "data"))
  setnames(d, toupper(names(d)))

  if ("STATE" %in% names(d)) setnames(d, "STATE", "STATE_NAME")
  else if ("AREA_TITLE" %in% names(d)) setnames(d, "AREA_TITLE", "STATE_NAME")

  group_col <- intersect(c("O_GROUP", "OCC_GROUP"), names(d))
  if (length(group_col) > 0) setnames(d, group_col[1], "OCC_GROUP_STD")
  d <- d[tolower(OCC_GROUP_STD) == "detailed"]

  keep_cols <- c("STATE_NAME", "OCC_CODE", "TOT_EMP",
                 "H_PCT10", "H_PCT25", "H_MEDIAN", "H_PCT75", "H_PCT90",
                 "A_PCT10", "A_PCT25", "A_MEDIAN", "A_PCT75", "A_PCT90")
  d <- d[, .SD, .SDcols = intersect(keep_cols, names(d))]

  wage_cols <- c("H_PCT10","H_PCT25","H_MEDIAN","H_PCT75","H_PCT90",
                 "A_PCT10","A_PCT25","A_MEDIAN","A_PCT75","A_PCT90")
  hourly_top <- ifelse(yr >= 2022, 115, 100)
  annual_top <- ifelse(yr >= 2022, 239200, 208000)

  for (col in intersect(wage_cols, names(d))) {
    v <- d[[col]]
    if (is.character(v)) {
      top_val <- ifelse(grepl("^A_", col), annual_top, hourly_top)
      v[v == "#"] <- as.character(top_val)
      v[v %in% c("*", "**")] <- NA_character_
    }
    d[[col]] <- suppressWarnings(as.numeric(v))
  }

  if (is.character(d$TOT_EMP)) {
    d[TOT_EMP %in% c("*", "**"), TOT_EMP := NA_character_]
    d[, TOT_EMP := as.numeric(TOT_EMP)]
  }

  d[, year := yr]
  d
}

oews_raw <- rbindlist(lapply(2017:2022, read_oews_year), fill = TRUE)

# Match state names to FIPS
us_states <- data.table(
  full = c(state.name, "District of Columbia", "Guam", "Puerto Rico", "Virgin Islands"),
  abbr = c(state.abb, "DC", "GU", "PR", "VI"),
  fips = c(1L,2L,4L,5L,6L,8L,9L,10L,12L,13L,15L,16L,17L,18L,19L,20L,21L,22L,23L,
           24L,25L,26L,27L,28L,29L,30L,31L,32L,33L,34L,35L,36L,37L,38L,39L,40L,
           41L,42L,44L,45L,46L,47L,48L,49L,50L,51L,53L,54L,55L,56L,
           11L, 66L, 72L, 78L)
)
oews_raw <- merge(oews_raw, us_states, by.x = "STATE_NAME", by.y = "full", all.x = TRUE)
setnames(oews_raw, c("abbr", "fips"), c("state_abbr", "st"))
oews_raw <- oews_raw[!is.na(st) & st < 57]

# Deflate by PCE
oews_raw <- merge(oews_raw, pce_annual, by = "year")
deflate_cols <- c("H_PCT10","H_PCT25","H_MEDIAN","H_PCT75","H_PCT90",
                  "A_PCT10","A_PCT25","A_MEDIAN","A_PCT75","A_PCT90")
for (col in deflate_cols) {
  oews_raw[, (col) := get(col) * 100 / pcedefl]
}

# 2-digit SOC and collapse
oews_raw[, soc2 := as.integer(substr(OCC_CODE, 1, 2))]

oews_agg <- oews_raw[!is.na(soc2) & !is.na(TOT_EMP) & TOT_EMP > 0, {
  w <- TOT_EMP
  tot <- sum(w, na.rm = TRUE)
  res <- list(TOT_EMP = tot)
  for (col in deflate_cols) {
    vals <- get(col)
    ok <- !is.na(vals) & !is.na(w)
    if (sum(ok) > 0) res[[col]] <- sum(vals[ok] * w[ok]) / sum(w[ok])
    else res[[col]] <- NA_real_
  }
  res
}, by = .(soc2, st, year)]

setorder(oews_agg, soc2, st, year)

# Growth rates (winsorized)
oews_agg[, dlTOT_EMP := (TOT_EMP - shift(TOT_EMP)) / shift(TOT_EMP), by = .(soc2, st)]
oews_agg[, dlH_MEDIAN := (H_MEDIAN - shift(H_MEDIAN)) / shift(H_MEDIAN), by = .(soc2, st)]
oews_agg[, dlTOT_EMP := winsorize(dlTOT_EMP)]
oews_agg[, dlH_MEDIAN := winsorize(dlH_MEDIAN)]

cat(sprintf("  %d obs, years %s\n", nrow(oews_agg),
            paste(sort(unique(oews_agg$year)), collapse = ", ")))

rm(oews_raw, pce_annual)
