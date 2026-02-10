# 04_clean_census.R — clean ACS census from raw ipums_census.dta

log_step(4, "Cleaning ACS census data from ipums_census.dta")

census_raw <- as.data.table(read_dta(file.path(data_dir, "ipums_census.dta")))
for (col in names(census_raw)) {
  if (haven::is.labelled(census_raw[[col]])) census_raw[, (col) := as.numeric(get(col))]
}

census_raw <- census_raw[empstat == 1 & age >= 25 & age <= 65]
census_raw[, male := fifelse(sex == 1, 1L, 0L)]
census_raw[, married := fifelse(marst == 1, 1L, 0L)]
census_raw[, race_white := fifelse(race == 1, 1L, 0L)]
census_raw[, race_black := fifelse(race == 2, 1L, 0L)]
census_raw[, educ_lesshs := fifelse(educd >= 0 & educd <= 61, 1L, 0L)]
census_raw[, educ_hs := fifelse(educd %in% c(62, 63, 64), 1L, 0L)]
census_raw[, educ_somecoll := fifelse(educd >= 65 & educd <= 83, 1L, 0L)]
census_raw[, educ_collpl := fifelse(educd >= 101 & educd <= 116, 1L, 0L)]
census_raw <- census_raw[incwage >= 15000]

census_raw[, soc2 := as.integer(substr(as.character(occsoc), 1, 2))]
census_raw <- census_raw[!is.na(soc2)]
setnames(census_raw, "statefip", "st")

census_agg <- census_raw[, .(
  famsize     = weighted.mean(famsize, perwt, na.rm = TRUE),
  nchild      = weighted.mean(nchild, perwt, na.rm = TRUE),
  age         = weighted.mean(age, perwt, na.rm = TRUE),
  male        = weighted.mean(male, perwt, na.rm = TRUE),
  married     = weighted.mean(married, perwt, na.rm = TRUE),
  race_white  = weighted.mean(race_white, perwt, na.rm = TRUE),
  race_black  = weighted.mean(race_black, perwt, na.rm = TRUE),
  educ_lesshs = weighted.mean(educ_lesshs, perwt, na.rm = TRUE),
  educ_hs     = weighted.mean(educ_hs, perwt, na.rm = TRUE),
  educ_somecoll = weighted.mean(educ_somecoll, perwt, na.rm = TRUE),
  educ_collpl = weighted.mean(educ_collpl, perwt, na.rm = TRUE),
  incwage     = weighted.mean(incwage, perwt, na.rm = TRUE),
  totsoc2st   = sum(perwt, na.rm = TRUE)
), by = .(st, soc2)]

totst <- census_raw[, .(totst = sum(perwt, na.rm = TRUE)), by = st]
census_agg <- merge(census_agg, totst, by = "st")
census_agg[, soc2stshare := totsoc2st / totst]

cat(sprintf("  %d state x occupation cells\n", nrow(census_agg)))

rm(census_raw, totst)
