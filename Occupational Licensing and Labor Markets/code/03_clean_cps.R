# 03_clean_cps.R — clean CPS microdata from raw ipums_monthly.dta

log_step(3, "Cleaning CPS microdata from ipums_monthly.dta")

cps_raw <- as.data.table(read_dta(file.path(data_dir, "ipums_monthly.dta")))

for (col in names(cps_raw)) {
  if (haven::is.labelled(cps_raw[[col]])) cps_raw[, (col) := as.numeric(get(col))]
}

setnames(cps_raw, "statefip", "st")

cps_raw[, employed := fifelse(empstat == 10, 1L, 0L)]
cps_raw <- cps_raw[famsize > 0]
cps_raw[, male := fifelse(sex == 1, 1L, 0L)]
cps_raw <- cps_raw[sex != 9]
cps_raw <- cps_raw[age >= 25 & age <= 65]

cps_raw[, race_white_nonhispan := fifelse(race == 100 & hispan == 0, 1L, 0L)]
cps_raw[, race_white_hispan := fifelse(race == 100 & hispan >= 100 & hispan <= 500, 1L, 0L)]
cps_raw[, race_black := fifelse(race == 200, 1L, 0L)]
cps_raw[, married := fifelse(marst == 1 | marst == 2, 1L, 0L)]

cps_raw[uhrswork1 == 997, uhrswork1 := ahrsworkt]
cps_raw <- cps_raw[!(uhrswork1 == 999 & employed == 1)]
cps_raw <- cps_raw[!(uhrswork1 == 0 & employed == 1)]

cps_raw[, has_procert_license := NA_integer_]
cps_raw[profcert == 1, has_procert_license := 0L]
cps_raw[profcert == 2, has_procert_license := 1L]

# Education -> years of schooling
cps_raw[, yrschool := NA_real_]
cps_raw[educ == 2, yrschool := 0]
cps_raw[educ == 11, yrschool := 1]
cps_raw[educ %in% c(12, 10), yrschool := 2]
cps_raw[educ == 13, yrschool := 3]
cps_raw[educ == 14, yrschool := 4]
cps_raw[educ %in% c(21, 20), yrschool := 5]
cps_raw[educ == 22, yrschool := 6]
cps_raw[educ %in% c(31, 30), yrschool := 7]
cps_raw[educ == 32, yrschool := 8]
cps_raw[educ == 40, yrschool := 9]
cps_raw[educ == 50, yrschool := 10]
cps_raw[educ == 60, yrschool := 11]
cps_raw[educ %in% c(70, 71, 72, 73), yrschool := 12]
cps_raw[educ %in% c(80, 81), yrschool := 13]
cps_raw[educ %in% c(91, 92), yrschool := 14]
cps_raw[educ == 100, yrschool := 15]
cps_raw[educ %in% c(110, 111, 121), yrschool := 16]
cps_raw[educ %in% c(124, 123), yrschool := 18]
cps_raw[educ == 125, yrschool := 20]

# Earnings
cps_raw[earnweek >= 9999, earnweek := NA_real_]
cps_raw <- cps_raw[!(is.na(earnweek) & employed == 1)]
cps_raw[jtyears > 99, jtyears := NA_real_]

# Deflate by monthly PCE
pce_monthly <- as.data.table(read_dta(file.path(data_dir, "stlouis_pcemonthly.dta")))
cps_raw <- merge(cps_raw, pce_monthly, by = c("year", "month"), all.x = TRUE)
cps_raw[!is.na(pcedefl), earnweek := earnweek * 100 / pcedefl]
cps_raw <- cps_raw[!(is.na(earnweek) & employed == 1)]

cps_raw[, wage := earnweek / uhrswork1]
cps_raw <- cps_raw[!is.na(uhrswork1) & wage >= 2]

# OCC2010 -> SOC2010 crosswalk
occ_xwalk <- as.data.table(read_dta(file.path(data_dir, "occ2010_to_soc2010.dta")))
cps_raw <- merge(cps_raw, occ_xwalk, by = "occ2010", all.x = TRUE)

# Manual SOC assignments for missing codes
cps_raw[occ2010 == 30 & is.na(soc2010), soc2010 := "11-201"]
cps_raw[occ2010 == 130, soc2010 := "11-312"]
cps_raw[occ2010 == 320, soc2010 := "11-906"]
cps_raw[occ2010 == 560, soc2010 := "13-104"]
cps_raw[occ2010 == 620, soc2010 := "13-107"]
cps_raw[occ2010 == 720, soc2010 := "13-112"]
cps_raw[occ2010 == 730, soc2010 := "13-119"]
cps_raw[occ2010 == 1000, soc2010 := "15-112"]
cps_raw[occ2010 == 1100, soc2010 := "15-114"]
cps_raw[occ2010 == 1230, soc2010 := "15-203"]
cps_raw[occ2010 == 1830, soc2010 := "19-305"]
cps_raw[occ2010 == 1960, soc2010 := "19-409"]
cps_raw[occ2010 == 2020, soc2010 := "21-109"]
cps_raw[occ2010 == 2140, soc2010 := "23-201"]
cps_raw[occ2010 == 2150, soc2010 := "23-209"]
cps_raw[occ2010 == 3130, soc2010 := "29-114"]
cps_raw[occ2010 == 3240, soc2010 := "31-901"]
cps_raw[occ2010 == 3410, soc2010 := "29-101"]
cps_raw[occ2010 == 3530, soc2010 := "29-209"]
cps_raw[occ2010 == 3650, soc2010 := "31-909"]
cps_raw[occ2010 == 3950, soc2010 := "33-301"]
cps_raw[occ2010 == 6940, soc2010 := "47-509"]
cps_raw[occ2010 == 7125, soc2010 := "49-209"]
cps_raw[occ2010 == 8230, soc2010 := "51-511"]
cps_raw[occ2010 == 9100, soc2010 := "53-301"]

cps_raw[soc2010 == "19-10X", soc2010 := "19-104"]
cps_raw[soc2010 == "21-209", soc2010 := "21-202"]
cps_raw[soc2010 == "25-100", soc2010 := "25-101"]
cps_raw[soc2010 == "25-300", soc2010 := "25-301"]

cps_raw <- cps_raw[!(is.na(soc2010) & employed == 1) & soc2010 != ""]
cps_raw[, soc2 := as.integer(substr(soc2010, 1, 2))]
cps_raw <- cps_raw[!is.na(soc2)]

cat(sprintf("  %d observations\n", nrow(cps_raw)))

rm(pce_monthly, occ_xwalk)
