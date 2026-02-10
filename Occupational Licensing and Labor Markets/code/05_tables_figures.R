# 05_tables_figures.R — produce all tables and figures

# ---- Merge CPS with regulation ----
log_step(5, "Table 1: Descriptive Statistics")

cps <- merge(cps_raw, soc_state[, .(st, soc2, year, reg_restrict, lreg_restrict)],
             by = c("st", "soc2", "year"), all.x = FALSE)
cps <- merge(cps, state_reg, by = c("st", "year"), all.x = FALSE)

med_reg <- cps[, median(reg_restrict, na.rm = TRUE)]
cps[, highreg := fifelse(reg_restrict > med_reg, 1L, 0L)]

tab1_vars   <- c("male", "race_white_nonhispan", "race_white_hispan", "race_black",
                  "married", "yrschool", "famsize", "age",
                  "has_procert_license", "employed", "earnweek", "reg_restrict")
tab1_labels <- c("Male", "White", "Hispanic", "Black", "Married",
                  "Years of education", "Family size", "Age",
                  "Is licensed", "Is employed", "Weekly earnings", "State x Occ. Reg")

tab1_all  <- wt_summary(cps, tab1_vars, tab1_labels, "wtfinl")
tab1_high <- wt_summary(cps[highreg == 1], tab1_vars, tab1_labels, "wtfinl")
tab1_low  <- wt_summary(cps[highreg == 0], tab1_vars, tab1_labels, "wtfinl")

tab1 <- data.frame(
  Variable  = tab1_all$Variable,
  All_Mean  = round(tab1_all$Mean, 2),   All_SD  = round(tab1_all$SD, 2),
  High_Mean = round(tab1_high$Mean, 2),  High_SD = round(tab1_high$SD, 2),
  Low_Mean  = round(tab1_low$Mean, 2),   Low_SD  = round(tab1_low$SD, 2)
)

write.csv(tab1, file.path(out_dir, "table1_descriptive.csv"), row.names = FALSE)
print(tab1)

# ---- Figures ----
log_step(6, "Generating Figures")

# -- Figure 1: Regulatory restrictions over time --
fig1a <- soc_state[, .(reg_restrict = sum(reg_restrict) / 1e6), by = year]
p1a <- ggplot(fig1a, aes(x = factor(year), y = reg_restrict)) +
  geom_col(fill = "olivedrab4", width = 0.6) +
  labs(x = NULL, y = "Regulatory Restrictions (millions)",
       subtitle = "Panel A: Unbalanced Panel") +
  theme_bw()
ggsave(file.path(out_dir, "fig1a_unbalanced.pdf"), p1a, width = 6, height = 5)
print(p1a)

bal_states <- soc_state[year >= 2019 & year <= 2022,
                        .(n_yr = uniqueN(year)), by = st][n_yr >= 3, st]
fig1b <- soc_state[year >= 2019 & year <= 2022 & st %in% bal_states,
                   .(reg_restrict = sum(reg_restrict) / 1e6), by = year]
p1b <- ggplot(fig1b, aes(x = factor(year), y = reg_restrict)) +
  geom_col(fill = "olivedrab4", width = 0.6) +
  labs(x = NULL, y = "Regulatory Restrictions (millions)",
       subtitle = "Panel B: Balanced Panel") +
  theme_bw()
ggsave(file.path(out_dir, "fig1b_balanced.pdf"), p1b, width = 6, height = 5)
print(p1b)

# -- Figure 3: Occupational heterogeneity --
oews_reg <- merge(oews_agg, soc_state[, .(st, soc2, year, reg_restrict)],
                  by = c("st", "soc2", "year"), all.x = FALSE)

fig3 <- oews_reg[year %in% c(2020, 2022) &
                   !is.na(H_MEDIAN) & !is.na(H_PCT75) & !is.na(H_PCT25) & TOT_EMP > 0,
                 .(reg_restrict = sum(reg_restrict),
                   H_MEDIAN = weighted.mean(H_MEDIAN, TOT_EMP),
                   H_PCT75  = weighted.mean(H_PCT75, TOT_EMP),
                   H_PCT25  = weighted.mean(H_PCT25, TOT_EMP),
                   TOT_EMP  = sum(TOT_EMP)),
                 by = .(soc2, year)]

setorder(fig3, soc2, year)
fig3[, dlreg := (reg_restrict - shift(reg_restrict)) / shift(reg_restrict), by = soc2]
fig3[, dlH_MED := (H_MEDIAN - shift(H_MEDIAN)) / shift(H_MEDIAN), by = soc2]
fig3[, lH_gap := log(H_PCT75) - log(H_PCT25)]
fig3 <- fig3[!is.na(dlreg)]
fig3[, dlreg := winsorize(dlreg)]
fig3[, dlH_MED := winsorize(dlH_MED)]

corr_3a <- wcor(fig3$dlreg, fig3$lH_gap, fig3$TOT_EMP)
corr_3b <- wcor(fig3$dlreg, fig3$dlH_MED, fig3$TOT_EMP)
cat(sprintf("  Fig 3 correlations: wage gap = %.2f, wage growth = %.2f\n", corr_3a, corr_3b))

p3a <- ggplot(fig3, aes(x = lH_gap, y = dlreg * 100, weight = TOT_EMP)) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_point(aes(size = TOT_EMP), shape = 1, alpha = 0.7) +
  labs(x = "log 75th to 25th Wage Gap", y = "Regulatory Growth (2020-2022)",
       subtitle = "Panel A: Regulation and 75-25 Wage Gap",
       caption = sprintf("Correlation = %.2f", corr_3a)) +
  guides(size = "none") + theme_bw()
ggsave(file.path(out_dir, "fig3a_wagegap.pdf"), p3a, width = 6, height = 5)
print(p3a)

p3b <- ggplot(fig3, aes(x = dlH_MED, y = dlreg * 100, weight = TOT_EMP)) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_point(aes(size = TOT_EMP), shape = 1, alpha = 0.7) +
  labs(x = "Median Wages Growth (2020-2022)", y = "Regulatory Growth (2020-2022)",
       subtitle = "Panel B: Regulation and Wage Growth",
       caption = sprintf("Correlation = %.2f", corr_3b)) +
  guides(size = "none") + theme_bw()
ggsave(file.path(out_dir, "fig3b_wagegrowth.pdf"), p3b, width = 6, height = 5)
print(p3b)

# -- Figure 4: Regulation and politics --
election  <- as.data.table(read_dta(file.path(data_dir, "state_2016_election.dta")))
election  <- election[!is.na(st) & st < 57]
state2019 <- as.data.table(read_dta(file.path(data_dir, "state2019.dta")))

fig4_st <- soc_state[year >= 2020 & year <= 2022,
                     .(reg_restrict = sum(reg_restrict)), by = .(st, year)]
setorder(fig4_st, st, year)
fig4_st[, dlreg := (reg_restrict - shift(reg_restrict)) / shift(reg_restrict), by = st]
fig4_agg <- fig4_st[, .(dlreg = mean(dlreg, na.rm = TRUE),
                         reg_restrict = mean(reg_restrict)), by = st]
fig4_agg[, lreg := log(reg_restrict)]

fig4 <- merge(fig4_agg, election[, .(st, trump_percent)], by = "st")
fig4 <- merge(fig4, state2019[, .(st, totpop)], by = "st")

corr_4a <- wcor(fig4$lreg, fig4$trump_percent, fig4$totpop)
fig4b <- fig4[!is.na(dlreg) & is.finite(dlreg)]
corr_4b <- wcor(fig4b$dlreg, fig4b$trump_percent, fig4b$totpop)
cat(sprintf("  Fig 4 correlations: level = %.2f, growth = %.2f\n", corr_4a, corr_4b))

p4a <- ggplot(fig4, aes(x = trump_percent, y = lreg, weight = totpop)) +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", alpha = 0.3) +
  geom_point(aes(size = totpop), shape = 1, alpha = 0.7) +
  labs(x = "GOP Vote Share", y = "log(Regulation) (2020-2022)",
       subtitle = "Panel A: Regulation and Politics",
       caption = sprintf("Correlation = %.2f", corr_4a)) +
  guides(size = "none") + theme_bw()
ggsave(file.path(out_dir, "fig4a_regpolitics.pdf"), p4a, width = 6, height = 5)
print(p4a)

p4b <- ggplot(fig4b, aes(x = trump_percent, y = dlreg, weight = totpop)) +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", alpha = 0.3) +
  geom_point(aes(size = totpop), shape = 1, alpha = 0.7) +
  labs(x = "GOP Vote Share", y = "Regulatory Growth (2020-2022)",
       subtitle = "Panel B: Regulation Growth and Politics",
       caption = sprintf("Correlation = %.2f", corr_4b)) +
  guides(size = "none") + theme_bw()
ggsave(file.path(out_dir, "fig4b_growthpolitics.pdf"), p4b, width = 6, height = 5)
print(p4b)

# -- Figure 5: Benchmarking with SOLI --
soli <- as.data.table(read_dta(file.path(data_dir, "licensing data/MasterData_SOLI2023.dta")))
bench_reg <- soc_state[year %in% c(2021, 2022), .(reg_restrict = sum(reg_restrict)), by = st]
bench <- merge(bench_reg, soli[, .(st, BarrierScore, LicensesScore)], by = "st")
bench[, lreg := log(reg_restrict)]

corr_5a <- cor(bench$lreg, bench$BarrierScore, use = "complete.obs")
corr_5b <- cor(bench$lreg, bench$LicensesScore, use = "complete.obs")
cat(sprintf("  Fig 5 correlations: barrier = %.2f, licenses = %.2f\n", corr_5a, corr_5b))

p5a <- ggplot(bench, aes(x = BarrierScore, y = lreg)) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_point(shape = 1, size = 2) +
  labs(x = "Trudeau and Timmons (2023)", y = "log(Regulatory Restrictions)",
       subtitle = "Panel A: Barrier Score",
       caption = sprintf("Correlation = %.2f", corr_5a)) + theme_bw()
ggsave(file.path(out_dir, "fig5a_barrier.pdf"), p5a, width = 6, height = 5)
print(p5a)

p5b <- ggplot(bench, aes(x = LicensesScore, y = lreg)) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_point(shape = 1, size = 2) +
  labs(x = "Trudeau and Timmons (2023)", y = "log(Regulatory Restrictions)",
       subtitle = "Panel B: Licenses Score",
       caption = sprintf("Correlation = %.2f", corr_5b)) + theme_bw()
ggsave(file.path(out_dir, "fig5b_licenses.pdf"), p5b, width = 6, height = 5)
print(p5b)

# ---- Table 2: OEWS regressions ----
log_step(7, "Table 2: OEWS Regressions")

oews_m <- merge(oews_agg, soc_state[, .(st, soc2, year, lreg_restrict)],
                by = c("st", "soc2", "year"), all.x = FALSE)
oews_m <- merge(oews_m, census_agg[, .(st, soc2, famsize, nchild,
                                        age_c = age, male_c = male,
                                        married_c = married,
                                        race_white, race_black)],
                by = c("st", "soc2"), all.x = TRUE)

oews_m[, lTOT_EMP := log(TOT_EMP)]
oews_m[, lH_MEDIAN := log(H_MEDIAN)]
oews_m[, lH_PCT75 := log(H_PCT75)]
oews_m[, lH_PCT25 := log(H_PCT25)]

m1  <- feols(lTOT_EMP  ~ lreg_restrict,              data = oews_m, cluster = ~st)
m2  <- feols(lTOT_EMP  ~ lreg_restrict | st + year,  data = oews_m, cluster = ~st)
m3  <- feols(lTOT_EMP  ~ lreg_restrict | st^year,    data = oews_m, cluster = ~st)
m4  <- feols(lH_MEDIAN ~ lreg_restrict,              data = oews_m, cluster = ~st)
m5  <- feols(lH_MEDIAN ~ lreg_restrict | st + year,  data = oews_m, cluster = ~st)
m6  <- feols(lH_MEDIAN ~ lreg_restrict | st^year,    data = oews_m, cluster = ~st)
m7  <- feols(lH_PCT75  ~ lreg_restrict,              data = oews_m, cluster = ~st)
m8  <- feols(lH_PCT75  ~ lreg_restrict | st + year,  data = oews_m, cluster = ~st)
m9  <- feols(lH_PCT75  ~ lreg_restrict | st^year,    data = oews_m, cluster = ~st)
m10 <- feols(lH_PCT25  ~ lreg_restrict,              data = oews_m, cluster = ~st)
m11 <- feols(lH_PCT25  ~ lreg_restrict | st + year,  data = oews_m, cluster = ~st)
m12 <- feols(lH_PCT25  ~ lreg_restrict | st^year,    data = oews_m, cluster = ~st)

tab2 <- etable(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12,
               se.below = TRUE,
               signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
               fitstat = ~ r2 + n,
               dict = c(lreg_restrict = "log(regulation)"))
print(tab2)

etable(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12,
       se.below = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       fitstat = ~ r2 + n,
       dict = c(lreg_restrict = "log(regulation)"),
       file = file.path(out_dir, "table2_oews.tex"), replace = TRUE)

# ---- Table 3: CPS regressions ----
log_step(8, "Table 3: CPS Individual-Level Regressions")

cps[, learnweek := log(earnweek)]
cps[, lregst_restrict := log(regst_restrict)]
cps[, lregst_x_lic := lregst_restrict * has_procert_license]
cps[, lreg_x_lic := lreg_restrict * has_procert_license]
cps[, indid := as.factor(ind1990)]

cps_emp  <- cps[!is.na(has_procert_license) & !is.na(employed) & !is.na(lregst_restrict)]
cps_earn <- cps[employed == 1 & !is.na(learnweek) & is.finite(learnweek) &
                  !is.na(lreg_restrict) & !is.na(has_procert_license)]

t3_1 <- feols(employed ~ lregst_restrict + has_procert_license + lregst_x_lic,
              data = cps_emp, weights = ~wtfinl, cluster = ~st)
t3_2 <- feols(employed ~ lregst_restrict + has_procert_license + lregst_x_lic +
                male + race_white_nonhispan + race_white_hispan + race_black +
                married + yrschool + famsize + age,
              data = cps_emp, weights = ~wtfinl, cluster = ~st)
t3_3 <- feols(employed ~ lregst_restrict + has_procert_license + lregst_x_lic +
                male + race_white_nonhispan + race_white_hispan + race_black +
                married + yrschool + famsize + age | st + indid + month + year,
              data = cps_emp, weights = ~wtfinl, cluster = ~st)

t3_4 <- feols(learnweek ~ lreg_restrict + has_procert_license + lreg_x_lic,
              data = cps_earn, weights = ~earnwt, cluster = ~st)
t3_5 <- feols(learnweek ~ lreg_restrict + has_procert_license + lreg_x_lic +
                male + race_white_nonhispan + race_white_hispan + race_black +
                married + yrschool + famsize + age,
              data = cps_earn, weights = ~earnwt, cluster = ~st)
t3_6 <- feols(learnweek ~ lreg_restrict + has_procert_license + lreg_x_lic +
                male + race_white_nonhispan + race_white_hispan + race_black +
                married + yrschool + famsize + age | st + indid + month + year,
              data = cps_earn, weights = ~earnwt, cluster = ~st)

tab3 <- etable(t3_1, t3_2, t3_3, t3_4, t3_5, t3_6,
               se.below = TRUE,
               signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
               fitstat = ~ r2 + n,
               dict = c(has_procert_license = "Has professional license",
                        lregst_restrict = "log(state restrictions)",
                        lregst_x_lic = "x Has professional license",
                        lreg_restrict = "log(State x Occ restrictions)",
                        lreg_x_lic = "x has professional license"))
print(tab3)

etable(t3_1, t3_2, t3_3, t3_4, t3_5, t3_6,
       se.below = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       fitstat = ~ r2 + n,
       dict = c(has_procert_license = "Has professional license",
                lregst_restrict = "log(state restrictions)",
                lregst_x_lic = "x Has professional license",
                lreg_restrict = "log(State x Occ restrictions)",
                lreg_x_lic = "x has professional license"),
       file = file.path(out_dir, "table3_cps.tex"), replace = TRUE)
