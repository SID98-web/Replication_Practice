library(haven)
library(fixest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(modelsummary)
library(scales)
library(patchwork)

# Set working directory to the folder containing this script before running,
# or use here::here() if the here package is available.
# e.g. setwd("/path/to/Replication file 2")
data_dir <- file.path("Data")
fig_dir  <- file.path("Figures")
tab_dir  <- file.path("Tables")

dir.create(tab_dir, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE)


# Country ordering (matches Stata originalcountry encoding)
country_order <- c("Brazil", "Canada", "USA", "Austria", "France", "Germany",
                   "Greece", "Hungary", "Italy", "Netherlands", "Poland",
                   "Serbia", "Spain", "Sweden", "Turkey", "UK", "Ukraine",
                   "China", "India", "Japan", "Korea", "Malaysia", "Russia",
                   "Singapore", "Taiwan", "Egypt", "Australia")

continent_map <- c(
  "Brazil" = "Americas", "Canada" = "Americas", "USA" = "Americas",
  "Austria" = "Europe", "France" = "Europe", "Germany" = "Europe",
  "Greece" = "Europe", "Hungary" = "Europe", "Italy" = "Europe",
  "Netherlands" = "Europe", "Poland" = "Europe", "Serbia" = "Europe",
  "Spain" = "Europe", "Sweden" = "Europe", "Turkey" = "Europe",
  "UK" = "Europe", "Ukraine" = "Europe",
  "China" = "Asia", "India" = "Asia", "Japan" = "Asia", "Korea" = "Asia",
  "Malaysia" = "Asia", "Russia" = "Asia", "Singapore" = "Asia", "Taiwan" = "Asia",
  "Egypt" = "Rest", "Australia" = "Rest"
)

continent_colors <- c("Average" = "black", "Americas" = "#800000",
                      "Europe" = "navy", "Asia" = "darkgreen", "Rest" = "sienna")


# Load data
df <- read_dta(file.path(data_dir, "G-SWA.dta"))

# Convert labeled vars to factors for fixest FE
df$gender_f      <- factor(df$gender)
df$agegroups_f   <- factor(df$agegroups)
df$education_f   <- factor(df$education)
df$industry_f    <- factor(df$industry_job)
df$wave_f        <- factor(df$wave)
df$country_f     <- factor(df$country)
df$origcountry_f <- factor(df$originalcountry)

nrow(df)


# =============================================================================
# TABLES
# =============================================================================

# Table 1: Structure of Preferences over WFH
sub1 <- df[!is.na(df$commute_time_hs), ]

t1_m1 <- feols(value_WFH_rawpercent25 ~ tertiary + graduate + married + male + with_kids + male_with_kids | wave_f + agegroups_f,
               data = sub1, cluster = ~original_country)
t1_m2 <- feols(value_WFH_rawpercent25 ~ tertiary + graduate + married + male + with_kids + male_with_kids + commute_time_hs | wave_f + agegroups_f,
               data = sub1, cluster = ~original_country)
t1_m3 <- feols(value_WFH_rawpercent25 ~ tertiary + graduate + married + male + with_kids + male_with_kids + commute_time_hs | wave_f + agegroups_f + original_country,
               data = sub1, cluster = ~original_country)
t1_m4 <- feols(value_WFH_rawpercent25 ~ tertiary + graduate + married + with_kids + commute_time_hs | wave_f + agegroups_f + original_country,
               data = sub1[sub1$male == 1, ], cluster = ~original_country)
t1_m5 <- feols(value_WFH_rawpercent25 ~ tertiary + graduate + married + with_kids + commute_time_hs | wave_f + agegroups_f + original_country,
               data = sub1[sub1$male == 0, ], cluster = ~original_country)

modelsummary(list("(1)" = t1_m1, "(2)" = t1_m2, "(3)" = t1_m3, "(4)" = t1_m4, "(5)" = t1_m5),
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "Table1.html"),
             title = "Table 1: The Structure of Preferences over WFH")

# Dep var means
with(sub1[!is.na(sub1$married) & !is.na(sub1$male_with_kids), ],
     summary(value_WFH_rawpercent25))


# Table 2 and related tables: Lockdown effects
outcomes <- c("n_work_home", "daysemployee_work_home", "daysemployer_work_home", "value_WFH_rawpercent25")

df2 <- df
df2$reg_deaths_pc <- ifelse(is.na(df2$reg_deaths_pc), df2$deaths_pc, df2$reg_deaths_pc)
df2$reg_LSI <- ifelse(is.na(df2$subn_LSI), df2$LSI, df2$subn_LSI)

# Standardize
df2$deathspc_std    <- scale(df2$deaths_pc)[,1]
df2$LSI_std         <- scale(df2$LSI)[,1]
df2$oxf_LSI_std     <- scale(df2$oxf_LSI)[,1]
df2$deathsregpc_std <- scale(df2$reg_deaths_pc)[,1]
df2$LSIreg_std      <- scale(df2$reg_LSI)[,1]
df2$mask_std        <- scale(df2$mask)[,1]
df2$log_gdp         <- log(df2$gdppc2019)

# Factor vars for FE
df2$gender_f    <- factor(df2$gender)
df2$agegroups_f <- factor(df2$agegroups)
df2$education_f <- factor(df2$education)
df2$industry_f  <- factor(df2$industry_job)
df2$wave_f      <- factor(df2$wave)

# Table 2
t2_models <- list()
for (y in outcomes) {
  f <- as.formula(paste0(y, " ~ LSI_std + deathspc_std + log_gdp | industry_f + education_f + gender_f + agegroups_f + wave_f"))
  t2_models[[y]] <- feols(f, data = df2, cluster = ~original_country)
}

modelsummary(t2_models,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             coef_map = c("LSI_std" = "Cum. Lockdown Stringency (std.)",
                          "deathspc_std" = "Cum. COVID-19 deaths per capita (std.)"),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "Table2.html"),
             title = "Table 2: WFH rises with lockdown stringency")


# Table 3A: More educated (education 3 or 4)
t3a_models <- list()
for (y in outcomes) {
  f <- as.formula(paste0(y, " ~ LSI_std + deathspc_std + log_gdp | industry_f + education_f + gender_f + agegroups_f + wave_f"))
  t3a_models[[y]] <- feols(f, data = df2[df2$education %in% c(3, 4), ], cluster = ~original_country)
}

modelsummary(t3a_models,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             coef_map = c("LSI_std" = "Cum. Lockdown Stringency (std.)",
                          "deathspc_std" = "Cum. COVID-19 deaths per capita (std.)"),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "Table3A.html"),
             title = "Table 3A: Lockdown effects, Tertiary+ educated")


# Table 3B: Graduate only (education 4)
t3b_models <- list()
for (y in outcomes) {
  f <- as.formula(paste0(y, " ~ LSI_std + deathspc_std + log_gdp | industry_f + education_f + gender_f + agegroups_f + wave_f"))
  t3b_models[[y]] <- feols(f, data = df2[df2$education == 4, ], cluster = ~original_country)
}

modelsummary(t3b_models,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             coef_map = c("LSI_std" = "Cum. Lockdown Stringency (std.)",
                          "deathspc_std" = "Cum. COVID-19 deaths per capita (std.)"),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "Table3B.html"),
             title = "Table 3B: Lockdown effects, Graduate educated")


# Table A.5: Adding mask mandate controls
ta5_models <- list()
for (y in outcomes) {
  f <- as.formula(paste0(y, " ~ LSI_std + deathspc_std + mask_std + log_gdp | industry_f + education_f + gender_f + agegroups_f + wave_f"))
  ta5_models[[y]] <- feols(f, data = df2, cluster = ~original_country)
}

modelsummary(ta5_models,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             coef_map = c("LSI_std" = "Cum. Lockdown Stringency (std.)",
                          "deathspc_std" = "Cum. COVID-19 deaths per capita (std.)",
                          "mask_std" = "Cum. Mask Mandate Orders (std.)"),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "TableA5.html"),
             title = "Table A.5: Adding mask mandates")


# Table A.6: Subnational variation
ta6_models <- list()
for (y in outcomes) {
  f <- as.formula(paste0(y, " ~ LSIreg_std + deathsregpc_std + log_gdp | industry_f + education_f + gender_f + agegroups_f + wave_f"))
  ta6_models[[y]] <- feols(f, data = df2, cluster = ~original_country)
}

modelsummary(ta6_models,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             coef_map = c("LSIreg_std" = "Cum. subnational Lockdown Stringency (std.)",
                          "deathsregpc_std" = "Cum. subnational COVID-19 deaths per capita (std.)"),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "TableA6.html"),
             title = "Table A.6: Subnational variation")


# Table A.7: Oxford stringency index
ta7_models <- list()
for (y in outcomes) {
  f <- as.formula(paste0(y, " ~ oxf_LSI_std + deathspc_std + log_gdp | industry_f + education_f + gender_f + agegroups_f + wave_f"))
  ta7_models[[y]] <- feols(f, data = df2, cluster = ~original_country)
}

modelsummary(ta7_models,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             coef_map = c("oxf_LSI_std" = "Cum. Oxford Stringency (std.)",
                          "deathspc_std" = "Cum. COVID-19 deaths per capita (std.)"),
             gof_map = c("nobs", "r.squared"),
             output = file.path(tab_dir, "TableA7.html"),
             title = "Table A.7: Oxford stringency index")


# Table A.1: Country-level survey waves
ta1 <- df %>%
  group_by(wave, original_country) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = wave, values_from = n, names_prefix = "Wave_") %>%
  arrange(original_country)

write.csv(ta1, file.path(tab_dir, "TableA1.csv"), row.names = FALSE)


# Table A.2: Country-level summary stats
outcomes_a2 <- c("age", "n_work_home", "daysemployer_work_home", "daysemployee_work_home",
                 "WFH_expectations1", "commuting_time", "value_WFH_rawpercent25", "WFHperceptions")

ta2 <- df %>%
  group_by(original_country) %>%
  summarise(across(all_of(outcomes_a2), ~mean(.x, na.rm=T)), .groups = "drop") %>%
  arrange(original_country)

write.csv(ta2, file.path(tab_dir, "TableA2.csv"), row.names = FALSE)


# Table A.3: Country-level percentages
df_a3 <- df %>%
  mutate(
    fem = (gender == 1) * 100,
    sec = (education == 2) * 100,
    tert = (education == 3) * 100,
    grad = (education == 4) * 100,
    with_kids1 = 100 * with_kids,
    commute_20 = (commuting_time <= 20) * 100,
    commute_60 = (commuting_time >= 60) * 100
  )

ta3 <- df_a3 %>%
  group_by(original_country) %>%
  summarise(across(c(fem, sec, tert, grad, with_kids1, commute_20, commute_60),
                   ~mean(.x, na.rm=T)),
            .groups = "drop") %>%
  arrange(original_country)

write.csv(ta3, file.path(tab_dir, "TableA3.csv"), row.names = FALSE)


# Table A.4: Comparison with Gallup (G-SWA side only)
df_a4 <- df_a3 %>%
  mutate(tertmore = tert + grad)

ta4 <- df_a4 %>%
  group_by(original_country) %>%
  summarise(fem = mean(fem, na.rm=T),
            age = mean(age, na.rm=T),
            sec = mean(sec, na.rm=T),
            tertmore = mean(tertmore, na.rm=T),
            .groups = "drop") %>%
  arrange(original_country)

write.csv(ta4, file.path(tab_dir, "TableA4.csv"), row.names = FALSE)


# =============================================================================
# FIGURES
# =============================================================================

# Helper: extract country-level adjusted means from regression
get_country_means <- function(model, ref_mean, countries = country_order) {
  cc <- coef(model)
  vals <- numeric(length(countries))
  names(vals) <- countries

  for (i in seq_along(countries)) {
    cname <- paste0("originalcountry::", i)
    if (cname %in% names(cc)) {
      vals[i] <- cc[cname] + ref_mean
    } else {
      vals[i] <- ref_mean  # base category (USA)
    }
  }
  vals
}

# Helper: build plot data frame
make_bar_df <- function(vals, countries = country_order) {
  avg_val <- mean(vals)

  plot_df <- data.frame(
    country = c("Average", countries),
    value = c(avg_val, vals),
    continent = c("Average", continent_map[countries]),
    stringsAsFactors = FALSE
  )
  plot_df$continent <- factor(plot_df$continent, levels = c("Average", "Americas", "Europe", "Asia", "Rest"))
  plot_df$country <- factor(plot_df$country, levels = rev(plot_df$country))
  plot_df
}

# Helper: horizontal bar chart
plot_bars <- function(plot_df, xlabel, dec = 1, figname) {
  fmt <- paste0("%.", dec, "f")
  plot_df$label <- sprintf(fmt, plot_df$value)

  xmax <- max(plot_df$value, na.rm=T) * 1.15

  p <- ggplot(plot_df, aes(x = value, y = country, fill = continent)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = label), hjust = -0.1, size = 2.8) +
    scale_fill_manual(values = continent_colors) +
    facet_grid(continent ~ ., scales = "free_y", space = "free_y", switch = "y") +
    xlim(0, xmax) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "none",
      strip.text.y.left = element_text(angle = 0, face = "bold", size = 8),
      strip.background = element_rect(fill = "grey95"),
      strip.placement = "outside",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(x = xlabel, y = NULL)

  ggsave(file.path(fig_dir, figname), p, width = 8, height = 10, dpi = 150)
  p
}


# Figures 1-4, A2, A5, A6: Country bar charts from regression
fig_vars <- c("n_work_home", "daysemployer_work_home", "value_WFH_rawpercent25",
              "commuting_time", "daysemployee_work_home", "WFH_expectations1", "WFHperceptions")

fig_labels <- c(
  n_work_home = "Number of days working from home this week",
  daysemployer_work_home = "Number of planned full workdays at home",
  value_WFH_rawpercent25 = "Amenity value of WFH option (% of pay)",
  commuting_time = "Time spent commuting to work, in minutes",
  daysemployee_work_home = "Number of desired full workdays at home",
  WFH_expectations1 = "WFH productivity during COVID relative to expectations, percent",
  WFHperceptions = "Percentage change in work from home perceptions"
)

fig_nums <- c(
  n_work_home = "1", daysemployer_work_home = "2", value_WFH_rawpercent25 = "3",
  commuting_time = "4", daysemployee_work_home = "A2",
  WFH_expectations1 = "A5", WFHperceptions = "A6"
)

fig_decs <- c(
  n_work_home = 1, daysemployer_work_home = 1, value_WFH_rawpercent25 = 1,
  commuting_time = 0, daysemployee_work_home = 1,
  WFH_expectations1 = 1, WFHperceptions = 0
)

for (v in fig_vars) {
  ref_mean <- mean(df[[v]][df$original_country == "USA"], na.rm=T)

  m <- feols(as.formula(paste0(v, " ~ i(originalcountry, ref=3) | gender_f + agegroups_f + education_f + industry_f + wave_f")),
             data = df, cluster = ~country)

  vals <- get_country_means(m, ref_mean)
  pdf <- make_bar_df(vals)
  plot_bars(pdf, fig_labels[v], fig_decs[v], paste0("Figure ", fig_nums[v], ".png"))
}


# Figures A.7 and A.8: LSI and Deaths (weighted country means, not regression)
df$deaths_100 <- 100000 * df$deaths_pc

# Compute inverse of country-wave group size for weighting
df <- df %>%
  group_by(country) %>%
  mutate(nn = 1 / n()) %>%
  ungroup()

for (v in c("LSI", "deaths_100")) {
  # Weighted mean by originalcountry
  vals <- numeric(27)
  for (i in 1:27) {
    sub <- df[df$originalcountry == i, ]
    vals[i] <- weighted.mean(sub[[v]], sub$nn, na.rm=T)
  }
  names(vals) <- country_order

  pdf <- make_bar_df(vals)

  xlabel <- ifelse(v == "LSI", "Cumulative Lockdown Stringency",
                   "Cumulative COVID-19 deaths per 100,000 habitants")
  dec <- 0
  fignum <- ifelse(v == "LSI", "A7", "A8")

  plot_bars(pdf, xlabel, dec, paste0("Figure ", fignum, ".png"))
}


# Figure 5: Women more highly value WFH in most countries
# Run separate regressions by gender
vals_by_gender <- list()
for (g in 1:2) {
  sub <- df[df$gender == g, ]
  ref_mean <- mean(sub$value_WFH_rawpercent25[sub$original_country == "USA"], na.rm=T)

  m <- feols(value_WFH_rawpercent25 ~ i(originalcountry, ref=3) | gender_f + agegroups_f + education_f + industry_f + wave_f,
             data = sub, cluster = ~country)

  vals_by_gender[[g]] <- get_country_means(m, ref_mean)
}

fig5_df <- data.frame(
  country = country_order,
  male = vals_by_gender[[2]],
  female = vals_by_gender[[1]]
)

p5 <- ggplot(fig5_df, aes(x = male, y = female)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  geom_point(shape = 18, size = 3, color = "navy") +
  geom_text(aes(label = country), size = 2.5, hjust = -0.15, vjust = 0.5) +
  theme_bw(base_size = 11) +
  labs(x = "Men", y = "Women",
       title = "Amenity value of WFH option by gender") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "Figure 5.png"), p5, width = 7, height = 6, dpi = 150)


# Figure 6: How the amenity value differs by sex and family
df6 <- df
df6$groups <- NA_integer_
df6$groups[df6$gender == 2 & df6$with_kids == 1 & df6$married == 1] <- 1L  # married men w/ kids
df6$groups[df6$gender == 2 & df6$with_kids == 0 & df6$married == 1] <- 2L  # married men w/o kids
df6$groups[df6$gender == 1 & df6$with_kids == 1 & df6$married == 1] <- 3L  # married women w/ kids
df6$groups[df6$gender == 1 & df6$with_kids == 0 & df6$married == 1] <- 4L  # married women w/o kids
df6$groups[df6$gender == 2 & df6$with_kids == 0 & df6$married == 0] <- 5L  # single men
df6$groups[df6$gender == 1 & df6$with_kids == 0 & df6$married == 0] <- 6L  # single women

# Drop countries with <50 obs in any subgroup within a pair
for (pair in list(c(1,2), c(3,4), c(5,6))) {
  obs_check <- df6 %>%
    filter(groups %in% pair) %>%
    group_by(groups, original_country) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(too_small = n <= 50)

  bad_countries <- obs_check %>%
    group_by(original_country) %>%
    filter(any(too_small)) %>%
    pull(original_country) %>%
    unique()

  df6$groups[df6$groups %in% pair & df6$original_country %in% bad_countries] <- NA_integer_
}

df6$gender_f    <- factor(df6$gender)
df6$agegroups_f <- factor(df6$agegroups)
df6$education_f <- factor(df6$education)
df6$industry_f  <- factor(df6$industry_job)
df6$wave_f      <- factor(df6$wave)

# Get adjusted means for each group
group_vals <- list()
for (g in 1:6) {
  sub <- df6[df6$groups == g & !is.na(df6$groups), ]

  if (nrow(sub) < 100) next

  ref_mean <- mean(sub$value_WFH_rawpercent25[sub$original_country == "USA"], na.rm=T)

  countries_in <- sort(unique(sub$originalcountry))
  ref_code <- 3  # USA

  m <- feols(value_WFH_rawpercent25 ~ i(originalcountry, ref=3) | gender_f + agegroups_f + education_f + industry_f + wave_f,
             data = sub, cluster = ~country)

  cc <- coef(m)
  vals <- numeric(27)
  names(vals) <- country_order
  vals[] <- NA_real_

  for (i in countries_in) {
    cname <- paste0("originalcountry::", i)
    if (cname %in% names(cc)) {
      vals[i] <- cc[cname] + ref_mean
    } else if (i == 3) {
      vals[i] <- ref_mean
    }
  }

  group_vals[[g]] <- vals
}

# Panel A: Married men with vs without children
f6a_df <- data.frame(
  country = country_order,
  with_kids = group_vals[[1]],
  without_kids = group_vals[[2]]
) %>% filter(!is.na(with_kids) & !is.na(without_kids))

p6a <- ggplot(f6a_df, aes(x = without_kids, y = with_kids)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  geom_point(shape = 18, size = 3, color = "navy") +
  geom_text(aes(label = country), size = 2.5, hjust = -0.15) +
  theme_bw(base_size = 11) +
  labs(x = "Married men without children", y = "Married men with children") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "Figure 6A.png"), p6a, width = 7, height = 6, dpi = 150)


# Panel B: Married women with vs without children
f6b_df <- data.frame(
  country = country_order,
  with_kids = group_vals[[3]],
  without_kids = group_vals[[4]]
) %>% filter(!is.na(with_kids) & !is.na(without_kids))

p6b <- ggplot(f6b_df, aes(x = without_kids, y = with_kids)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  geom_point(shape = 18, size = 3, color = "navy") +
  geom_text(aes(label = country), size = 2.5, hjust = -0.15) +
  theme_bw(base_size = 11) +
  labs(x = "Married women without children", y = "Married women with children") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "Figure 6B.png"), p6b, width = 7, height = 6, dpi = 150)


# Panel C: Single men vs women
f6c_df <- data.frame(
  country = country_order,
  women = group_vals[[6]],
  men = group_vals[[5]]
) %>% filter(!is.na(women) & !is.na(men))

p6c <- ggplot(f6c_df, aes(x = men, y = women)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  geom_point(shape = 18, size = 3, color = "navy") +
  geom_text(aes(label = country), size = 2.5, hjust = -0.15) +
  theme_bw(base_size = 11) +
  labs(x = "Single men", y = "Single women") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "Figure 6C.png"), p6c, width = 7, height = 6, dpi = 150)


# Figure 8: Planned WFH rises with productivity surprise
fig8_df <- df %>%
  group_by(WFH_expectations1) %>%
  summarise(daysemployer = mean(daysemployer_work_home, na.rm=T), .groups = "drop") %>%
  filter(!is.na(WFH_expectations1))

p8 <- ggplot(fig8_df, aes(x = WFH_expectations1, y = daysemployer)) +
  geom_point(shape = 18, size = 3, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.7) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  scale_y_continuous(breaks = seq(0, 2, 0.5)) +
  theme_bw(base_size = 11) +
  labs(x = "Relative to expectations, WFH Productivity during COVID (%)",
       y = "Number of planned full workdays at home") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "Figure 8.png"), p8, width = 7, height = 5, dpi = 150)


# Figure A.1: Histogram of WFH amenity value with residual density
m_a1 <- feols(value_WFH_rawpercent25 ~ i(originalcountry, ref=3) | gender_f + agegroups_f + education_f + industry_f + wave_f,
              data = df, cluster = ~original_country)

df$resid_a1 <- NA_real_
idx <- which(!is.na(df$value_WFH_rawpercent25))
df$resid_a1[idx] <- residuals(m_a1, type = "response")
overall_mean <- mean(df$value_WFH_rawpercent25, na.rm=T)
df$resid_centered <- df$resid_a1 + overall_mean

pa1 <- ggplot() +
  geom_histogram(data = df[!is.na(df$value_WFH_rawpercent25), ],
                 aes(x = value_WFH_rawpercent25, y = after_stat(density)),
                 bins = 30, fill = "steelblue", alpha = 0.5) +
  geom_density(data = df[!is.na(df$resid_centered), ],
               aes(x = resid_centered), color = "red", linewidth = 0.8) +
  theme_bw(base_size = 11) +
  labs(x = "Amenity value of the option to WFH 2-3 days",
       y = "Density") +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "FigureA.1.png"), pa1, width = 7, height = 5, dpi = 150)


# Figure A.3: Quit or look for WFH job if required to return
df_a3fig <- df %>%
  filter(!is.na(return_office) & n_work_home > 0 & !is.na(n_work_home))

df_a3fig$notreturn_look <- 100 * (df_a3fig$return_office == 2)
df_a3fig$notreturn_quit <- 100 * (df_a3fig$return_office == 3)

# Countries in wave 2 only (where return_office is available)
w2_countries <- sort(unique(df_a3fig$original_country))

# Run regressions for "look for WFH job" and "quit"
# Need to find which countries are in this subsample and their originalcountry codes
w2_codes <- sort(unique(df_a3fig$originalcountry))
df_a3fig$gender_f    <- factor(df_a3fig$gender)
df_a3fig$agegroups_f <- factor(df_a3fig$agegroups)
df_a3fig$education_f <- factor(df_a3fig$education)
df_a3fig$industry_f  <- factor(df_a3fig$industry_job)

for (v in c("notreturn_look", "notreturn_quit")) {
  ref_mean <- mean(df_a3fig[[v]][df_a3fig$original_country == "USA"], na.rm=T)

  m <- feols(as.formula(paste0(v, " ~ i(originalcountry, ref=3) | gender_f + agegroups_f + education_f + industry_f")),
             data = df_a3fig, cluster = ~country)

  cc <- coef(m)
  vals <- numeric(length(w2_codes))
  names(vals) <- country_order[w2_codes]

  for (i in seq_along(w2_codes)) {
    code <- w2_codes[i]
    cname <- paste0("originalcountry::", code)
    if (cname %in% names(cc)) {
      vals[i] <- cc[cname] + ref_mean
    } else {
      vals[i] <- ref_mean
    }
  }

  assign(paste0("vals_", v), vals)
}

# Stacked bar: quit + look
a3_df <- data.frame(
  country = names(vals_notreturn_quit),
  quit = vals_notreturn_quit,
  look = vals_notreturn_look - vals_notreturn_quit
) %>%
  mutate(total = quit + look) %>%
  arrange(desc(total))

a3_avg <- data.frame(country = "Average", quit = mean(vals_notreturn_quit),
                     look = mean(vals_notreturn_look) - mean(vals_notreturn_quit),
                     total = mean(vals_notreturn_look))

a3_df <- bind_rows(a3_avg, a3_df)

# Assign continents
a3_df$continent <- ifelse(a3_df$country == "Average", "Average", continent_map[a3_df$country])
a3_df$continent <- factor(a3_df$continent, levels = c("Average", "Americas", "Europe", "Asia", "Rest"))

# Reorder by continent then position
a3_order <- a3_df %>%
  arrange(continent, match(country, c("Average", country_order))) %>%
  pull(country)
a3_df$country <- factor(a3_df$country, levels = rev(a3_order))

a3_long <- a3_df %>%
  pivot_longer(cols = c(quit, look), names_to = "type", values_to = "value") %>%
  mutate(type = factor(type, levels = c("look", "quit")))

pa3 <- ggplot(a3_long, aes(x = value, y = country, fill = type)) +
  geom_col(width = 0.7) +
  geom_text(data = a3_df, aes(x = total, y = country, label = round(total, 0), fill = NULL),
            hjust = -0.2, size = 2.8) +
  scale_fill_manual(values = c("look" = "navy", "quit" = "steelblue"),
                    labels = c("Look for a\nWFH job", "Quit")) +
  facet_grid(continent ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_bw(base_size = 10) +
  theme(
    legend.position.inside = c(0.85, 0.95),
    legend.text = element_text(size = 8),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 8),
    strip.background = element_rect(fill = "grey95"),
    strip.placement = "outside",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Share of employees that would quit or look for a WFH job (%)",
       y = NULL, fill = NULL)

ggsave(file.path(fig_dir, "Figure A3.png"), pa3, width = 8, height = 10, dpi = 150)


# Figure A.4: Planned WFH by country, scatter with fitted line
df_a4fig <- df %>%
  filter(ever_WFH == 100 & !is.na(WFH_expectations1) & !is.na(daysemployer_work_home))

a4_collapsed <- df_a4fig %>%
  group_by(original_country, WFH_expectations1) %>%
  summarise(daysemployer = mean(daysemployer_work_home, na.rm=T), .groups = "drop")

# Split into 3 panels (alphabetical order, groups of 9)
alpha_countries <- sort(unique(a4_collapsed$original_country))
n_countries <- length(alpha_countries)
panel_size <- ceiling(n_countries / 3)

a4_collapsed$panel <- NA_integer_
for (i in seq_along(alpha_countries)) {
  a4_collapsed$panel[a4_collapsed$original_country == alpha_countries[i]] <- ceiling(i / panel_size)
}

pa4_list <- list()
for (pp in 1:3) {
  sub <- a4_collapsed[a4_collapsed$panel == pp, ]

  pa4_list[[pp]] <- ggplot(sub, aes(x = WFH_expectations1, y = daysemployer)) +
    geom_point(shape = 18, size = 2, color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
    facet_wrap(~original_country, ncol = 3) +
    scale_x_continuous(breaks = c(-25, -15, -5, 0, 5, 15, 25)) +
    scale_y_continuous(breaks = 0:3) +
    theme_bw(base_size = 9) +
    labs(x = "Relative to expectations, WFH Productivity during COVID (%)",
         y = "Number of planned full workdays at home") +
    theme(panel.grid.minor = element_blank())

  ggsave(file.path(fig_dir, paste0("Figure A4_", pp, ".png")), pa4_list[[pp]],
         width = 8, height = 8, dpi = 150)
}
