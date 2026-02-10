library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(sjlabelled)
library(lubridate)

#----------------------------
# 1) Download + read files
#----------------------------
tdir <- tempdir()
zip_path <- file.path(tdir, "njmin.zip")

download.file(
  url      = "http://davidcard.berkeley.edu/data_sets/njmin.zip",
  destfile = zip_path,
  mode     = "wb"
)
unzip(zip_path, exdir = tdir)

codebook_path <- file.path(tdir, "codebook")
data_path     <- file.path(tdir, "public.dat")

codebook <- read_lines(codebook_path)

#----------------------------
# 2) Parse variable names + labels 
#----------------------------
cb_block <- codebook[8:59] %>% 
  (\(x) x[-c(5, 6, 13, 14, 32, 33)])()

var_names <- cb_block %>%
  str_sub(1, 8) %>%
  str_squish() %>%
  str_to_lower()

var_labels <- cb_block %>%
  str_replace(".*\\.[0-9]", "") %>%
  (\(x) x[-c(5:10)])() %>%
  str_squish()

# Fix the one weird label
var_labels[41] <- "region of restaurant"

#----------------------------
# 3) Read raw + basic cleaning
#----------------------------
data_raw <- read_table2(data_path, col_names = FALSE)

data_mod <- data_raw %>%
  select(-X47) %>%
  `colnames<-`(var_names) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(sheet = if_else(sheet == 407 & chain == 4, 408, sheet)) %>%
  set_label(var_labels)

#----------------------------
# 4) Reshape to long (wave1/wave2 in one shot)
#    We keep structure vars fixed and pivot all others by suffix "2"
#----------------------------
id_vars <- c("sheet", "chain", "co_owned", "state", "southj", "centralj", "northj", "shore", "pa1", "pa2")

card_krueger_1994 <- data_mod %>% 
  pivot_longer(
    cols = -all_of(id_vars),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*?)(2?)$"
  ) %>%
  mutate(
    observation = if_else(wave == "", "February 1992", "November 1992")
  ) %>%
  select(-wave) %>%
  # Value labels (do AFTER reshape so you do it once)
  mutate(
    chain = recode(chain, `1` = "bk", `2` = "kfc", `3` = "roys", `4` = "wendys"),
    state = recode(state, `1` = "New Jersey", `0` = "Pennsylvania"),
    region = case_when(
      southj == 1   ~ "southj",
      centralj == 1 ~ "centralj",
      northj == 1   ~ "northj",
      shore == 1    ~ "shorej",
      pa1 == 1      ~ "phillypa",
      pa2 == 1      ~ "eastonpa",
      TRUE          ~ NA_character_
    ),
    co_owned = if_else(co_owned == 1, "yes", "no"),
    bonus    = if_else(bonus == 1, "yes", "no"),
    special  = if_else(special == 1, "yes", "no"),   # wave1 variable name after pivot
    type     = if_else(type == 1, "phone", "personal"),
    meals = recode(meals,
                   `0` = "none",
                   `1` = "free meals",
                   `2` = "reduced price meals",
                   `3` = "both free and reduced price meals"
    ),
    status = recode(status,
                    `0` = "refused second interview",
                    `1` = "answered 2nd interview",
                    `2` = "closed for renovations",
                    `3` = "closed permanently",
                    `4` = "closed for highway construction",
                    `5` = "closed due to Mall fire"
    ),
    date = mdy(date)
  ) %>%
  select(-southj, -centralj, -northj, -shore, -pa1, -pa2) %>%
  copy_labels(data_mod)

#----------------------------
# 5) Final variables
#----------------------------
card_krueger_1994_mod <- card_krueger_1994 %>%
  mutate(
    emptot  = empft + nmgrs + 0.5 * emppt,
    pct_fte = empft / emptot * 100
  )
