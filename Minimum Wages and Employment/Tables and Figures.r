library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(sjlabelled)
library(ggrepel)
library(scales)
library(stringr)
library(ggpubr)
library(plm)
library(knitr)
library(kableExtra)
library(lmtest)

#Load Dataset
#card_krueger_1994_mod

######################################################################
#Table 0 : Summary Statistics
######################################################################

card_krueger_1994_mod |>
  psych::describe() |>
  kable(caption = "Table 0: Summary Statistics")|>
  kable_classic()



######################################################################
#Table 1
######################################################################

print(codebook)


# # Pretreatment Means (Wave 1)
# card_krueger_1994_mod %>%
#   filter(observation == "February 1992") %>%
#   group_by(state) %>%
#   summarise(Num_stores = length(sheet),
#             Refused_Second_Interview = sum(status == 0),
#             Num_Closed = sum(status == 3),
#             Num_Renovations = sum(status == 2),
#             Num_Interviewed = sum(status == 1),
#             Num_temp_closed = sum(status == 4),
#             Num_Close_due_to_fire = sum(status == 5)) %>%
#   pivot_longer(cols=-state, names_to = "variable") %>%
#   pivot_wider(names_from = state, values_from = value)



######################################################################
#Table 2
######################################################################

#Distrubution of Restaurents 
card_krueger_1994_mod %>%
  # chain distribution
  count(chain, state) %>%
  group_by(state) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  select(variable = chain, state, p) %>%
  
  # add company-owned row
  bind_rows(
    card_krueger_1994_mod %>%
      group_by(state) %>%
      summarise(p = mean(co_owned == "yes", na.rm = TRUE)) %>%
      mutate(variable = "Company-owned") %>%
      select(variable, state, p)
  ) %>%
  
  # reshape to final table
  pivot_wider(names_from = state, values_from = p) %>%
  mutate(across(-variable, percent, accuracy = 0.1)) %>%
  noquote()

# Pretreatment Means (Wave 1)
card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  group_by(state) %>%
  summarise("FTE employment"  = mean(emptot, na.rm = TRUE),
            "Percentage full-time employees"   = mean(pct_fte, na.rm = TRUE),
            "Starting wage"  = mean(wage_st, na.rm = TRUE),
            "Hours open (weekday) " = mean(hrsopen, na.rm = TRUE),
            "Wage = $4.25 (percentage)" = mean(pctaff, na.rm = TRUE),
            "Price of full meal" =mean(pentree+pfry+psoda,na.rm = TRUE)) %>% 
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

# Post Treatment Means (Wave 2)
card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  group_by(state) %>%
  summarise("FTE employment"  = mean(emptot, na.rm = TRUE),
            "Percentage full-time employees"   = mean(pct_fte, na.rm = TRUE),
            "Starting wage"  = mean(wage_st, na.rm = TRUE),
            "Hours open (weekday) " = mean(hrsopen, na.rm = TRUE),
            "Price of full meal" =mean(pentree+pfry+psoda,na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)  







######################################################################
#Figure 1 
######################################################################
state_colors <- c(
  "New Jersey" = "#1f78b4",      # blue
  "Pennsylvania" = "#e31a1c"     # red
)

#February 1992 histogram 
hist.feb <- card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  ggplot(aes(x = wage_st, fill = state)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count)) * 100),
    bins = 23,
    position = "dodge",
    alpha = 0.6,
    color = "black"
  ) +
  labs(
    title = "February 1992",
    x = "Wage range",
    y = "Percent of stores",
    fill = ""
  ) +
  scale_fill_manual(values = state_colors) +
  theme_minimal()


#November 1992 histogram
hist.nov <- card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  ggplot(aes(x = wage_st, fill = state)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count)) * 100),
    bins = 23,
    position = "dodge",
    alpha = 0.6,
    color = "black"
  ) +
  labs(
    title = "November 1992",
    x = "Wage range",
    y = "Percent of stores",
    fill = ""
  ) +
  scale_fill_manual(values = state_colors) +
  theme_minimal()

#Arrage Panel
ggarrange(
  hist.feb,
  hist.nov,
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)

######################################################################
#Table 3
######################################################################
card_krueger_1994_mod %>%
  group_by(observation, state) %>%
  summarise(
    emptot = mean(emptot, na.rm = TRUE),
    .groups = "drop"
  ) %>%
    pivot_wider(
    names_from = observation,
    values_from = emptot
  ) %>%
    mutate(
    change = `November 1992` - `February 1992`
  ) %>%
    pivot_longer(
    cols = c(`February 1992`, `November 1992`, change),
    names_to = "Label",
    values_to = "value"
  ) %>%
  mutate(
    Label = case_when(
      Label == "February 1992" ~ "FTE employment before",
      Label == "November 1992" ~ "FTE employment after",
      Label == "change" ~ "Change in mean FTE employment"
    )
  ) %>%
    pivot_wider(
    names_from = state,
    values_from = value
  ) %>%
    mutate(
    `Difference (NJ − PA)` = `New Jersey` - Pennsylvania
  ) %>%
    mutate(across(-Label, round, 2))


# Calculate counterfactual outcome
nj_counterfactual <- tibble(
  observation = c("February 1992","November 1992"), 
  state = c("New Jersey (Counterfactual)","New Jersey (Counterfactual)"),
  emptot = as.numeric(c(njfeb, njfeb-(pafeb-panov)))
) 

# Data points for treatment event
intervention <- tibble(
  observation = c("Intervention", "Intervention", "Intervention"),
  state = c("New Jersey", "Pennsylvania", "New Jersey (Counterfactual)"),
  emptot = c(19.35, 22.3, 19.35)
) 

# Combine data
did_plotdata <- bind_rows(differences, 
                          nj_counterfactual, 
                          intervention)


did_plotdata %>%
  mutate(
    observation = factor(
      observation,
      levels = c("February 1992", "Intervention", "November 1992")
    ),
    label = if_else(
      observation == "November 1992",
      as.character(state),
      NA_character_
    )
  ) %>%
  ggplot(
    aes(
      x = observation,
      y = emptot,
      group = state,
      color = state
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = 2,
    linetype = "dotted",
    linewidth = 1.1,
    color = "black"
  ) +
  geom_label_repel(
    aes(label = label),
    na.rm = TRUE,
    box.padding = 0.3,
    point.padding = 0.2,
    min.segment.length = 0,
    seed = 42
  ) +
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(
    limits = c(17, 24),
    expand = expansion(mult = c(0, 0.05))
  ) +
  annotate(
    "text",
    x = "November 1992",
    y = 19.6,
    label = "",
    angle = 90,
    size = 3
  ) +
  labs(
    x = NULL,
    y = "Mean FTE Employment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
######################################################################
#Table 4
######################################################################






######################################################################
#Table 5
######################################################################






######################################################################
#Table 6
######################################################################





######################################################################
#Table 7
######################################################################



######################################################################
#Table 8
######################################################################