#####################################
### SETUP 
#####################################
### Install packages
# install.packages("sf")
# install.packages("geobr")
# install.packages("tidyverse")

### Load packages
library(haven)
library(sf)
library(geobr)
library(tidyverse)

### Import data 
data <- read_dta("/Users/maxglanville/Desktop/Combined.dta")
#data <- read_dta("C:\\Users\\mag1695\\Desktop\\Combined.dta")


#################################### 
### National performance trends 
####################################
kpi_vars <- c(
  "ratio_vaccine",
  "ratio_diabetes",
  "ratio_hypertension",
  "ratio_stdTest",
  "ratio_dental",
  "ratio_prenatalVisit",
  "ratio_papsmear"
)

kpi_trends <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% c("22_1","22_2","22_3",
                       "23_1","23_2","23_3",
                       "24_1","24_2","24_3",
                       "25_1")) %>%
  select(period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")


g1 <- ggplot(kpi_trends, aes(x = period, y = avg_value, color = kpi, group = kpi)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "National KPI Trends (2022–2025)",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

kpi_trends_relative <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% c("22_1","22_2","22_3",
                       "23_1","23_2","23_3",
                       "24_1","24_2","24_3",
                       "25_1")) %>%
  select(period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    period = factor(period, levels = c(
      "22_1","22_2","22_3",
      "23_1","23_2","23_3",
      "24_1","24_2","24_3",
      "25_1"
    )),
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    gap_from_target = avg_value - target
  )

g2 <- ggplot(kpi_trends_relative, aes(x = period, y = gap_from_target, color = kpi, group = kpi)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "National KPI Performance Relative to Target (22_1–25_1)",
    x = "Period",
    y = "Average KPI minus target",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


g1
g2

#################################### 
### National performance trends - BAR CHARTS 
####################################

kpi_vars <- c(
  "ratio_vaccine",
  "ratio_diabetes",
  "ratio_hypertension",
  "ratio_stdTest",
  "ratio_dental",
  "ratio_prenatalVisit",
  "ratio_papsmear"
)

kpi_trends <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% c("22_1","22_2","22_3",
                       "23_1","23_2","23_3",
                       "24_1","24_2","24_3",
                       "25_1")) %>%
  select(period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    period = factor(period, levels = c(
      "22_1","22_2","22_3",
      "23_1","23_2","23_3",
      "24_1","24_2","24_3",
      "25_1"
    ))
  )

kpi_change <- kpi_trends %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(kpi, period, avg_value) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = avg_value
  ) %>%
  mutate(
    change_25_1_vs_22_1 = `25_1` - `22_1`
  )

g1_bar <- ggplot(kpi_change, aes(x = kpi, y = change_25_1_vs_22_1)) +
  geom_col() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in National KPI Levels\n(25_1 minus 22_1)",
    x = "KPI",
    y = "Change in average KPI (25_1 - 22_1)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

g1_bar

kpi_vars <- c(
  "ratio_vaccine",
  "ratio_diabetes",
  "ratio_hypertension",
  "ratio_stdTest",
  "ratio_dental",
  "ratio_prenatalVisit",
  "ratio_papsmear"
)

kpi_trends_relative <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% c("22_1","22_2","22_3",
                       "23_1","23_2","23_3",
                       "24_1","24_2","24_3",
                       "25_1")) %>%
  select(period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols  = all_of(kpi_vars),
    names_to  = "kpi",
    values_to = "value"
  ) %>%
  group_by(kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    gap_from_target = avg_value - target
  )


kpi_gap_paths <- kpi_trends_relative %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(kpi, period, gap_from_target) %>%
  tidyr::pivot_wider(
    names_from  = period,
    values_from = gap_from_target,
    names_prefix = "p_"
  ) %>%
  mutate(
    change = p_25_1 - p_22_1
  )

g2_funkybar <- ggplot(kpi_gap_paths, aes(x = kpi)) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # target line
  # thick "column" from start to end
  geom_segment(aes(x = kpi, xend = kpi,
                   y = p_22_1, yend = p_25_1),
               linewidth = 6, alpha = 0.6) +
  # start point (hollow)
  geom_point(aes(y = p_22_1), size = 3, shape = 21, fill = "white") +
  # end point (solid)
  geom_point(aes(y = p_25_1), size = 3) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in KPI Performance Relative to Target\nFrom 22_1 to 25_1",
    x = "KPI",
    y = "Gap from target (avg KPI – target)"
  )

g2_funkybar


#################################### 
### National performance trends - above versus below the indicator (incentive effect)
#################################### 

# Assistance vector
period_levels <- c(
  "22_1","22_2","22_3",
  "23_1","23_2","23_3",
  "24_1","24_2","24_3",
  "25_1"
)

# Create label for above or below target in 22_1
baseline_status <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period == "22_1") %>%
  select(municipality_id, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    above_target_22_1 = value >= target
  ) %>%
  select(municipality_id, kpi, above_target_22_1)

# Merge in baseline status 
kpi_trends_split <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  # attach baseline status
  left_join(baseline_status,
            by = c("municipality_id", "kpi")) %>%
  # drop cases where we don't know baseline status
  filter(!is.na(above_target_22_1)) %>%
  group_by(above_target_22_1, kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    period = factor(period, levels = period_levels),
    group_label = if_else(
      above_target_22_1,
      "Municipalities ABOVE target in 22_1",
      "Municipalities BELOW target in 22_1"
    )
  )

# Graphs -  
g1_below <- ggplot(
  kpi_trends_split %>% filter(!above_target_22_1),
  aes(x = period, y = avg_value, color = kpi, group = kpi)
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "National KPI Trends (22_1–25_1)\nMunicipalities BELOW target in 22_1",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


g1_below

g1_above <- ggplot(
  kpi_trends_split %>% filter(above_target_22_1),
  aes(x = period, y = avg_value, color = kpi, group = kpi)
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "National KPI Trends (22_1–25_1)\nMunicipalities ABOVE target in 22_1",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g1_above

# Graphs relative 
kpi_trends_split_rel <- kpi_trends_split %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    gap_from_target = avg_value - target
  )


g2_below <- ggplot(
  kpi_trends_split_rel %>% filter(!above_target_22_1),
  aes(x = period, y = gap_from_target, color = kpi, group = kpi)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Performance Relative to Target (22_1–25_1)\nMunicipalities BELOW target in 22_1",
    x = "Period",
    y = "Average KPI minus target",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2_above <- ggplot(
  kpi_trends_split_rel %>% filter(above_target_22_1),
  aes(x = period, y = gap_from_target, color = kpi, group = kpi)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Performance Relative to Target (22_1–25_1)\nMunicipalities ABOVE target in 22_1",
    x = "Period",
    y = "Average KPI minus target",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2_below
g2_above

####################################
### National performance trends - above versus below the indicator (incentive effect) - BAR CHARTS 
####################################
kpi_paths_split <- kpi_trends_split %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(above_target_22_1, kpi, period, avg_value) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = avg_value,
    names_prefix = "p_"
  ) %>%
  mutate(
    change_25_1_vs_22_1 = p_25_1 - p_22_1
  )

g1_below_bar <- kpi_paths_split %>%
  filter(!above_target_22_1) %>%
  ggplot(aes(x = kpi, y = change_25_1_vs_22_1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in KPI Levels (25_1 – 22_1)\nMunicipalities BELOW target in 22_1",
    x = "KPI",
    y = "Change in average KPI"
  )

g1_below_bar


g1_above_bar <- kpi_paths_split %>%
  filter(above_target_22_1) %>%
  ggplot(aes(x = kpi, y = change_25_1_vs_22_1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in KPI Levels (25_1 – 22_1)\nMunicipalities ABOVE target in 22_1",
    x = "KPI",
    y = "Change in average KPI"
  )

g1_above_bar

kpi_paths_split_rel <- kpi_trends_split_rel %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(above_target_22_1, kpi, period, gap_from_target) %>%
  tidyr::pivot_wider(
    names_from  = period,
    values_from = gap_from_target,
    names_prefix = "p_"
  ) %>%
  mutate(
    change_gap_25_1_vs_22_1 = p_25_1 - p_22_1
  )

kpi_change_by_group <- kpi_paths_split_rel %>%
  mutate(
    group = if_else(
      above_target_22_1,
      "ABOVE target in 22_1",
      "BELOW target in 22_1"
    ),
    delta_gap = p_25_1 - p_22_1
  )

g1_abovebelow_bar <- kpi_change_by_group %>%
  ggplot(aes(x = kpi, y = delta_gap, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in KPI Gap to Target (22_1 → 25_1)\nBy Baseline Group",
    x = "KPI",
    y = "Change in gap (25_1 – 22_1)",
    fill = "Group"
  )

g1_abovebelow_bar





g2_below_funky <- kpi_paths_split_rel %>%
  filter(!above_target_22_1) %>%
  ggplot(aes(x = kpi)) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # target line
  geom_segment(aes(x = kpi, xend = kpi,
                   y = p_22_1, yend = p_25_1),
               linewidth = 6, alpha = 0.6) +
  geom_point(aes(y = p_22_1), size = 3, shape = 21, fill = "white") +
  geom_point(aes(y = p_25_1), size = 3) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Path Relative to Target (22_1 → 25_1)\nMunicipalities BELOW target in 22_1",
    x = "KPI",
    y = "Gap from target (avg KPI – target)"
  )

g2_below_funky


g2_above_funky <- kpi_paths_split_rel %>%
  filter(above_target_22_1) %>%
  ggplot(aes(x = kpi)) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # target line
  geom_segment(aes(x = kpi, xend = kpi,
                   y = p_22_1, yend = p_25_1),
               linewidth = 6, alpha = 0.6) +
  geom_point(aes(y = p_22_1), size = 3, shape = 21, fill = "white") +
  geom_point(aes(y = p_25_1), size = 3) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Path Relative to Target (22_1 → 25_1)\nMunicipalities ABOVE target in 22_1",
    x = "KPI",
    y = "Gap from target (avg KPI – target)"
  )

g2_above_funky



kpi_paths_combined <- kpi_paths_split_rel %>%
  mutate(
    kpi = factor(kpi, levels = c(
      "ratio_vaccine",
      "ratio_stdTest",
      "ratio_prenatalVisit",
      "ratio_papsmear",
      "ratio_hypertension",
      "ratio_diabetes",
      "ratio_dental"
    ))
  ) %>%
  mutate(
    group = if_else(
      above_target_22_1,
      "ABOVE target in 22_1",
      "BELOW target in 22_1"
    ),
    kpi_num = as.numeric(kpi),                       # now SAFE
    y_pos   = kpi_num + if_else(group == "ABOVE target in 22_1", 0.18, -0.18)
  )


g2_combined_parallel <- ggplot(kpi_paths_combined, aes(colour = group)) +
  # target line at 0
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  # horizontal segments: from p_22_1 to p_25_1, at fixed y_pos
  geom_segment(
    aes(x = p_22_1, xend = p_25_1, y = y_pos, yend = y_pos),
    linewidth = 4,
    alpha = 0.7
  ) +
  
  # start point (22_1)
  geom_point(
    aes(x = p_22_1, y = y_pos, fill = group),
    size = 3,
    shape = 21
  ) +
  
  # end point (25_1)
  geom_point(
    aes(x = p_25_1, y = y_pos, fill = group),
    size = 3,
    shape = 21,
    colour = "black"
  ) +
  
  # put proper KPI labels back on the y-axis
  scale_y_continuous(
    breaks = unique(kpi_paths_combined$kpi_num),
    labels = levels(kpi_paths_combined$kpi)
  ) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Path Relative to Target (22_1 → 25_1)\nBelow vs Above Target Municipalities",
    x = "Gap from target (Avg KPI – Target)",
    y = "KPI",
    colour = "Group",
    fill   = "Group"
  )

g2_combined_parallel


##############################
### NATIONAL PERFORMANCE BY DECILE OF RATIO COMBINED
##############################
# 1. Compute baseline performance (ratioCombined in 22_1) and deciles
baseline_deciles <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period == "22_1") %>%
  select(municipality_id, ratioCombined) %>%
  group_by(municipality_id) %>%
  summarise(
    ratioCombined_22_1 = mean(ratioCombined, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # create deciles (1 = lowest performance, 10 = highest)
  mutate(
    decile = ntile(ratioCombined_22_1, 10),
    decile_label = factor(
      paste0("Decile ", decile),
      levels = paste0("Decile ", 1:10)
    )
  )

kpi_trends_decile <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  # attach baseline decile based on ratioCombined in 22_1
  left_join(baseline_deciles, by = "municipality_id") %>%
  # drop municipalities with missing baseline ratioCombined
  filter(!is.na(decile)) %>%
  group_by(decile, decile_label, kpi, period) %>%
  summarise(
    avg_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )

g1_decile <- ggplot(
  kpi_trends_decile,
  aes(x = period, y = avg_value, color = kpi, group = kpi)
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ decile_label) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Trends by Baseline Decile of Weighted KPI (ratioCombined)\n(22_1–25_1)",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

g1_decile

kpi_trends_decile_rel <- kpi_trends_decile %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    gap_from_target = avg_value - target
  )


g2_decile <- ggplot(
  kpi_trends_decile_rel,
  aes(x = period, y = gap_from_target, color = kpi, group = kpi)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ decile_label) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Performance Relative to Target by Baseline Decile of Weighted KPI\n(22_1–25_1)",
    x = "Period",
    y = "Average KPI minus target",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

g2_decile

###############################
### NATIONAL - SHARE OF MUNICIPALITIES MEETING THE TARGET 
##################################
kpi_time_target <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    hit_target = value >= target
  ) %>%
  group_by(kpi, period) %>%
  summarise(
    share_hit = mean(hit_target, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )

gg_share_hit <- ggplot(kpi_time_target,
                       aes(x = period, y = share_hit, group = kpi, color = kpi)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Share of Municipalities Meeting KPI Targets Over Time",
    x = "Period",
    y = "Share meeting target",
    color = "KPI"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_share_hit

##########################################
### SHARE OF MUNICIPALITIES IMPROVING 
##########################################
# kpi_before_after <- data %>%
#   mutate(period = as.character(period)) %>%
#   filter(period %in% c("22_1", "25_1")) %>%
#   select(municipality_id, municipality_name, period, all_of(kpi_vars)) %>%
#   pivot_longer(
#     cols = all_of(kpi_vars),
#     names_to = "kpi",
#     values_to = "value"
#   ) %>%
#   group_by(municipality_id, municipality_name, kpi, period) %>%
#   summarise(
#     avg_value = mean(value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   tidyr::pivot_wider(
#     names_from  = period,
#     values_from = avg_value
#   ) %>%
#   # optional: compute change if you want it
#   mutate(
#     change = `25_1` - `22_1`
#   )
# 
# 
# gg_scatter_before_after <- ggplot(
#   kpi_before_after,
#   aes(x = `22_1`, y = `25_1`)
# ) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   geom_point(size = 0.5, alpha = 0.15) +   # <<< SMALL + TRANSPARENT
#   facet_wrap(~ kpi, scales = "free") +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Municipality KPI: 22_1 vs 25_1",
#     subtitle = "Above the line = improvement",
#     x = "KPI in 22_1",
#     y = "KPI in 25_1"
#   )
# 
# gg_scatter_before_after
# 
# 
# gg_scatter_before_after



#############################
### PERFORMANCE TRENDS - BY QUARTILE 
#############################
baseline_quartiles <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period == "22_1") %>%
  select(municipality_id, ratioCombined) %>%
  group_by(municipality_id) %>%
  summarise(
    ratioCombined_22_1 = mean(ratioCombined, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    quartile = ntile(ratioCombined_22_1, 4),
    quartile_label = factor(
      paste0("Quartile ", quartile),
      levels = paste0("Quartile ", 1:4)
    )
  )

kpi_trends_quartile <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  left_join(baseline_quartiles, by = "municipality_id") %>%
  filter(!is.na(quartile)) %>%
  group_by(quartile, quartile_label, kpi, period) %>%
  summarise(
    avg_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )


g1_quartile <- ggplot(
  kpi_trends_quartile,
  aes(x = period, y = avg_value, color = kpi, group = kpi)
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ quartile_label) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Trends by Baseline Quartile of Weighted KPI (22_1 → 25_1)",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

g1_quartile


# 1–3. Difference 25_1 - 22_1 for each quartile & KPI
kpi_quartile_diff <- kpi_trends_quartile %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(quartile, quartile_label, kpi, period, avg_value) %>%
  pivot_wider(
    names_from  = period,
    values_from = avg_value
  ) %>%
  mutate(
    diff_25_1_vs_22_1 = `25_1` - `22_1`
  )
# 4. Column chart: KPI on y, difference on x, one bar per quartile
g_quartile_diff <- ggplot(
  kpi_quartile_diff,
  aes(x = diff_25_1_vs_22_1, y = kpi, fill = quartile_label)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge(width = 0.7)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in KPI (25_1 – 22_1) by Baseline Quartile",
    x = "Change in average KPI value",
    y = "KPI",
    fill = "Baseline quartile\n(ratioCombined in 22_1)"
  )

g_quartile_diff



kpi_trends_quartile_rel <- kpi_trends_quartile %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    gap_from_target = avg_value - target
  )


g2_quartile <- ggplot(
  kpi_trends_quartile_rel,
  aes(x = period, y = gap_from_target, color = kpi, group = kpi)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ quartile_label) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Performance Relative to Target by Baseline Quartile\n(22_1 → 25_1)",
    x = "Period",
    y = "Average KPI minus target",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

g2_quartile


kpi_quartile_rel_segments <- kpi_trends_quartile_rel %>%
  filter(period %in% c("22_1", "25_1")) %>%
  mutate(
    # make sure KPI order is stable
    kpi = factor(kpi, levels = c(
      "ratio_vaccine",
      "ratio_stdTest",
      "ratio_prenatalVisit",
      "ratio_papsmear",
      "ratio_hypertension",
      "ratio_diabetes",
      "ratio_dental"
    ))
  ) %>%
  select(quartile, quartile_label, kpi, period, gap_from_target) %>%
  pivot_wider(
    names_from  = period,
    values_from = gap_from_target,
    names_prefix = "gap_"
  ) %>%
  # 2. Create manual y-offsets so quartiles sit side-by-side for each KPI
  mutate(
    kpi_num = as.numeric(kpi),
    offset = case_when(
      quartile == 1 ~ -0.30,
      quartile == 2 ~ -0.10,
      quartile == 3 ~  0.10,
      quartile == 4 ~  0.30,
      TRUE          ~  0
    ),
    y_pos = kpi_num + offset
  )

# 3. Plot funky horizontal "parallel trends" segments relative to target 0
g2_quartile_parallel <- ggplot(
  kpi_quartile_rel_segments,
  aes(colour = quartile_label, fill = quartile_label)
) +
  # target line at 0
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  # segment from 22_1 gap to 25_1 gap
  geom_segment(
    aes(x = gap_22_1, xend = gap_25_1, y = y_pos, yend = y_pos),
    linewidth = 3,
    alpha = 0.7
  ) +
  # start point (22_1)
  geom_point(
    aes(x = gap_22_1, y = y_pos),
    size = 2.5,
    shape = 21
  ) +
  # end point (25_1)
  geom_point(
    aes(x = gap_25_1, y = y_pos),
    size = 2.5,
    shape = 21,
    colour = "black"
  ) +
  
  # Put proper KPI labels back on y-axis
  scale_y_continuous(
    breaks = sort(unique(kpi_quartile_rel_segments$kpi_num)),
    labels = levels(kpi_quartile_rel_segments$kpi)
  ) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Gap to Target (22_1 → 25_1)\nby Baseline Quartile",
    x = "Gap from target (Average KPI – Target)",
    y = "KPI",
    colour = "Baseline quartile",
    fill   = "Baseline quartile"
  )

g2_quartile_parallel


#############################
### PERFORMANCE TRENDS - REGIONAL LEVEL 
#############################
# Regional flag 

data_region <- data %>%
  mutate(
    region = case_when(
      state_abbr %in% c("RO","AC","AM","RR","PA","AP","TO") ~ "North",
      state_abbr %in% c("MA","PI","CE","RN","PB","PE","AL","SE","BA") ~ "Northeast",
      state_abbr %in% c("MG","ES","RJ","SP") ~ "Southeast",
      state_abbr %in% c("PR","SC","RS") ~ "South",
      state_abbr %in% c("MS","MT","GO","DF") ~ "Center-West",
      state_abbr == "" ~ NA_character_,
      TRUE ~ NA_character_
    )
  )


kpi_trends_region <- data_region %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(region, period, all_of(kpi_vars)) %>%
  filter(!is.na(region)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(region, kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = factor(period, levels = period_levels))


g1_region <- ggplot(
  kpi_trends_region,
  aes(x = period, y = avg_value, color = kpi, group = kpi)
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Trends by Region (22_1–25_1)",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

g1_region


kpi_trends_region_relative <- kpi_trends_region %>%
  mutate(
    target = case_when(
      kpi == "ratio_prenatalVisit"  ~ 0.45,
      kpi == "ratio_stdTest"        ~ 0.60,
      kpi == "ratio_dental"         ~ 0.60,
      kpi == "ratio_papsmear"       ~ 0.40,
      kpi == "ratio_vaccine"        ~ 0.95,
      kpi == "ratio_hypertension"   ~ 0.50,
      kpi == "ratio_diabetes"       ~ 0.50,
      TRUE ~ NA_real_
    ),
    gap_from_target = avg_value - target
  )


g2_region <- ggplot(
  kpi_trends_region_relative,
  aes(x = period, y = gap_from_target, color = kpi, group = kpi)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "KPI Performance Relative to Target by Region (22_1–25_1)",
    x = "Period",
    y = "Average KPI minus target",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

g2_region


kpi_region_diff <- kpi_trends_region %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(region, kpi, period, avg_value) %>%
  pivot_wider(
    names_from  = period,
    values_from = avg_value
  ) %>%
  mutate(
    diff_25_1_vs_22_1 = `25_1` - `22_1`
  )

# 2. Bar chart: KPI on x, diff on y, one bar per region
g_region_diff <- ggplot(
  kpi_region_diff,
  aes(x = kpi, y = diff_25_1_vs_22_1, fill = region)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge(width = 0.7)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Change in KPI (25_1 – 22_1) by Region",
    x = "KPI",
    y = "Change in average KPI value",
    fill = "Region"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

g_region_diff


#############################
### PERFORMANCE TRENDS - STATE LEVEL 
#############################

kpi_trends_state <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(state_abbr, period, all_of(kpi_vars)) %>%
  pivot_longer(
    cols = all_of(kpi_vars),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(state_abbr, kpi, period) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = factor(period, levels = period_levels))

g1_state <- ggplot(
  kpi_trends_state,
  aes(x = period, y = avg_value, color = kpi, group = kpi)
) +
  geom_line(size = 1.1) +
  geom_point(size = 1.4) +
  facet_wrap(~ state_abbr) +
  theme_minimal(base_size = 13) +
  labs(
    title = "KPI Trends by State (2022–2025)",
    x = "Period",
    y = "Average KPI Value",
    color = "KPI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10)
  )


g1_state
#############################
### PERFORMANCE TRENDS - MUNICIPAL LEVEL 
#############################

# choose which KPI to map
kpi_var <- "ratioCombined"   # change this to e.g. "ratio_prenatalVisit" if you want

muni_change <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% c("22_1", "25_1")) %>%
  select(municipality_id, municipality_name, period, !!sym(kpi_var)) %>%
  group_by(municipality_id, municipality_name, period) %>%
  summarise(
    avg_value = mean(!!sym(kpi_var), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_value
  ) %>%
  mutate(
    change = `25_1` - `22_1`
  )

muni_change <- muni_change %>%
  mutate(municipality_id = as.numeric(municipality_id))

muni_sf <- read_municipality(year = 2020)

muni_sf <- muni_sf %>%
  mutate(code_muni_6 = code_muni %/% 10)

muni_map <- muni_sf %>%
  left_join(muni_change, by = c("code_muni_6" = "municipality_id"))


gg_muni_change <- ggplot(muni_map) +
  geom_sf(aes(fill = change), color = NA) +
  scale_fill_gradient2(
    low = "darkred",     # worse
    mid = "white",       # no change
    high = "darkgreen",  # best improvement
    midpoint = 0,
    na.value = "grey90",
    name = "Δ KPI (25_1 – 22_1)"
  ) +
  theme_void(base_size = 14) +
  labs(
    title = "Change in KPI between 22_1 and 25_1 by Municipality",
    subtitle = kpi_var
  )

gg_muni_change


# 1. Define KPIs to map 

kpi_vars_to_map <- c(
  "ratio_vaccine",
  "ratio_diabetes",
  "ratio_hypertension",
  "ratio_stdTest",
  "ratio_dental",
  "ratio_prenatalVisit",
  "ratio_papsmear"
)

# 2. Prepare municipality shapefile with 6-digit code 

# Read municipalities (only need to do this once)
muni_sf <- read_municipality(year = 2020)

# Create a 6-digit version of the municipality code to match your data
muni_sf <- muni_sf %>%
  mutate(code_muni_6 = code_muni %/% 10)   # integer division drops last digit


# 3. Helper function: compute 22_1 → 25_1 change for a given KPI 

compute_muni_change <- function(df, kpi_var) {
  df %>%
    mutate(period = as.character(period)) %>%
    filter(period %in% c("22_1", "25_1")) %>%
    select(municipality_id, municipality_name, period, !!sym(kpi_var)) %>%
    group_by(municipality_id, municipality_name, period) %>%
    summarise(
      avg_value = mean(!!sym(kpi_var), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = period,
      values_from = avg_value
    ) %>%
    mutate(
      change = `25_1` - `22_1`,
      municipality_id = as.numeric(municipality_id)   # ensure numeric for join
    )
}


# 4. Helper function: make map for one KPI 

make_kpi_map <- function(muni_sf, muni_change, kpi_var) {
  
  muni_map <- muni_sf %>%
    left_join(muni_change, by = c("code_muni_6" = "municipality_id"))
  
  ggplot(muni_map) +
    geom_sf(aes(fill = change), color = NA) +
    scale_fill_gradient2(
      low      = "darkred",     # worse
      mid      = "white",       # no change
      high     = "darkgreen",   # best improvement
      midpoint = 0,
      na.value = "grey90",
      name     = "Δ KPI (25_1 – 22_1)"
    ) +
    theme_void(base_size = 14) +
    labs(
      title    = "Change in KPI between 22_1 and 25_1 by Municipality",
      subtitle = kpi_var
    )
}


# 5. Loop over KPIs and build all maps 

kpi_maps <- list()

for (kpi in kpi_vars_to_map) {
  # compute change for this KPI
  muni_change_k <- compute_muni_change(data, kpi)
  
  # build map for this KPI
  kpi_maps[[kpi]] <- make_kpi_map(muni_sf, muni_change_k, kpi)
}


#############################
### INEQUALITY - 90TH MINUS 10TH  
#############################

kpi_vars_all <- c("ratioCombined", kpi_vars)

kpi_inequality <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, period, all_of(kpi_vars_all)) %>%
  pivot_longer(
    cols = all_of(kpi_vars_all),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(kpi, period) %>%
  summarise(
    p10 = quantile(value, 0.10, na.rm = TRUE),
    p90 = quantile(value, 0.90, na.rm = TRUE),
    gap_p90_p10 = p90 - p10,
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )

gg_gap_combined <- kpi_inequality %>%
  filter(kpi == "ratioCombined") %>%
  ggplot(aes(x = period, y = gap_p90_p10, group = 1)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time for Composite KPI (ratioCombined)\n90th – 10th Percentile Gap",
    x = "Period",
    y = "Gap (p90 – p10)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_gap_combined

gg_gap_kpis <- kpi_inequality %>%
  filter(kpi != "ratioCombined") %>%
  ggplot(aes(x = period, y = gap_p90_p10, group = kpi, color = kpi)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time Across KPIs (p90 – p10 Gap)",
    x = "Period",
    y = "Gap (p90 – p10)",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_gap_kpis

#############################
### INEQUALITY - 90P MINUS 10P, BY REGION
##############################
kpi_vars_all <- c("ratioCombined", kpi_vars)

kpi_inequality_region <- data_region %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, region, period, all_of(kpi_vars_all)) %>%
  pivot_longer(
    cols = all_of(kpi_vars_all),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(region, kpi, period) %>%
  summarise(
    p10 = quantile(value, 0.10, na.rm = TRUE),
    p90 = quantile(value, 0.90, na.rm = TRUE),
    gap_p90_p10 = p90 - p10,
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )

gg_gap_combined_region <- kpi_inequality_region %>%
  filter(kpi == "ratioCombined") %>%
  ggplot(aes(x = period, y = gap_p90_p10, group = 1)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time for Composite KPI (ratioCombined)\n90th – 10th Percentile Gap by Region",
    x = "Period",
    y = "Gap (p90 – p10)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_gap_combined_region

gg_gap_kpis_region <- kpi_inequality_region %>%
  filter(kpi != "ratioCombined") %>%
  ggplot(aes(x = period, y = gap_p90_p10, group = kpi, color = kpi)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time Across KPIs (p90 – p10 Gap) by Region",
    x = "Period",
    y = "Gap (p90 – p10)",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_gap_kpis_region


#############################
### INEQUALITY - 75P MINUS 25P
#############################
# include ratioCombined plus the 7 KPIs
kpi_vars_all <- c("ratioCombined", kpi_vars)

kpi_inequality_iqr <- data %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, period, all_of(kpi_vars_all)) %>%
  pivot_longer(
    cols = all_of(kpi_vars_all),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(kpi, period) %>%
  summarise(
    p25 = quantile(value, 0.25, na.rm = TRUE),
    p75 = quantile(value, 0.75, na.rm = TRUE),
    gap_p75_p25 = p75 - p25,
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )

gg_gap_combined_iqr <- kpi_inequality_iqr %>%
  filter(kpi == "ratioCombined") %>%
  ggplot(aes(x = period, y = gap_p75_p25, group = 1)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time for Composite KPI (ratioCombined)\n75th – 25th Percentile Gap",
    x = "Period",
    y = "Gap (p75 – p25)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_gap_combined_iqr

gg_gap_kpis_iqr <- kpi_inequality_iqr %>%
  filter(kpi != "ratioCombined") %>%
  ggplot(aes(x = period, y = gap_p75_p25, group = kpi, color = kpi)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time Across KPIs\n75th – 25th Percentile Gap",
    x = "Period",
    y = "Gap (p75 – p25)",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_gap_kpis_iqr


#############################
### INEQUALITY - 75P MINUS 25P, BY REGION
#############################

kpi_vars_all <- c("ratioCombined", kpi_vars)

kpi_iqr_region <- data_region %>%
  mutate(period = as.character(period)) %>%
  filter(period %in% period_levels) %>%
  select(municipality_id, region, period, all_of(kpi_vars_all)) %>%
  pivot_longer(
    cols = all_of(kpi_vars_all),
    names_to = "kpi",
    values_to = "value"
  ) %>%
  group_by(region, kpi, period) %>%
  summarise(
    p25 = quantile(value, 0.25, na.rm = TRUE),
    p75 = quantile(value, 0.75, na.rm = TRUE),
    iqr = p75 - p25,
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = period_levels)
  )

gg_iqr_combined_region <- kpi_iqr_region %>%
  filter(kpi == "ratioCombined") %>%
  ggplot(aes(x = period, y = iqr, group = 1)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time for Composite KPI (ratioCombined)\nInterquartile Range (p75 – p25) by Region",
    x = "Period",
    y = "IQR (p75 – p25)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_iqr_combined_region

gg_iqr_kpis_region <- kpi_iqr_region %>%
  filter(kpi != "ratioCombined") %>%
  ggplot(aes(x = period, y = iqr, group = kpi, color = kpi)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inequality Over Time Across KPIs\nInterquartile Range (p75 – p25) by Region",
    x = "Period",
    y = "IQR (p75 – p25)",
    color = "KPI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_iqr_kpis_region


#############################
### GRAPHS
#############################

# National level 

g1
g1_bar
g2
g2_funkybar
g1_below
g1_below_bar
g1_above
g1_above_bar
g1_abovebelow_bar
g2_below
g2_below_funky
g2_above
g2_above_funky
#g2_combined_funky 
g2_combined_parallel
g1_decile
g2_decile
g1_quartile
g_quartile_diff
g2_quartile 
g2_quartile_parallel
gg_share_hit
#gg_scatter_before_after


# Regional level 
g1_region 
g2_region 

# State level 
g1_state

# Municipality level 
gg_muni_change
kpi_maps$ratio_vaccine
kpi_maps$ratio_diabetes
kpi_maps$ratio_hypertension
kpi_maps$ratio_stdTest
kpi_maps$ratio_dental
kpi_maps$ratio_prenatalVisit
kpi_maps$ratio_papsmear

# Inequality 
gg_gap_combined
gg_gap_kpis
gg_gap_combined_region
gg_gap_kpis_region
gg_gap_combined_iqr
gg_gap_kpis_iqr
gg_iqr_combined_region
gg_iqr_kpis_region








