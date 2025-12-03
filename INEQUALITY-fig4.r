# Setup
library(dplyr)
library(purrr)
library(tidyr)

# Start & finish as you defined, but synced on the same municipalities
start <- kpi_merged221 %>% 
  select(municipality_id, ratioCombined, ADH_PESOTOT_NEG, ADH_IDHM_E, ADH_RDPC, ADH_IDHM_L) %>%
  filter(!is.na(ratioCombined))

finish <- kpi_merged %>% 
  select(municipality_id, ratioCombined, ADH_PESOTOT_NEG, ADH_IDHM_E, ADH_RDPC, ADH_IDHM_L) %>%
  filter(!is.na(ratioCombined))


# Keep only municipalities that appear in BOTH
muni_keep <- intersect(start$municipality_id, finish$municipality_id)

start_filtered <- start  %>% filter(municipality_id %in% muni_keep)
finish_filtered <- finish %>% filter(municipality_id %in% muni_keep)

# Helper functino 
calc_rc_for_var <- function(df, var, rc_col = "ratioCombined") {
  x  <- df[[var]]
  rc <- df[[rc_col]]
  
  # Drop rows where x is NA
  ok <- !is.na(x) & !is.na(rc)
  x  <- x[ok]
  rc <- rc[ok]
  
  # If too few observations, return NA
  if (length(x) < 5) {
    return(tibble(
      variable      = var,
      rc_10         = NA_real_,
      rc_90         = NA_real_,
      rc_diff       = NA_real_
    ))
  }
  
  # 10th and 90th percentile of x
  q10 <- as.numeric(quantile(x, 0.10, na.rm = TRUE))
  q90 <- as.numeric(quantile(x, 0.90, na.rm = TRUE))
  
  # Find closest municipality to each percentile
  idx10 <- which.min(abs(x - q10))
  idx90 <- which.min(abs(x - q90))
  
  rc_10 <- rc[idx10]
  rc_90 <- rc[idx90]
  
  tibble(
    variable = var,
    rc_10    = rc_10,
    rc_90    = rc_90,
    rc_diff  = rc_90 - rc_10
  )
}


# Variables for calc. 
vars <- c("ADH_PESOTOT_NEG", "ADH_IDHM_E", "ADH_RDPC", "ADH_IDHM_L")

# Start period
results_start <- map_dfr(vars, ~ calc_rc_for_var(start_filtered, .x)) %>%
  mutate(period = "start")

# Finish period
results_finish <- map_dfr(vars, ~ calc_rc_for_var(finish_filtered, .x)) %>%
  mutate(period = "finish")


# Calc
results_long <- bind_rows(results_start, results_finish)

results_wide1 <- results_long %>%
  select(variable, period, rc_10, rc_90, rc_diff) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(rc_10, rc_90, rc_diff),
    names_glue  = "{.value}_{period}"
  )

results_wide1

results_wide1 <- results_wide1 %>%
  bind_rows(
    tibble(
      variable        = "total",
      rc_10_start     = NA_real_,
      rc_10_finish    = NA_real_,
      rc_90_start     = NA_real_,
      rc_90_finish    = NA_real_,
      rc_diff_start   = 6.5122,
      rc_diff_finish  = 6.0424
    )
  )

# Graph 
library(ggplot2)

# Prep data for plotting
plot_data <- results_wide1 %>%
  select(variable, rc_diff_start, rc_diff_finish) %>%
  pivot_longer(
    cols = c(rc_diff_start, rc_diff_finish),
    names_to = "period",
    values_to = "rc_diff"
  ) %>%
  mutate(
    # Rename period values + enforce Start on left
    period = factor(period,
                    levels = c("rc_diff_start", "rc_diff_finish"),
                    labels = c("Start", "Finish")),
    
    # Rename variables
    variable = case_when(
      variable == "total"           ~ "Total",
      variable == "ADH_PESOTOT_NEG" ~ "Black",
      variable == "ADH_IDHM_E"      ~ "Education",
      variable == "ADH_IDHM_L"      ~ "Health",
      variable == "ADH_RDPC"        ~ "Income",
      TRUE                          ~ variable
    ),
    
    # Multiply ONLY Black rows by -1
    rc_diff = if_else(variable == "Black", -1 * rc_diff, rc_diff),
    
    # Order categories for x-axis
    variable = factor(variable,
                      levels = c("Total", "Black", "Education", "Health", "Income"))
  )

# Plot
ggplot(plot_data, aes(x = variable, y = rc_diff, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    x = NULL,
    y = "Gap (90th - 10th percentile)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12)
  )


ggplot(plot_data, aes(x = variable, y = rc_diff, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Inequalities in Primary Care Performance",
    subtitle = "Gap in combined KPI score between municipalities \nin the 90th and 10th percentile across various dimensions of inequality",
    x = NULL,
    y = "Gap (90th - 10th percentile)",
    fill = NULL
  ) +
  scale_x_discrete(labels = c(
    "Total" = "Overall inequality",
    "Black" = "Racial",
    "Education" = "Education",
    "Health" = "Health",
    "Income" = "Income"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 12)
  )


ggplot(plot_data, aes(x = variable, y = rc_diff, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Inequalities in primary care performance",
    subtitle = "Gap in combined KPI score between municipalities \nin the 90th and 10th percentile across various dimensions of inequality",
    x = NULL,
    y = "",
    fill = NULL
  ) +
  scale_fill_manual(values = c("Start" = "#F8766D", "Finish" = "#00BFC4"),
                    labels = c("Start" = "2022", "Finish" = "2025")) +
  scale_x_discrete(labels = c(
    "Total" = "Overall inequality",
    "Black" = "Racial",
    "Education" = "Education",
    "Health" = "Health",
    "Income" = "Income"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    legend.position = "right",
    axis.text.x = element_text(size = 9)
  )


