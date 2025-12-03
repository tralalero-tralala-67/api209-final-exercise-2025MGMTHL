library(broom)
library(dplyr)
library(ggplot2)

# 1. Extract coefficients and exclude FE + intercept
coefs <- tidy(model) %>%
  filter(!grepl("factor\\(region\\)", term),
         term != "(Intercept)")

# 2. Scale coefficients into meaningful units
coef_effects <- coefs %>%
  mutate(
    scale = case_when(
      term %in% c("ln_DFSAUSM_pc", "ln_pop", "ln_gdp_pc") ~ 0.10,   # 10% change
      term == "ADH_IDHM_E_scaled"                                 ~ 1, # +0.1
      term %in% c("black_pct", "ADH_PPOB", "rural_pct")   ~ 0.10, # +10 pp
      TRUE ~ 1
    ),
    effect     = estimate * scale,
    se_effect  = std.error * scale,
    lower      = effect - 1.96 * se_effect,
    upper      = effect + 1.96 * se_effect,
    term_clean = recode(term,
                        "ln_DFSAUSM_pc" = "Primary care spending**",
                        "ADH_IDHM_E_scaled"    = "Education***",
                        "black_pct"     = "Black share**",
                        "ADH_PPOB"      = "Poverty**",
                        "ln_pop"        = "Population***",
                        "ln_gdp_pc"        = "GDP per capita**",
                        "rural_pct"     = "Rural share**"
    )
  )

# 3. Sort top-to-bottom
plot_data <- coef_effects %>%
  arrange(desc(effect)) %>%
  mutate(term_clean = factor(term_clean, levels = rev(term_clean)))

# 4. Plot with vertical caps on the CI
ggplot(plot_data, aes(x = term_clean, y = effect)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15,        # <-- This adds the small horizontal "T" caps
    linewidth = 0.7
  ) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "grey40") +
  coord_flip() +
  labs(
    title = "Associations between municipal characteristics \nand KPI performance",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 12)
  ) 


