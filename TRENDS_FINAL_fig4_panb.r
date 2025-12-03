gg_gap_combined_region <- kpi_inequality_region %>%
  filter(kpi == "ratioCombined",
         !is.na(region)) %>%            # ⬅ remove NA region
  ggplot(aes(x = period, y = gap_p90_p10, group = 1)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~ region, scales = "free_y") +   # cleaner facet display
  scale_x_discrete(                           # ⬅ spreads out x-axis ticks
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    title = "Panel B: Regional inequalities in primary care performance",
    subtitle = "Gap between municipalities in the 90th and 10th percentile \nfor the combined KPI, 2022–2025",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic"),
    strip.text = element_text(size = 10, face = "bold")
  )

gg_gap_combined_region

kpi_percentiles_region <- kpi_inequality_region %>%
  filter(kpi == "ratioCombined") %>%
  select(region, period, p10, p90) %>%
  pivot_longer(
    cols = c(p10, p90),
    names_to = "percentile",
    values_to = "value"
  ) %>%
  mutate(percentile = recode(percentile,
                             "p10" = "10th percentile",
                             "p90" = "90th percentile"))

gg_percentiles_region <- kpi_percentiles_region %>%
  ggplot(aes(x = period, y = value, color = percentile, group = percentile)) +
  geom_line(linewidth = 1.2) +
  #geom_point(size = 3) +
  facet_wrap(~ region) +
  theme_minimal(base_size = 14) +
  labs(
    title = "10th and 90th Percentiles Over Time for Composite KPI (ratioCombined)",
    subtitle = "By Region",
    x = "Period",
    y = "Percentile Value",
    color = "Percentile"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg_percentiles_region

