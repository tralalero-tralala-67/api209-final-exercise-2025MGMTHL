# Colours pulled from the national KPI figure
kpi_colors <- c(
  "ratio_vaccine"       = "#3872DF",  # blue
  "ratio_hypertension"  = "#ACBDB5",  # grey-green
  "ratio_diabetes"      = "#35713A",  # green
  "ratio_stdTest"       = "#C6B5DF",  # lilac
  "ratio_dental"        = "#F2BE41",  # gold/orange
  "ratio_prenatalVisit" = "#D99D9C",  # pink
  "ratio_papsmear"      = "#766B9A"   # dark purple
)

# Pretty labels (2-line versions)
kpi_labels <- c(
  "ratio_vaccine"       = "% children\nvaccinated",
  "ratio_diabetes"      = "% diabetes",
  "ratio_hypertension"  = "% hypertension",
  "ratio_stdTest"       = "% STI test",
  "ratio_dental"        = "% dental care",
  "ratio_prenatalVisit" = "% 6 prenatal visits",
  "ratio_papsmear"      = "% cytology test"
)

gg_gap_kpis <- kpi_inequality %>%
  filter(kpi != "ratioCombined") %>%
  ggplot(aes(
    x = period,
    y = gap_p90_p10 * 100,   # ⬅ multiply by 100 for percentage points
    group = kpi,
    color = kpi
  )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = kpi_colors,              # ⬅ use the correct object
    breaks = names(kpi_labels),       # ⬅ ensures legend uses these levels
    labels = kpi_labels
  ) +
  labs(
    title = "Panel A: Inequality trends across KPIs",
    subtitle = "Percentage point gap in KPI score between municipalities in the \n90th and 10th percentile, 2022–2025",
    x = "",
    y = "",
    color = "KPI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = "right"
  )

gg_gap_kpis
