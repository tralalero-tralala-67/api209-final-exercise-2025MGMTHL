# Pretty labels
kpi_labels <- c(
  "ratio_vaccine"       = "% 1-year-old children vaccinated",
  "ratio_diabetes"      = "% people with diabetes",
  "ratio_hypertension"  = "% people with hypertension",
  "ratio_stdTest"       = "% pregnant women tested for syphilis and HIV",
  "ratio_dental"        = "% pregnant women who received dental care",
  "ratio_prenatalVisit" = "% pregnant women with at least 6 prenatal visits",
  "ratio_papsmear"      = "% women with cervical cytology"
)

# Colours pulled from the national KPI figure
kpi_colors <- c(
  "ratio_vaccine"       = "#3872DF",  # blue
  "ratio_hypertension"      = "#ACBDB5",  # grey-green
  "ratio_diabetes"  = "#35713A",  # green
  "ratio_stdTest"       = "#C6B5DF",  # lilac
  "ratio_dental"        = "#F2BE41",  # gold/orange
  "ratio_prenatalVisit" = "#D99D9C",  # pink
  "ratio_papsmear"      = "#766B9A"   # dark purple
)


# Pretty labels (2-line versions)
kpi_labels <- c(
  "ratio_vaccine"       = "% 1-year-old children\nvaccinated",
  "ratio_diabetes"      = "% people with\ndiabetes",
  "ratio_hypertension"  = "% people with\nhypertension",
  "ratio_stdTest"       = "% pregnant women tested\nfor syphilis and HIV",
  "ratio_dental"        = "% pregnant women who\nreceived dental care",
  "ratio_prenatalVisit" = "% pregnant women with at least\n6 prenatal visits",
  "ratio_papsmear"      = "% women with\ncervical cytology"
)

g2_above_funky <- kpi_paths_split_rel %>%
  filter(above_target_22_1) %>%
  ggplot(aes(x = kpi, colour = kpi, fill = kpi)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_segment(aes(xend = kpi,
                   y = p_22_1*100,
                   yend = p_25_1*100),
               linewidth = 6, alpha = 0.6) +
  geom_point(aes(y = p_22_1*100), size = 3, shape = 21, fill = "white") +
  geom_point(aes(y = p_25_1*100), size = 3) +
  
  scale_x_discrete(labels = kpi_labels) +
  scale_colour_manual(values = kpi_colors, labels = kpi_labels) +
  scale_fill_manual(values = kpi_colors, labels = kpi_labels) +
  
  coord_flip() +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    
    # Move title left
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"), 
    
    # Allow multi-line x-axis labels
    axis.text.y = element_text(lineheight = 0.9)
  ) +
  
  labs(
    title = "Panel B: Average KPI performance relative to national targets \nfor municipalities ABOVE target in 2022",,
    subtitle = "Percentage points, 2022-2025",
    x = "",
    y = ""
  )

g2_above_funky



