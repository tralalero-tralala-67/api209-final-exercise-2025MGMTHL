g2_below_funky <- kpi_paths_split_rel %>%
  filter(!above_target_22_1) %>%       # ⬅️ Below-target definition
  ggplot(aes(x = kpi, colour = kpi, fill = kpi)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_segment(aes(xend = kpi,
                   y = p_22_1*100,
                   yend = p_25_1*100),
               linewidth = 6, alpha = 0.6) +
  
  geom_point(aes(y = p_22_1*100),
             size = 3, shape = 21, fill = "white") +
  
  geom_point(aes(y = p_25_1*100),
             size = 3) +
  
  scale_x_discrete(labels = kpi_labels) +
  scale_colour_manual(values = kpi_colors, labels = kpi_labels) +
  scale_fill_manual(values = kpi_colors, labels = kpi_labels) +
  
  coord_flip() +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    
    # SAME title formatting as the ABOVE plot
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"), 
    axis.text.y = element_text(lineheight = 0.9)
  ) +
  
  labs(
    title = "Panel A: Average KPI performance relative to national targets \nfor municipalities BELOW target in 2022",,
    subtitle = "Percentage points, 2022-2025",
    x = "",
    y = ""
  )

g2_below_funky
