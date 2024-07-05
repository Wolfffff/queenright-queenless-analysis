Q_QRW_INT_QLW <- list(
  Q = "#893F71",
  QRW = "#AC6F82",
  INF = "#CC5500",
  QLW = "#FFAE00"
)

COLONY_COLORS <- wes_palette("Cavalcanti1")

CONSISTENT_THEME <- theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "None",
        text = element_text(size = 9),
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        aspect.ratio = 1,
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 9),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )

CONSISTENT_THEME_NO_ASPECT <- theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "None",
        text = element_text(size = 9),
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 9),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )