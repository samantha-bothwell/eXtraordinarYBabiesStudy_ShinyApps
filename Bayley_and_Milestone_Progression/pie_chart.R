library(ggplot2)

milestones_sum <- Milestones %>% group_by(sca_condition) %>% summarise(N = n())

# Add proportions and cumulative positions for the labels
milestones_sum$fraction <- milestones_sum$N / sum(milestones_sum$N)
milestones_sum$ymax <- cumsum(milestones_sum$fraction)
milestones_sum$ymin <- c(0, head(milestones_sum$ymax, n = -1))
milestones_sum$label_position <- (milestones_sum$ymax + milestones_sum$ymin) / 2
milestones_sum$label <- paste0(milestones_sum$sca_condition, "\n(N = ", milestones_sum$N, ")")

# Plot
ggplot(milestones_sum, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.65, fill = sca_condition)) +
  geom_rect(alpha = 0.7) +
  geom_text(aes(x = 3.35, y = label_position, label = label), color = "black", size = 5.6) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Donut Plot of SCA Conditions") +
  scale_fill_manual(values = c("#4B0082", "#fdb863", "cyan3"))

