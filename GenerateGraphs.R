


# Graph: Precursor x RT ----
  precursor_rt <- DiannReport(labels, levels) %>%
  ggplot(aes(x = RT, y = Precursor.Quantity)) +
  geom_line(aes(color = condition), show.legend = FALSE) +
  scale_color_manual(values = colors) +
  labs(x = "Retention time (min)",
       y = "Precursor quantity",
       color = NULL) +
  facet_wrap(~Run, ncol = 6, scales = "free") +
  theme(strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank()
  )

precursor_x_RT <- (precursor_rt)
plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 30, face = "bold"))

ggsave(filename = "Figure_1-6.png",
       path = "plots",
       plot = Figure_1,
       width = 24, height = 20,
       units = "in", dpi = 300)


