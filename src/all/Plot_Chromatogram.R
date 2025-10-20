# Generate Graph: Precursor x RT ----
# Reconstruction of the ion chromatograms, the precursor quantity is
# plotted over the retention time (min) for each sample.

Plot_Chromatogram <- function(
  dr,
  labels,
  colors,
  cols = 3,
  rows = NULL
) {
  precursorRT <- ggplot2::ggplot(
    dr,
    ggplot2::aes(x = RT, y = Precursor.Quantity)
  ) +
    ggplot2::geom_line(ggplot2::aes(color = condition), show.legend = FALSE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      x = "Retention time (min)",
      y = "Precursor quantity",
      color = colors
    ) +
    ggplot2::facet_wrap(
      ~Run,
      labeller = ggplot2::as_labeller(labels),
      ncol = cols,
      nrow = rows,
      scales = "free"
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      panel.background = ggplot2::element_blank()
    )

  return(precursorRT)
}
