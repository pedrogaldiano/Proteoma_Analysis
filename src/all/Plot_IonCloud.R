#############
# Generate the Íon cloud  ----

# For the m/z map, the density of ions collected is plotted over
# the scan range (m/z) for each sample.
#############

Plot_IonCloud <- function(dr, labels, cols = 3, rows = NULL ) {

  ionCloud <- ggplot2::ggplot(dr, ggplot2::aes(x = RT, y = Precursor.Mz)) +
    ggpointdensity::geom_pointdensity(size = 0.1,
                                      method = "neighbors", #kde2d shows a weird legend
                                      show.legend = TRUE) +
    viridis::scale_color_viridis(option = "H") +
    ggplot2::scale_x_continuous(limits = c(0, 90)) +
    ggplot2::labs(
      x = "Retention time (min)",
      y = "Scan range (m/z)",
      color = NULL
    ) +
    ggplot2::facet_wrap(~Run,
                        labeller = ggplot2::as_labeller(labels),
                        scales = "free",
                        ncol = cols) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.height = ggplot2::unit(0.25, "cm"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      panel.background = ggplot2::element_blank()
    )
  
  return(ionCloud)
}
