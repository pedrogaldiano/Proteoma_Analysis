# Generate Graph: Precursor x RT ----
# Reconstruction of the ion chromatograms, the precursor quantity is
# plotted over the retention time (min) for each sample.


Generate_Graph_PrecursorRT <- function(dr, colors, cols = 3, rows = NULL) {
 # dr <- Generate_DiannReport(samples)
 #  cols = 3
 #  rows = NULL
 #  colors = c(
 #    "#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99",
 #    "#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
 
 precursorRT <- ggplot2::ggplot(dr, ggplot2::aes(x = RT, y = Precursor.Quantity)) +
   ggplot2::geom_line(ggplot2::aes(color = condition), show.legend = FALSE) +
   ggplot2::scale_color_manual(values = colors) +
   ggplot2::labs(
     x = "Retention time (min)",
     y = "Precursor quantity",
     color = colors
   ) +
   ggplot2::facet_wrap(~Run,
                       labeller = ggplot2::as_labeller(labels_names),
                       ncol = cols,
                       nrow = rows,
                       scales = "free") +
   ggplot2::theme(
     strip.background = ggplot2::element_blank(),
     panel.border = ggplot2::element_rect(color = "black", fill = NA),
     panel.background = ggplot2::element_blank()
   )

 return(precursorRT)
}

