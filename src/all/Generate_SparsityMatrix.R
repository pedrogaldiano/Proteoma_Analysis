#############
# Generate Sparsity Matrix  ----
#############

Generate_SparsityMatrix <- function(dm, labels) {
  #Filter to use only labels that exist in the matrix
  labels <- labels[colnames(dm)]

  df <- as.data.frame(dm)
  labels <- setNames(names(labels), labels)
  df <- dplyr::rename(df, labels)

  sparsityPlot <- naniar::vis_miss(df) +
    ggplot2::labs(
      x = NULL,
      y = "Number of proteins"
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      axis.text.y = ggplot2::element_text(color = "black", vjust = 1),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        color = "black"
      ),
      line = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(color = "white")
    )

  return(sparsityPlot)
}
