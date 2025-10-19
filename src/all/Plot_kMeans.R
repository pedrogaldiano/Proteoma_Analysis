#############
# Generate Graph PCA and K-means  ----

# Counting the number of unique peptides per run
#############

Plot_kMeans <- function(imtx, numberOfClusters, seed = 123) {
  pcaComplete <- prcomp(t(imtx), scale = TRUE)

  # Principal component 1 and 2
  pcaTwoDimensions <- as.data.frame(pcaComplete$x[, 1:2])

  kmeansPlot <- factoextra::fviz_nbclust(
    pcaTwoDimensions,
    FUNcluster = kmeans,
    method = "wss",
    k.max = ifelse(ncol(imtx) < 11, ncol(imtx) - 1, 10)
  ) +

    ggplot2::geom_point(size = 4, color = "steelblue") +
    ggplot2::geom_vline(xintercept = numberOfClusters, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Optimal number of clusters") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      text = ggplot2::element_text(color = "black", size = 20),
      title = ggplot2::element_text(
        color = "black",
        size = 20,
        face = "bold",
        hjust = 0.5
      ),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black", face = "bold"),
      axis.ticks = ggplot2::element_line(color = "black"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA)
    )

  return(kmeansPlot)
}
