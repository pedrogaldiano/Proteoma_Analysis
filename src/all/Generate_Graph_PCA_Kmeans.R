#############
# Generate Graph PCA and K-means  ----

# Counting the number of unique peptides per run
#############


# TODO:: This should be broken down in 2 or 3 functions
Generate_Graph_PCA_and_Kmeans <- function(imtx, numberOfClusters, clusterColors, labels, seed = 123) {
  
  
  #Filter to use only labels that exist in the matrix
  labels <- labels[colnames(imtx)]
  
  df <- as.data.frame(imtx)
  labels <- setNames(names(labels), labels)
  imtx <- dplyr::rename(df, labels)
  
  pcaComplete <- prcomp(t(imtx), scale = TRUE)
  
  # Principal component 1 and 2
  pcaTwoDimensions <- as.data.frame(pcaComplete$x[, 1:2])
  
  
  kmeansPlot <- factoextra::fviz_nbclust(pcaTwoDimensions,
                                         FUNcluster = kmeans,
                                         method = "wss") +
    
    ggplot2::geom_point(size = 4, color = "steelblue") +
    ggplot2::geom_vline(xintercept = numberOfClusters, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Optimal number of clusters") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      text = ggplot2::element_text(color = "black", size = 20),
      title = ggplot2::element_text(color = "black", size = 20, face = "bold", hjust = 0.5),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black", face = "bold"),
      axis.ticks = ggplot2::element_line(color = "black"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA))
  
  set.seed(seed)
  
  kMeansPCA <- kmeans(pcaTwoDimensions, centers = numberOfClusters)
  
  pcaPlot <- factoextra::fviz_pca_ind(pcaComplete,
                                      geom = c("point", "text"),
                                      habillage = kMeansPCA$cluster,
                                      labelsize = 7, pointsize = 3,
                                      palette = clusterColors,
                                      addEllipses = FALSE,
                                      ggtheme = ggplot2::theme_classic(),
                                      repel = TRUE) +
    
    ggplot2::labs(title = "PCA clustered by k-means") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   text = ggplot2::element_text(color = "black", size = 20),
                   title = ggplot2::element_text(color = "black", size = 20,
                                                 face = "bold", hjust = 0.5),
                   axis.text = ggplot2::element_text(color = "black"),
                   axis.title = ggplot2::element_text(color = "black", face = "bold"),
                   strip.background = ggplot2::element_rect(fill = "grey90"),
                   strip.text = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                   legend.title.position = "top",
                   legend.position = "none",
                   panel.border = ggplot2::element_rect(color = "black", fill = NA))
  
  kMeans_PCA <- patchwork::wrap_plots(kmeansPlot, pcaPlot, ncol = 2) +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 30, face = "bold"))
  
  
  return(kMeans_PCA)
}
