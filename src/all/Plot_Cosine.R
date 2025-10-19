#############
# Generate Cosine Similarity  ----

#Compares graphically the difference between samples using cosine similarity
#############


Plot_Cosine <- function(imtx, labels) {
  
  cosine <- as.data.frame(lsa::cosine(imtx))
  cosine <- tibble::rownames_to_column(cosine, var = "Sample")
  cosine <- tidyr::pivot_longer(cosine, -Sample, names_to = "Match")
  cosine <- dplyr::mutate(cosine,
                          Similarity = "Cosine similarity",
                          Sample = factor(Sample, levels = names(labels)),
                          Match = factor(Match, levels = names(labels)))
  
  cosinePlot <- ggplot2::ggplot(cosine) +
    ggplot2::geom_tile(ggplot2::aes(x = Sample, y = Match, fill = value)) +
    viridis::scale_fill_viridis(option = "E") +
    ggplot2::labs(title = "Similarity matrix",
                  x = NULL,
                  y = NULL,
                  fill = "Cosine similarity") +
    # theme_minimal() +
    ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::scale_x_discrete(labels = labels) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      text = ggplot2::element_text(size = 20),
      axis.text.x = ggplot2::element_text(
        angle = 90, hjust = 1, vjust = 0.5, color = "black", size = 35),
      axis.text.y = ggplot2::element_text(
        angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 35),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.key.width = ggplot2::unit(2.5, "cm"),
      legend.key.height = ggplot2::unit(0.3, "cm"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      panel.background = ggplot2::element_blank())
  
  return(cosinePlot)
}
