#############
# Generate Number of Proteins  ----

# Counting the number of unique proteins per run
#############

Generate_Graph_NumberOfProteins <- function(dr,labels, colors) {
  totalUniqueProteins <- dplyr::n_distinct(dr$Protein.Ids)
  
  protByRun <- dplyr::summarise(
    dplyr::group_by(dr, Run, condition), 
    n_proteins = dplyr::n_distinct(Protein.Ids)
  )
  
  proteinsPlot <- ggplot2::ggplot(protByRun,
                                  ggplot2::aes(y = Run,
                                               x = n_proteins,
                                               fill = condition)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_proteins, hjust =  +1),
      color = "black", size = 2, fontface = "bold"
    ) +
    ggplot2::labs(y = NULL,
                  x = NULL,
                  subtitle = paste(totalUniqueProteins, "unique proteins"),
                  title = "Number of proteins",
                  fill = NULL) +
    ggplot2::scale_y_discrete(labels = labels) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8, vjust = 0.5, hjust = 1),
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      panel.background = ggplot2::element_blank()
    )
  
  return(proteinsPlot)
}
