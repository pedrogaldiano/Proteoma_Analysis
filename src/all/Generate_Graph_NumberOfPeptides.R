#############
# Generate Number of Peptides  ----

# Counting the number of unique peptides per run
#############


Generate_Graph_NumberOfPeptides <- function(dr, labels, colors) {
  totalUniquePeptides <- dplyr::n_distinct(dr$Stripped.Sequence)
  
  peptByRun <- dplyr::summarise(
    dplyr::group_by(dr, Run, condition), 
    n_peptides = dplyr::n_distinct(Stripped.Sequence)
  )
  
  peptidesPlot <- ggplot2::ggplot(peptByRun,
                                  ggplot2::aes(y = Run,
                                               x = n_peptides,
                                               fill = condition)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_peptides, hjust =  +1),
      color = "black", size = 2, fontface = "bold"
    ) +
    ggplot2::labs(y = NULL,
                  x = NULL,
                  subtitle = paste(totalUniquePeptides, "unique peptides"),
                  title = "Number of peptides",
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
  
  return(peptidesPlot)
}


