#############
# Generate Box Plot of protein abundance comparison ----

#############


Compare_ProteinsAbundance <- function(imtx,
                                      genes,
                                      colorList,
                                      labels,
                                      n_columns = 4) {
  
  imtx <- imtx |>
    as.data.frame() |>
    tibble::rownames_to_column("protein")
  
  
  comparisonData <- imtx |>
    dplyr::filter(protein %in% genes) |>
    tidyr::pivot_longer(cols = -c(protein),
                        names_to = "sample",
                        values_to = "abundance") |>
    dplyr::mutate(condition = stringr::str_remove(sample, "_REP_."),
                  condition = factor(condition, levels = unique(condition)))
  
  comparisonPlot <- comparisonData |> 
    ggplot2::ggplot(ggplot2::aes(x = condition, y = abundance, fill = condition)) +
    ggplot2::geom_boxplot(outlier.alpha = 0.4, linewidth = 0.25) +
    ggplot2::geom_jitter(ggplot2::aes(color = condition), alpha = 0.7, size = 0.5) +
    ggplot2::labs(x = NULL, y = "log<sub>2</sub>(Abundance)") +
    
    ggplot2::scale_x_discrete(labels = labels) +
    
    
    ggplot2::facet_wrap(~protein, ncol = n_columns, scales = "free_y") +
    ggplot2::scale_fill_manual(values = colorList) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 20, face = "bold", color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title = ggtext::element_markdown(size = 20, face = "bold"),
      axis.ticks = ggplot2::element_line(color = "black"),
      panel.border = ggplot2::element_rect(color = "black", fill=NA)
    )
  
  return(comparisonPlot)
}
