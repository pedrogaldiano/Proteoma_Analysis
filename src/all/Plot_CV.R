#############
# Generate Graph Coefficient of Variation  ----

#############


CV_Matrix <- function(data, condition) {
  
  cat(condition, "\n")
  data <- data |> as.data.frame() |> 
    tibble::rownames_to_column(var = "protein")
  
  selected <- dplyr::select(
    data,
    protein,
    stringr::str_subset(colnames(data), condition)
  ) |> dplyr::rowwise()
  
  mutated <- dplyr::mutate(selected,
                           dplyr::across(where(is.numeric), ~ 2^(.) - 1),
                           cv = 100 *
                             (
                               sd(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE) /
                                 mean(dplyr::c_across(dplyr::where(is.numeric)), na.rm = TRUE)
                             ),
                           condition = condition
  )
  
  
  return(dplyr::select(mutated, protein, cv, condition))
}


Plot_CV <- function(imtx,
                                          conditions,
                                          labelsName,
                                          colorList) {
  
  CVs <- data.frame()
  
  for (condition in conditions) {
    CVs <- dplyr::bind_rows(CVs, CV_Matrix(imtx, condition = condition))
  }
  
  CVs |>
    dplyr::mutate(
      condition = factor(condition,
                         levels = names(labelsName)))

   CVsPlot <- CVs |>
    ggplot2::ggplot(ggplot2::aes(y = condition, x = cv, fill = condition)) +
     ggplot2::geom_violin(linewidth = 0.2) +
     ggplot2::geom_boxplot(width = 0.2, fill = "white",
                 linewidth = 0.2, outlier.alpha = 0.5) +
     ggplot2::geom_vline(xintercept = 20, linetype = "dashed", color = "black") +
     ggplot2::scale_fill_manual(values = colorList) +
     ggplot2::labs(
      y = NULL,
      x = "Coefficient of variation (%)",
      fill = NULL) +
     ggplot2::theme_linedraw() +
     ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_text(size = 20, face = "bold",),
      axis.text = ggplot2::element_text(color = "black", size = 20, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks = ggplot2::element_line(),
      line = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 20, face = "bold", color = "black")) +
     ggplot2::scale_y_discrete(labels = labelsName)
  
   
  return(CVsPlot)
}
