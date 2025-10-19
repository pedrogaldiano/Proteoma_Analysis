#############
# Generate a Graph Bland-Atman ----

#############

Count_SignificantProteins <- function(limmaResults) {
  
  # count the number of proteins in each status
  signif_proteins <- limmaResults |>
    dplyr::filter(status != "Not significant") |>
    dplyr::group_by(status, Comparison) |>
    dplyr::summarise(proteins = dplyr::n())
  
  return(signif_proteins)
}


Plot_BlandAltman <- function(limmaResults, genesToHighLight, labels, cols = 3) {
  
  signifProteins <- Count_SignificantProteins(limmaResults)
  
  blandAltmanLimmaPlot <- ggplot2::ggplot(limmaResults,
                                          ggplot2::aes(x = AveExpr,
                                                       y = logFC,
                                                       color = status)
  ) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_smooth(method = "gam", se = FALSE, color = "darkblue", linewidth = 1) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5, alpha = 1)),
                    shape = ggplot2::guide_legend(override.aes = list(size = 5, alpha = 1))) +
    ggplot2::geom_hline(yintercept = c(-0.58, 0.58), linetype = "dashed", color = "black") +
    ggplot2::scale_color_manual(values = c("Decreased" = "steelblue",
                                           "Not significant" = "grey60",
                                           "Increased" = "firebrick")) +
    ggplot2::facet_wrap(~Comparison, scales = "free", ncol = cols,
                        labeller = ggplot2::as_labeller(labels)) + 
    ggplot2::geom_text(data = signifProteins |> 
                         dplyr::filter(status == "Decreased"),
                       ggplot2::aes(x = 13.5, y = -3, label = paste0(proteins)),
                       size = 8, fontface = "bold",
                       hjust = 0.5,
                       vjust = -1, 
                       show.legend = FALSE) +
    ggplot2::geom_text(data = signifProteins |> 
                         dplyr::filter(status == "Increased"),
                       ggplot2::aes(x = 13.5, y = 1, label = paste0(proteins)),
                       size = 8, fontface = "bold",
                       hjust = 0.5,
                       vjust = -1, 
                       show.legend = FALSE) +
    ggrepel::geom_label_repel(data = limmaResults |>
                                dplyr::filter(status != "Not significant" & Protein %in% genesToHighLight),
                              ggplot2::aes(label = paste0(Protein)),
                              show.legend = FALSE) +
    ggplot2::labs(title = "Bland-Altman plots of limma analysis",
                  x = "log<sub>2</sub>(Average protein abundance)",
                  y = "log<sub>2</sub>FC") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                   text = ggplot2::element_text(size = 20),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.box = "vertical",
                   strip.text = ggplot2::element_text(size = 15, face = "bold", color = "black"),
                   axis.text = ggplot2::element_text(color = "black"),
                   axis.title = ggtext::element_markdown(size = 20, face = "bold"),
                   axis.ticks = ggplot2::element_line(color = "black"),
                   panel.border = ggplot2::element_rect(color = "black", fill = NA)
    )
  
  return(blandAltmanLimmaPlot)
}
