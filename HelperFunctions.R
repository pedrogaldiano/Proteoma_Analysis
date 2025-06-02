
# Helper functions ----

# Return a dataset with the coefficients of variation
CV_Matrix <- function(data, condition) {
  
  df <- as.data.frame(data)
  
  selected <- dplyr::select(df,
                            protein,
                            str_subset(colnames(data), condition))
  
  selected_rowwise <- rowwise(selected)
  
  mutated <- dplyr::mutate(selected_rowwise,
                           across(where(is.numeric),~2^(.) - 1),
                           cv = 100 * 
                             (
                               sd(c_across(where(is.numeric)),na.rm = TRUE) /
                                 mean(c_across(where(is.numeric)),na.rm = TRUE)
                             ),
                           condition = condition)
  
  cat("Coefficient of variation calculated for each protein (row)\n")
  
  return(dplyr::select(mutated, protein, cv, condition))
}

# This function works to remove the proteins with more 
# than a specific percentage of missing values
RemoveMissingAboveThreshold <- function(data, threshold) {
  df <- as.data.frame(data)
  
MeanOfMissing <- function(df) {
    return(mean(is.na(df)))
  }
  
  addedMissingness = dplyr::mutate(df,
                                   prot_miss = apply(df,1,MeanOfMissing)
  )
  
  cat("Proteins with missingness above", threshold, "were removed from dataset\n")
  
  return(dplyr::filter(addedMissingness, prot_miss > threshold))
}

# Generate the diannReport.rds from report.parquet file
DiannReport <- function(labels, levels, directory = "../data/DIANN_results/") {
  
  if(file.exists("diannReport.rds")) { 
    
    cat("\nDiannReport.rds were loaded.\n")
    return(read_rds("diannReport.rds")) 
    }
  
  labels <- as.character(labels)
  levels <- as.character(levels)
  
  report <- arrow::read_parquet(paste(directory, "report.parquet", sep=""))
  
  reportFiltered <- dplyr::filter(report,
                                  Lib.PG.Q.Value <= 0.01 
                                  & Lib.Q.Value <= 0.01 
                                  & PG.Q.Value <= 0.01)
  
  
  reportRecoded <- dplyr::mutate(reportFiltered,
                                Run = recode(Run, !!!labels),
                                Run = factor(Run, levels = levels),
                                condition = str_remove(Run, " r1| r2| r3"),
                                File.Name = Run,
                                peptide_length = nchar(Stripped.Sequence)
  )
  
  diannReport <- dplyr::filter(reportRecoded,
                               str_detect(Protein.Names, "MOUSE")
                               )
  
  write_rds(diannReport, file = "diannReport.rds")
  cat("\nDiannReport.rds were created\n")
  
  return(diannReport)
}



# Graph: Precursor x RT ----

precursor_rt <- DiannReport(labels, levels) %>%
  ggplot(aes(x = RT, y = Precursor.Quantity)) +
  geom_line(aes(color = condition), show.legend = FALSE) +
  scale_color_manual(values = colors) +
  labs(x = "Retention time (min)",
       y = "Precursor quantity",
       color = NULL) +
  facet_wrap(~Run, ncol = 6, scales = "free") +
  theme(strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank()
  )

precursor_x_RT <- (precursor_rt)
plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 30, face = "bold"))

ggsave(filename = "Figure_1-6.png",
       path = "plots",
       plot = Figure_1,
       width = 24, height = 20,
       units = "in", dpi = 300)


# adjust the general theme for the plots
# theme_update(
#   text = element_text(color = "black", size = 20),
#   axis.text = element_text(color = "black"),
#   axis.title = element_text(color = "black", face = "bold"),
#   strip.background = element_blank(),
#   strip.text = element_text(face = "bold"),
#   legend.title = element_text(face = "bold", hjust = 0.5),
#   legend.title.position = "top"
# )