
#Libraries
library(diann) # to extract the MaxLFQ matrix from DIANN report
library(arrow) # to read the report.parquet file
library(here) # to avoid the need for use the path while loading the data
library(tidyverse) # to do the data wrangling, plots, etc...
library(janitor) # to clean the column names
library(ggpointdensity) # to reconstruct the m/z density map
library(naniar) # for sparsity analysis
library(factoextra) # to plot the PCA
library(patchwork) # to combine plots
library(lsa) # to calculate the cosine similarity
library(ggvenn) # to plot the Venn diagram
library(paletteer) # to use the nice color palette
library(ggtext) # to provide Markdown and HTML formatting in ggplot2
library(ggrepel) # to avoid the overlapping of the labels in the plots
library(kableExtra) # to format the tables
library(limma)  # to calculate the differential abundance
#library(ggplot2)

install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("https://github.com/vdemichev/diann-rpackage")
library(diann)

# make the text in the plots more readable
theme_update(
  text = element_text(color = "black", size = 20),
  axis.text = element_text(color = "black"),
  axis.title = element_text(color = "black", face = "bold"),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold"),
  legend.title = element_text(face = "bold", hjust = 0.5),
  legend.title.position = "top"
)



reportParquetDIR = "/home/pedro/Documentos/data-proteomica-urina/report.parquet"

# Import and filter the report.parquet file from DIA-NN v2.0.2 search for mix A and B
diann_report <- arrow::read_parquet(reportParquetDIR) %>%
  dplyr::filter(Lib.PG.Q.Value <= 0.01 & Lib.Q.Value <= 0.01 & PG.Q.Value <= 0.01) %>%
  dplyr::mutate(
    Run = case_when(
      Run == "P1_02" ~ "Veneno r1",
      Run == "P1_03" ~ "Veneno r2",
      Run == "P1_04" ~ "iMeprina 30mg r1",
      Run == "P1_05" ~ "iMeprina 30mg r2",
      Run == "P1_07" ~ "Veneno + iMeprina 30mg r1",
      Run == "P1_08" ~ "Veneno + iMeprina 30mg r2",
      Run == "P1_09" ~ "Veneno + iMeprina 30mg r3",
      Run == "P1_10" ~ "iCatepsina 100mg r1",
      Run == "P1_11" ~ "iCatepsina 100mg r2",
      Run == "P1_12" ~ "iCatepsina 100mg r3",
      Run == "P1_13" ~ "Veneno + iCatepsina 100mg r1",
      Run == "P1_14" ~ "Veneno + iCatepsina 100mg r2",
      Run == "P1_15" ~ "Veneno + iCatepsina 100mg r3",
      Run == "P2_01" ~ "Salina r1",
      Run == "P2_02" ~ "Salina r2",
      Run == "P2_03" ~ "Salina r3",
      Run == "P2_04" ~ "iMeprina 15mg r1",
      Run == "P2_05" ~ "iMeprina 15mg r2",
      Run == "P2_06" ~ "iMeprina 15mg r3",
      Run == "P2_07" ~ "Veneno + iMeprina 15mg r1",
      Run == "P2_08" ~ "Veneno + iMeprina 15mg r2",
      Run == "P2_09" ~ "Veneno + iMeprina 15mg r3",
      Run == "P2_11" ~ "iCatepsina 50mg r1",
      Run == "P2_12" ~ "iCatepsina 50mg r2",
      Run == "P2_13" ~ "Veneno + iCatepsina 50mg r1",
      Run == "P2_14" ~ "Veneno + iCatepsina 50mg r2",
      Run == "P2_15" ~ "Veneno + iCatepsina 50mg r3"),
    Run = factor(Run, levels = c(
      "Salina r1", "Salina r2", "Salina r3",
      "Veneno r1", "Veneno r2",
      "iMeprina 15mg r1", "iMeprina 15mg r2", "iMeprina 15mg r3",
      "Veneno + iMeprina 15mg r1", "Veneno + iMeprina 15mg r2", "Veneno + iMeprina 15mg r3",
      "iMeprina 30mg r1", "iMeprina 30mg r2",
      "Veneno + iMeprina 30mg r1", "Veneno + iMeprina 30mg r2", "Veneno + iMeprina 30mg r3",
      "iCatepsina 50mg r1", "iCatepsina 50mg r2",
      "Veneno + iCatepsina 50mg r1", "Veneno + iCatepsina 50mg r2", "Veneno + iCatepsina 50mg r3",
      "iCatepsina 100mg r1", "iCatepsina 100mg r2", "iCatepsina 100mg r3",
      "Veneno + iCatepsina 100mg r1", "Veneno + iCatepsina 100mg r2", "Veneno + iCatepsina 100mg r3")
    ),
    File.Name = Run,
    peptide_length = nchar(Stripped.Sequence)
  ) %>%
  dplyr::filter(str_detect(Protein.Ids, "cRAP", negate = TRUE))






