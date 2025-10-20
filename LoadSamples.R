#############
# Put all the samples and relations in the same place ----

#############

# Name used for Run in the report.part
# tibble::view(unique(arrow::read_parquet("./data/input/report.parquet")$Run))

samplesDF <- read.csv2(
  "./Data/input/sample_name_mapping.csv",
  header = TRUE,
  sep = ";"
)

#Mix A -> 200ng HeLa + 300ng Yeast
#Mix B -> 200ng HeLa + 100ng Yeast
#Mix C -> 200ng HeLa + 50ng Yeast

fancyLabels <- setNames(samplesDF$PrettyName, samplesDF$SampleName)

samplesToCompare = list(
  MixA_vs_MixB = "Mix_A - Mix_B",
  MixA_vs_MixC = "Mix_A - Mix_C",
  MixB_vs_MixC = "Mix_B - Mix_C"
)



colors <- c(
  "#e31a1c",
  "#1f78b4",
  "#008000"
)

clusterColor <- c(
  "#e31a1c",
  "#1f78b4",
  "#008000"
)


genesToHighLight_BlandAltman <- c(
  "ACL4",
  "EIF1B",
  "CAMK1",
  "NECTIN1",
  "CDC25C",
  "RFA3 ",
  "NUP57",
  "SPHK1",
  "NUC1",
  "CDPF1",
  "TNFAIP1",
  "RDL2 ",
  "H3-3A",
  "SDCCAG8",
  "RAB43",
  "OST3",
  "YDR391C",
  "CDNF ",
  "PAAT",
  "SHE2",
  "MICALL2",
  "STT4",
  "YNL108C",
  "LMO4 ",
  "BHLHE40",
  "TRIQK",
  "YIF1",
  "HPM1",
  "SENP8",
  "HBG2",
  "ROT2"
)


genesToCompare <- c(
  "ACL4",
  "EIF1B",
  "CAMK1",
  "NECTIN1",
  "CDC25C"
)

# List of needed packages
# packages <- c(
#   "diann",           # to extract the MaxLFQ matrix from DIANN report
#   "arrow",           # to read the report.parquet file
#   "here",            # to avoid the need for use the path while loading the data
#   "tidyverse",       # to do the data wrangling, plots, etc...
#   "janitor",         # to clean the column names
#   "ggpointdensity",  # to reconstruct the m/z density map
#   "naniar",          # for sparsity analysis
#   "factoextra",      # to plot the PCA
#   "patchwork",       # to combine plots
#   "lsa",             # to calculate the cosine similarity
#   "ggvenn",          # to plot the Venn diagram
#   "paletteer",       # to use the nice color palette
#   "ggtext",          # to provide Markdown and HTML formatting in ggplot2
#   "ggrepel",         # to avoid the overlapping of the labels in the plots
#   "kableExtra",      # to format the tables
#   "limma",           # to calculate the differential abundance
#   "DIAgui"           # (no comment provided)
# )
