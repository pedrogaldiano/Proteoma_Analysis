#############
# Generate a Diann Matrix  ----

# Create a Diann Matrix using the package Diann

#Header
#  Genes -> Gene name
#  Protein.Group -> Protein name
#  Protein.Names -> Protein name + organism
#############

# TODO: id.header = "Protein.Names"

Generate_DiannMatrix <- function(dr, header = c("Genes", "Protein.Group", "Protein.Names")) {
  
header <- match.arg(header)

  result <- diann::diann_matrix(
    dr,
    id.header = header,
    quantity.header = "Genes.MaxLFQ.Unique",
    proteotypic.only = TRUE,
    pg.q = 0.01
  )
  
  return(result)
}

