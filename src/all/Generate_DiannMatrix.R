#############
# Generate a Diann Matrix  ----

# Create a Diann Matrix using the package Diann
#############

# TODO: id.header = "Protein.Names"

Generate_DiannMatrix <- function(dr, header = "Genes") {
  if (header == "Protein") {
    result <- diann::diann_matrix(
      dr,
      id.header = "Protein.Group",
      quantity.header = "Genes.MaxLFQ.Unique",
      proteotypic.only = TRUE,
      pg.q = 0.01
    )

    return(result)
  } else if (header == "Genes") {
    result <- diann::diann_matrix(
      dr,
      id.header = "Genes",
      quantity.header = "Genes.MaxLFQ.Unique",
      proteotypic.only = TRUE,
      pg.q = 0.01
    )

    return(result)
  }
}
