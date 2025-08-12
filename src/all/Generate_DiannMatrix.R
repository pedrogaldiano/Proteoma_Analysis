#############
# Generate a Diann Matrix  ----

# Create a Diann Matrix using the package Diann
#############


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


FilterByDiannMatrix <- function(data) {
  
  result <- data %>%
    diann::diann_matrix(id.header = "Genes",
                        quantity.header = "Genes.MaxLFQ.Unique",
                        proteotypic.only = TRUE,
                        pg.q = 0.01)
  
  cat("\n\nFilter by unique genes using Diann_Matrix")
  return(result)
}