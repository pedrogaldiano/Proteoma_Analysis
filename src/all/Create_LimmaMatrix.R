#############
# Create a Limma Matrix ----

#This file contain all limma related functions
#############

Create_ContrastFitQuantUMS <- function(imtx) {
  imtx <- imputedMatrix
  comparison <- samplesToCompare
  method = "robust" #or "lm"
  
  if (!method %in% c("robust", "lm"))
  {
    cat("method should recieve 'lm' or 'robust'.")
    break
  }
  
  mtx <- limma::normalizeBetweenArrays(imtx, method = "scale")
  
  cols <- stringr::str_remove(colnames(mtx), "_REP_.")
  groups <- factor(cols, levels = unique(cols))
  
  
  designGroups <- model.matrix(~ 0 + groups)
  
  colnames(designGroups) <- stringr::str_remove(colnames(designGroups), "groups")

  
  contrastMatrix <- do.call(
    limma::makeContrasts,
    c(comparison, list(levels = designGroups))
  )  
  
  fitQuantUMS <- limma::lmFit(mtx, designGroups, method = method)

  contrastFitQuantUMS <- limma::eBayes(
    limma::contrasts.fit(fitQuantUMS, contrastMatrix)
    )
  
  return(contrastFitQuantUMS)
}
