#############
# Create a Limma Matrix ----

#This file contain all limma related functions
#############

Create_ContrastFitQuantUMS <- function(
  imtx,
  comparisonList,
  method = "robust"
) {
  if (!method %in% c("robust", "ls")) {
    cat("method should recieve 'ls' or 'robust'.")
    break
  }

  mtx <- limma::normalizeBetweenArrays(imtx, method = "scale")

  cols <- stringr::str_remove(colnames(mtx), "_REP_.")
  groups <- factor(cols, levels = unique(cols))

  designGroups <- model.matrix(~ 0 + groups)

  colnames(designGroups) <- stringr::str_remove(
    colnames(designGroups),
    "groups"
  )

  contrastMatrix <- do.call(
    limma::makeContrasts,
    c(comparisonList, list(levels = designGroups))
  )

  fitQuantUMS <- limma::lmFit(mtx, designGroups, method = method)

  contrastFitQuantUMS <- limma::eBayes(
    limma::contrasts.fit(fitQuantUMS, contrastMatrix)
  )

  return(contrastFitQuantUMS)
}


ApplyLimmaTopTable <- function(contrastFitQuantUMS, comparison) {
  limmaResult <- limma::topTable(
    contrastFitQuantUMS,
    coef = comparison,
    number = Inf,
    adjust.method = "BH"
  ) |>
    tibble::rownames_to_column("proteins") |>
    dplyr::mutate(
      status = dplyr::case_when(
        logFC > 0.58 & adj.P.Val <= 0.05 ~ "Increased",
        logFC < -0.58 & adj.P.Val <= 0.05 ~ "Decreased",
        TRUE ~ "Not significant"
      ),
      status = factor(
        status,
        levels = c("Decreased", "Not significant", "Increased")
      ),
      condition = comparison
    )

  return(limmaResult)
}

CompareLimmaResults <- function(contrastFit, comparisonList) {
  resultQuantUMS <- list() # Result from samples comparison

  for (item in names(comparisonList)) {
    limmaResult <- ApplyLimmaTopTable(contrastFit, item)
    resultQuantUMS <- dplyr::bind_rows(resultQuantUMS, limmaResult)
  }

  result <- resultQuantUMS |>
    dplyr::mutate(
      logFC = round(logFC, 2),
      AveExpr = round(AveExpr, 2),
      B = round(B, 2)
    ) |>
    dplyr::rename(Protein = proteins, Comparison = condition)

  cat("\n\nComparisons between groups done.\n")

  return(result)
}
