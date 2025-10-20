#############
# Filter missing values  ----

# Remove proteins where the missing value is above a threshold
#############

Acceptable_Missing <- function(data, threshold) {
  prot_miss <- NULL

  MeanOfMissing <- function(x) {
    return(mean(is.na(x)))
  }

  df <- as.data.frame(data)

  result <- dplyr::mutate(df, prot_miss = apply(df, 1, MeanOfMissing)) |>

    dplyr::filter(prot_miss <= threshold) |>
    dplyr::select(-prot_miss)

  cat("\nMissingness threshold <=", threshold)

  return(log2(as.matrix(result) + 1))
}
