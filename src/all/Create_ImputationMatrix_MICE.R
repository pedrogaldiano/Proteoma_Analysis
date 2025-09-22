#############
# Create Imputation Matrix MICE  ----

# Impute values to NA places using MICE package

#############


Create_ImputarionMatrix_MICE <- function(dm,
                                         methodValue,
                                         override = TRUE,
                                         mValue = 5,
                                         maxitValue = 5,
                                         seed = 123,
                                         path = "././data/temp_files/")
  {
  
  fileName <- paste0(path, "result_imputation_MICE_", methodValue, ".rds")
  fileExist <- file.exists(fileName)
  
  if (!(override) & fileExist) {
    
    cat("\nLoad file:", fileName,"\n")
    return(readRDS(fileName))
  } else {
    
    
    matrixForImputation <- log2(dm + 1) |>
      as.data.frame() |>
      tibble::rownames_to_column(var = "protein")
  
    cat("\nCreating the imputed matrix.\n\nThis is going to take a while.\n")
    
    
    # methods = rf (random forest imputations), pmm (predictive mean matching)
    imputedMatrix <- mice::mice(matrixForImputation,
                                method = methodValue,
                                m = mValue,
                                maxit = maxitValue,
                                seed = seed)
    
    result <- mice::complete(imputedMatrix)
    
    
    result <- result |>
      tibble::column_to_rownames(var = "protein") |>
      as.matrix()
    
    saveRDS(result, file = fileName)
    
    return(result)
  }
}




