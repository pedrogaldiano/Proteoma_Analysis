
#############
# Create Imputation Matrix Step Wise  ----
# Two-Step Approach

# Step 1: Handles proteins that are completely missing in entire groups by 
# imputing values slightly below the detection limit with added noise

# Step 2: Handles remaining missing values using either:
# Mean imputation for low-abundance proteins (simpler, more stable)
# MICE (Multiple Imputation) for high-abundance 
# proteins (more sophisticated, preserves relationships)
# This approach recognizes that low-abundance proteins often have more
# technical noise, so simpler imputation is appropriate, while high-abundance
# proteins benefit from more sophisticated methods that preserve biological
# relationships.

#############

Create_ImputationMatrix_StepWise <- function(dm,
                                             override = TRUE,
                                             path = "././data/temp_files/") {

  fileName <- paste0(path, "result_imputation_stepWise.rds")
  fileExist <- file.exists(fileName)
  
  if (!(override) & fileExist) {
    
    cat("\nLoad file:", fileName,"\n")
    return(readRDS(fileName))
  } else {
    

    matrixForImputation <- log2(dm + 1)
    groupLabels <- stringr::str_remove(colnames(matrixForImputation), "_REP_.")
    
    imputedData <- StepWiseImputation(
      protein_matrix = matrixForImputation,
      group_vector = groupLabels,
      shift_value = -1.5,
      noise_type = "normal",
      noise_sd = 0.1,
      min_detection_method = "global",
      verbose = TRUE
    )
    result <- imputedData$imputed_data
    # log <- imputedData$missing_protein_log
    
    saveRDS(result, fileName)
    return(result)
  }
}



StepWiseImputation <- function(
    protein_matrix,
    group_vector,
    use_mean_for_low = TRUE,
    abundance_threshold = NULL,
    shift_value = -1,
    noise_type = "normal",
    noise_sd = 0.1,
    noise_range = c(-0.2, 0.2),
    min_detection_method = "global",
    seed = 123,
    verbose = TRUE
) {
  set.seed(seed)
  result_matrix <- protein_matrix
  unique_groups <- unique(group_vector)
  missing_protein_log <- data.frame()
  
  # Step 1: Handle completely missing proteins within groups
  if (verbose) {
    cat("Step 1: Detecting and handling completely missing proteins...\n")
  }
  
  for (protein in 1:nrow(protein_matrix)) {
    protein_values <- protein_matrix[protein, ]
    
    # Determine minimum value based on method
    if (min_detection_method == "global") {
      min_value <- min(protein_values, na.rm = TRUE)
    } else {
      # Group-wise: find minimum among groups that have this protein
      group_mins <- sapply(unique_groups, function(g) {
        g_indices <- which(group_vector == g)
        g_values <- protein_values[g_indices]
        if (all(is.na(g_values))) {
          return(Inf)
        }
        return(min(g_values, na.rm = TRUE))
      })
      min_value <- min(group_mins[!is.infinite(group_mins)])
    }
    
    # Skip if all values are missing for this protein
    if (is.infinite(min_value)) {
      next
    }
    
    # Check each group for complete missingness
    for (group in unique_groups) {
      group_indices <- which(group_vector == group)
      group_protein_values <- protein_values[group_indices]
      
      # If all values in this group are missing
      if (all(is.na(group_protein_values))) {
        if (verbose) {
          cat(sprintf(
            "  Protein %d completely missing in group %s (min_value: %.3f)\n",
            protein,
            group,
            min_value
          ))
        }
        
        # Log this imputation
        missing_protein_log <- rbind(
          missing_protein_log,
          data.frame(
            protein = protein,
            group = group,
            min_value = min_value,
            shifted_value = min_value + shift_value
          )
        )
        
        # Calculate shifted value
        shifted_value <- min_value + shift_value
        
        # Generate values with noise for this group
        n_samples <- length(group_indices)
        
        if (noise_type == "normal") {
          noise <- rnorm(n_samples, mean = 0, sd = noise_sd)
        } else if (noise_type == "uniform") {
          noise <- runif(n_samples, min = noise_range[1], max = noise_range[2])
        }
        
        imputed_values <- shifted_value + noise
        
        # Assign to result matrix
        result_matrix[protein, group_indices] <- imputed_values
      }
    }
  }
  
  # Step 2: Main imputation for remaining missing values
  if (verbose) {
    cat("Step 2: Performing main imputation...\n")
  }
  
  # Calculate overall abundance threshold if not provided
  if (is.null(abundance_threshold)) {
    abundance_threshold <- median(result_matrix, na.rm = TRUE)
  }
  
  for (group in unique_groups) {
    if (verbose) {
      cat("  Processing group", group, "...\n")
    }
    
    group_indices <- which(group_vector == group)
    group_data <- result_matrix[, group_indices, drop = FALSE]
    
    if (any(is.na(group_data))) {
      for (protein in 1:nrow(group_data)) {
        protein_data <- group_data[protein, ]
        
        if (any(is.na(protein_data))) {
          non_na_values <- protein_data[!is.na(protein_data)]
          
          if (length(non_na_values) == 0) {
            # All missing - use threshold value (shouldn't happen after step 1)
            protein_data[] <- abundance_threshold
          } else if (length(non_na_values) == 1) {
            # Only one value - use it
            protein_data[is.na(protein_data)] <- non_na_values[1]
            
            # Does nothing because it means there is 
            # only one protein abundance in the group
            
          } else {
            # Multiple values available
            protein_median <- median(non_na_values)
            
            if (
              use_mean_for_low &&
              !is.na(protein_median) &&
              protein_median <= abundance_threshold
            ) {
              # Low abundance - use mean
              protein_mean <- mean(non_na_values)
              protein_data[is.na(protein_data)] <- protein_mean
            } else {
              # High abundance - try mice, fallback to mean
              temp_df <- data.frame(
                value = protein_data,
                group_id = rep(1, length(protein_data))
              )
              
              tryCatch(
                {
                  mice_result <- mice::mice(
                    temp_df[, "value", drop = FALSE],
                    m = 1,
                    method = "pmm",
                    printFlag = FALSE
                  )
                  #Not sure if should be mice or tidyr
                  completed <- mice::complete(mice_result, 1)
                  protein_data <- completed[, 1]
                },
                error = function(e) {
                  # Fallback to mean
                  protein_mean <- mean(non_na_values)
                  protein_data[is.na(protein_data)] <<- protein_mean
                }
              )
            }
          }
          
          result_matrix[protein, group_indices] <- protein_data
        }
      }
    }
  }
  
  # Return results
  return(list(
    imputed_data = na.omit(result_matrix),
    missing_protein_log = missing_protein_log,
    parameters = list(
      shift_value = shift_value,
      noise_type = noise_type,
      noise_sd = noise_sd,
      min_detection_method = min_detection_method
    )
  ))
}
