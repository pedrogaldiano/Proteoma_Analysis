
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
library(paletteer) # to use the nice color palette
library(ggtext) # to provide Markdown and HTML formatting in ggplot2
library(ggrepel) # to avoid the overlapping of the labels in the plots
library(limma) # to calculate the differential abundance
library(Mfuzz)
library(e1071)

# adjust the general theme for the plots
theme_update(
  text = element_text(color = "black", size = 20),
  axis.text = element_text(color = "black"),
  axis.title = element_text(color = "black", face = "bold"),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold"),
  legend.title = element_text(face = "bold", hjust = 0.5),
  legend.title.position = "top"
)

#  Load the helper functions to be used in the analysis -------------------

# Function to extract the coefficients of variation from the protein abundance matrix
compute_cv_mtx <- function(protein_matrix, group_labels) {
  # protein_matrix: rows = proteins, columns = samples
  # group_labels: vector indicating group membership for each sample

  unique_groups <- unique(group_labels)
  cv_results <- data.frame(row.names = rownames(protein_matrix))

  for (group in unique_groups) {
    # Get samples belonging to current group
    group_samples <- which(group_labels == group)
    group_data <- protein_matrix[, group_samples, drop = FALSE]

    # Calculate CV for each protein in this group
    group_means <- rowMeans(group_data, na.rm = TRUE)
    group_sds <- apply(group_data, 1, sd, na.rm = TRUE)

    # CV = (standard deviation / mean) * 100
    cv_values <- (group_sds / group_means) * 100

    # Store results
    cv_results[[paste0("CV_", group)]] <- cv_values
  }

  return(cv_results)
}

# This function will create a column to store the percentage of missing values for each protein
protein_missingness <- function(x) {
  missingness_prop <- function(x) {
    sum(is.na(x)) / length(x) # calculate the percentage of missing values
  }
  x <- as.data.frame(x) %>% # convert the matrix to a dataframe
    dplyr::mutate(prot_miss = apply(x, 1, missingness_prop)) # apply the missingness_prop function to each row
}

# This function works to remove the proteins with more than a specific percentage of missing values
remove_missing <- function(x, threshold) {
  x <- as.data.frame(x) %>% # convert the matrix to a dataframe (just to make sure you are working with a dataframe)
    dplyr::filter(prot_miss <= threshold) %>% # filter the proteins with less than or equal to the threshold
    dplyr::select(-prot_miss) %>% # remove the prot_miss column
    as.matrix() # convert the dataframe back to a matrix
}

# The function handles missing protein abundance values using a two-step approach that's specifically tailored for proteomics data characteristics.

# protein_matrix: protein abundance data (rows = proteins, columns = samples)
# group_vector: Sample group labels (e.g., "Control", "Treatment1", "Treatment2")
# use_mean_for_low: When TRUE, uses mean imputation for low-abundance proteins, more sophisticated methods for high-abundance ones
# abundance_threshold: Cutoff to distinguish low vs high abundance proteins (if NULL, uses median)
# shift_value: How much below the minimum detected value to impute completely missing proteins (default: -1 log units)
# noise_type: Type of random noise to add ("normal" or "uniform")
# noise_sd: Standard deviation for normal noise (default: 0.1)
# noise_range: Range for uniform noise (default: -0.2 to 0.2)
# min_detection_method: "global": Uses overall minimum across all samples or "group_wise": Uses minimum within each group that has the protein

# Two-Step Approach
# Step 1: Handles proteins that are completely missing in entire groups by imputing values slightly below the detection limit with added noise
# Step 2: Handles remaining missing values using either:
# Mean imputation for low-abundance proteins (simpler, more stable)
# MICE (Multiple Imputation) for high-abundance proteins (more sophisticated, preserves relationships)
# This approach recognizes that low-abundance proteins often have more technical noise, so simpler imputation is appropriate, while high-abundance proteins benefit from more sophisticated methods that preserve biological relationships.
impute_stepWise_proteomics <- function(
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
    protein_values <- protein_matrix[protein, ] #pegou a linha

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
          #TODO: por que não fazer o desvio a partir do menor valor?
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
                  completed <- complete(mice_result, 1)
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
    imputed_data = result_matrix,
    missing_protein_log = missing_protein_log,
    parameters = list(
      shift_value = shift_value,
      noise_type = noise_type,
      noise_sd = noise_sd,
      min_detection_method = min_detection_method
    )
  ))
}

# This function will perform Mfuzz clustering
mfuzz_clutering <- function(protein_matrix, group_labels, num_clusters = 6) {
  # Step 1: Calculate group means
  unique_groups <- unique(group_labels)
  group_means <- matrix(
    NA,
    nrow = nrow(protein_matrix),
    ncol = length(unique_groups)
  )

  for (i in 1:length(unique_groups)) {
    group_samples <- which(group_labels == unique_groups[i])
    group_means[, i] <- rowMeans(protein_matrix[, group_samples], na.rm = TRUE)
  }

  rownames(group_means) <- rownames(protein_matrix)
  colnames(group_means) <- unique_groups

  # Step 2: Create ExpressionSet
  eset <- new("ExpressionSet", exprs = as.matrix(group_means))

  # Step 3: Filter and standardize
  eset <- filter.std(eset, min.std = 0) # Keep all proteins
  eset <- standardise(eset)

  # Step 4: Estimate parameters and cluster
  m <- mestimate(eset)
  clusters <- mfuzz(eset, c = num_clusters, m = m)

  # Step 5: Plot results
  mfuzz.plot2(eset, cl = clusters, mfrow = c(2, 3))

  # Return results
  return(list(clusters = clusters, eset = eset))
}

# Function to extract cluster information
extract_cluster_info <- function(mfuzz_results, membership_threshold = 0.5) {
  clusters <- mfuzz_results$clusters

  # Extract cluster assignments and membership values
  cluster_assignments <- clusters$cluster
  membership_matrix <- clusters$membership

  # Create a comprehensive results data frame
  results_df <- data.frame(
    Protein = names(cluster_assignments),
    Cluster = cluster_assignments,
    stringsAsFactors = FALSE
  )

  # Add membership values for each cluster
  for (i in 1:ncol(membership_matrix)) {
    results_df[[paste0("Membership_Cluster_", i)]] <- membership_matrix[, i]
  }

  # Add maximum membership value
  results_df$Max_Membership <- apply(membership_matrix, 1, max)

  # Filter proteins with high membership (confident assignments)
  high_confidence <- results_df[
    results_df$Max_Membership >= membership_threshold,
  ]

  return(list(
    all_assignments = results_df,
    high_confidence = high_confidence,
    membership_matrix = membership_matrix
  ))
}

# Function to create a membership heatmap
plot_membership_heatmap <- function(mfuzz_results, top_n = 50) {
  cluster_info <- extract_cluster_info(mfuzz_results)
  membership_matrix <- cluster_info$membership_matrix

  # Select top proteins with highest membership values
  max_memberships <- apply(membership_matrix, 1, max)
  top_indices <- order(max_memberships, decreasing = TRUE)[
    1:min(top_n, nrow(membership_matrix))
  ]

  # Plot heatmap of membership values
  library(pheatmap)

  pheatmap(
    membership_matrix[top_indices, ],
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    scale = "none",
    color = colorRampPalette(c("grey90", "firebrick"))(100),
    main = paste(
      "Top",
      min(top_n, nrow(membership_matrix)),
      "Proteins - Cluster Membership"
    ),
    fontsize = 8,
    fontsize_row = 6,
    angle_col = 0
  )
}

#  Import and filter the report.parquet file from DIA-NN v2.1.0 search ----
# We filter the data using Lib.PG.Q.Value ≤ 0.01, Lib.Q.Value ≤ 0.01, and PG.Q.Value ≤ 0.01 for the analysis.

diann_report <- arrow::read_parquet("report.parquet") %>%
  dplyr::filter(
    Lib.PG.Q.Value <= 0.01 & Lib.Q.Value <= 0.01 & PG.Q.Value <= 0.01
  ) %>%
  dplyr::mutate(
    Run = case_when(
      Run == "P1_02" ~ "Venom_r1",
      Run == "P1_03" ~ "Venom_r2",
      Run == "P1_04" ~ "Actinonin_30mg_r1",
      Run == "P1_05" ~ "Actinonin_30mg_r2",
      Run == "P1_07" ~ "Ven_Actn_30mg_r1",
      Run == "P1_08" ~ "Ven_Actn_30mg_r2",
      Run == "P1_09" ~ "Ven_Actn_30mg_r3",
      Run == "P1_10" ~ "JPMOEt_100mg_r1",
      Run == "P1_11" ~ "JPMOEt_100mg_r2",
      Run == "P1_12" ~ "JPMOEt_100mg_r3",
      Run == "P1_13" ~ "Ven_jPMOEt_100mg_r1",
      Run == "P1_14" ~ "Ven_jPMOEt_100mg_r2",
      Run == "P1_15" ~ "Ven_jPMOEt_100mg_r3",
      Run == "P2_01" ~ "NaCl_r1",
      Run == "P2_02" ~ "NaCl_r2",
      Run == "P2_03" ~ "NaCl_r3",
      Run == "P2_04" ~ "Actinonin_15mg_r1",
      Run == "P2_05" ~ "Actinonin_15mg_r2",
      Run == "P2_06" ~ "Actinonin_15mg_r3",
      Run == "P2_07" ~ "Ven_Actn_15mg_r1",
      Run == "P2_08" ~ "Ven_Actn_15mg_r2",
      Run == "P2_09" ~ "Ven_Actn_15mg_r3",
      Run == "P2_11" ~ "JPMOEt_50mg_r1",
      Run == "P2_12" ~ "JPMOEt_50mg_r2",
      Run == "P2_13" ~ "Ven_jPMOEt_50mg_r1",
      Run == "P2_14" ~ "Ven_jPMOEt_50mg_r2",
      Run == "P2_15" ~ "Ven_jPMOEt_50mg_r3"
    ),
    Run = factor(
      Run,
      levels = c(
        "NaCl_r1",
        "NaCl_r2",
        "NaCl_r3",
        "Actinonin_15mg_r1",
        "Actinonin_15mg_r2",
        "Actinonin_15mg_r3",
        "Actinonin_30mg_r1",
        "Actinonin_30mg_r2",
        "JPMOEt_50mg_r1",
        "JPMOEt_50mg_r2",
        "JPMOEt_100mg_r1",
        "JPMOEt_100mg_r2",
        "JPMOEt_100mg_r3",
        "Venom_r1",
        "Venom_r2",
        "Ven_Actn_15mg_r1",
        "Ven_Actn_15mg_r2",
        "Ven_Actn_15mg_r3",
        "Ven_Actn_30mg_r1",
        "Ven_Actn_30mg_r2",
        "Ven_Actn_30mg_r3",
        "Ven_jPMOEt_50mg_r1",
        "Ven_jPMOEt_50mg_r2",
        "Ven_jPMOEt_50mg_r3",
        "Ven_jPMOEt_100mg_r1",
        "Ven_jPMOEt_100mg_r2",
        "Ven_jPMOEt_100mg_r3"
      )
    ),
    condition = str_remove(Run, "_r1|_r2|_r3"),
    File.Name = Run,
    peptide_length = nchar(Stripped.Sequence)
  ) %>%
  dplyr::filter(str_detect(Protein.Names, "MOUSE", negate = FALSE))


# extracting the matrix of abundance from DIA-NN report.parquet file
quantums_mtx <- diann_report %>%
  dplyr::filter(PG.MaxLFQ.Quality > 0.75 & Empirical.Quality > 0.75) %>%
  diann::diann_matrix(
    .,
    id.header = "Genes",
    quantity.header = "Genes.MaxLFQ.Unique",
    proteotypic.only = T,
    pg.q = .01
  ) 




df_for_imputation <- log2(quantums_mtx[, -1] + 1)
#TODO: Se eu vou imputar, não faz sentido remover a amostra NaCl r1 (primeira coluna)

cat(
  "Percentage of missing values:",
  mean(is.na(df_for_imputation)) * 100,
  "%\n"
)
group_labels <- str_remove(colnames(df_for_imputation), "_r\\d+")

imputed_data <- impute_stepWise_proteomics(
  protein_matrix = df_for_imputation,
  group_vector = group_labels,
  shift_value = -1.5,
  noise_type = "normal",
  noise_sd = 0.1,
  min_detection_method = "global",
  verbose = TRUE
)

view(imputed_data$imputed_data)
view(df_for_imputation)

# results:
imputed_matrix <- imputed_data$imputed_data
missing_log <- imputed_data$missing_protein_log
print(missing_log) # See which proteins were handled in step 1 of imputation

#  calculate the cosine similarity in the matrix and plot the heatmap -----
