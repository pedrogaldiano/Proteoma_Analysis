#############
# Check if the samples names are valid ----

# <condition>_REP_<replicate>

# examples: 
#   NaCl_REP_1, 
#   JPMOEt_50mg_REP_1 
#   Ven_and_Actn_15mg_REP_2
#############


Check_SampleName <- function(samples) {
  groups <- unique(stringr::str_remove_all(unname(samples), "_REP_.$"))
  
  for (group in groups) {
    occurrences = sum(stringr::str_detect(groups, group))
    
    if (occurrences > 1) {
      cat("\nThis name should be unique and not a substring of another name:", 
          group,
          "\nPlease, rename.")
      break
    } else if (stringr::str_detect(group,"[A-z]|_|[0-9]+",negate = TRUE)) {
      cat("\nThis name should contain only letters, underscore ('_') and number:",
          group,
          "\nPlease, rename.")
      break
    }
  }
  cat("\nThe samples names seems ok")
}




