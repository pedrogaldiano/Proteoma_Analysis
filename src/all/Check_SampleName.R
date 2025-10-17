#############
# Check if the samples names are valid ----

# <condition>_REP_<replicate>

# examples: 
#   NaCl_REP_1, 
#   JPMOEt_50mg_REP_1 
#   Ven_and_Actn_15mg_REP_2
#############


Check_SampleName <- function(samples) {
  
  groups <- unique(stringr::str_remove_all(samples, "_REP_[[:alnum:]]+$"))
  
  for (group in groups) {
    occurrences = sum(stringr::str_detect(groups, group))
    
    if (occurrences < 1 | (stringr::str_detect(group,"[A-z]|_|[0-9]+", negate = TRUE))) {
      problems <- append(problems, group)
    }
  }
  
  if (exists("problems")) {
    cat("\nThis name must be unique and not a substring of another name.
        \nThis name must contain only letters, underscore ('_') and numbers
        \nCheck those names:\n")
    
    for (name in problems) {
      cat(name, "\n")
    }
  } else {
    cat("\nSample Name seems fine!.\n")
  }
}
