#############
# Source all files ----
#############

filesSource <- list.files(
  c("./src/requirement","./src/read", "./src/process", "./src/all"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE)


sapply(filesSource, source, .GlobalEnv)
