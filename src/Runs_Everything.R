

filesSource <- list.files(
  c("./src/requirement","./src/read", "./src/process"),
  pattern="*.R$",
  full.names = TRUE,
  ignore.case = TRUE)


sapply(filesSource, source, .GlobalEnv)


