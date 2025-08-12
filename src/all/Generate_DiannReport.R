#############
# Generate diann report  ----

# Generate the diannReport.rds from report.parquet file using arrow library 
# and making some basic filtering
#############

Generate_DiannReport <- function(
    samples,
    organism = "",
    parquetDIR = "./data/input/report.parquet",
    rdsDIR = "./data/temp_files/diannReport.rds"
) {
  
  if (file.exists(rdsDIR)) {
    return(readRDS(rdsDIR))
  }

  report <- arrow::read_parquet(parquetDIR)
  
  report <- dplyr::filter(
    report,
    Lib.PG.Q.Value <= 0.01 &
      Lib.Q.Value <= 0.01 &
      PG.Q.Value <= 0.01
  )
        
  if (organism != "") {
    report <- dplyr::filter(report, stringr::str_detect(Protein.Names, organism))
  }
  
  report <- dplyr::mutate(
    report,
    Run = dplyr::recode(Run, !!!samples),
    Run = factor(Run, levels = unname(samples)),
    condition = stringr::str_remove(Run, "_REP_."),
    File.Name = Run,
    peptide_length = nchar(Stripped.Sequence)
    )
  
  saveRDS(report, rdsDIR)
  return(report)
}
