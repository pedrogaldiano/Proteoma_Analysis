#############
# Generate diann report  ----

# Generate the diannReport.rds from report.parquet file using arrow library
# and making some basic filtering
#############

Make_DiannReport <- function(
  mapping,
  organism = "",
  parquetDIR = "./data/input/report.parquet"
) {
  report <- arrow::read_parquet(parquetDIR)

  report <- dplyr::filter(
    report,
    Lib.PG.Q.Value <= 0.01 &
      Lib.Q.Value <= 0.01 &
      PG.Q.Value <= 0.01
  )

  if (organism != "") {
    report <- dplyr::filter(
      report,
      stringr::str_detect(Protein.Names, organism)
    )
  }

  #TODO: I don't need all columns to mix to report, maybe a inner join is better for this scenario!
  report <- dplyr::left_join(report, mapping, by = c("Run" = "Run"))

  report <- dplyr::mutate(
    report,
    Run = report$SampleName,
    Run = factor(Run, levels = mapping$SampleName),
    condition = stringr::str_remove(Run, "_REP_[[:alnum:]]+$"),
    File.Name = Run,
    peptide_length = nchar(Stripped.Sequence)
  )

  return(report)
}
