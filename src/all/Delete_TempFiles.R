#############
# Delete Temp Files ----

# Useful to clean up all .rds files created
#############

Delete_TempFiles <- function(path = "././data/temp_files") {
  if (dir.exists(path)) {
    tempFiles <- list.files(path, full.names = TRUE)

    file.remove(tempFiles)
  } else {
    cat("Path doesn't exist")
  }
}
