#############
# Check and install the necessary packages ----
#############

Install_Packages <- function(pkgs) {
  
  # Get which packages are not installed
  notInstalled <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  
  # Install CRAN packages
  if (length(notInstalled) > 0) {
    install.packages(notInstalled)
  }
  
  #TODO: What if they are available only in github ou biocmanager?
}


Load_Packages <- function(pkgs) {
  #TODO: Do I need to load them if I do this: deplyr::filter
}