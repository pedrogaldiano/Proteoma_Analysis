#############
# Check and install the necessary package ----
#############

Install_Packages <- function(pkgs) {
  
  # Get which packages are not installed
  notInstalled <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  
  # Install CRAN packages
  if (length(notInstalled) > 0) {
    install.packages(notInstalled)
  }
  
  #TODO: What if they are avaiable only in github ou biocmanager?
}
