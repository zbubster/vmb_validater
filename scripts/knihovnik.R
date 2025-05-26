# knihovnik
# this function checks, whether there are all packages downloaded (if not, asks user, if instalation is wanted and do so). at the end, attach all input packages with library() function

knihovnik <- function(kniha) {
  # create subset of not installed packages
  not_installed <- kniha[!(kniha %in% installed.packages()[, "Package"])]
  
  # skipped, when all pckgs are already installed
  # should pckgs be installed?
  if (length(not_installed) > 0) {
    cat("Not installed packages:\n")
    cat(paste("-", not_installed), sep = "\n")
    cat("Install them now?? [y/n]: ")
    response <- tolower(readline())
    
    # exit route
    if (!response %in% c("y", "yes")) {
      return(cat("Cancelled."))
    }
    
    # install required pckgs
    install.packages(not_installed, dependencies = TRUE)
  }
  
  # attach pckgs
  for (kapitola in kniha) {
    suppressPackageStartupMessages(library(kapitola, character.only = TRUE))
    cat("DONE", kapitola, "\n")
  }
}

co <- c("terra", "dplyr", "ggplot2", "caret")
knihovnik(co)

rm(co)
gc()
