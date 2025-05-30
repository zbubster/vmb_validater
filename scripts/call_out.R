#---#---#---#---#---#---#---#---#---#---#---#---# 
#                    Function                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

# this function waits for user input, whether something should be excluded
call_out <- function(col){
  repeat{
    out <- readline(paste0("Do you want to exclude something from collumn ", as.character(col), "? [T/F]: "))
    ifelse(toupper(out) %in% c("T", "F"),
           break,
           cat("Invalid input!\n"))
  }
  out <- toupper(out) == "T"
  return(out)
}