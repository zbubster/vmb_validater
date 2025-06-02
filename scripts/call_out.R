#---#---#---#---#---#---#---#---#---#---#---#---# 
#                    Function                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

# this function waits for user input, whether something should be excluded
call_out <- function(obj){
  repeat{
    out <- readline(paste0("Do you want to exclude ", as.character(obj), "? [T/F]: "))
    ifelse(toupper(out) %in% c("T", "F"),
           break,
           cat("Invalid input!\n"))
  }
  out <- toupper(out) == "T"
  return(out)
}
