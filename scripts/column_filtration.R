#---#---#---#---#---#---#---#---#---#---#---#---# 
#                    Function                   #
#---#---#---#---#---#---#---#---#---#---#---#---#

# column filteration

select_cols <- function(vector) {
  stopifnot(inherits(vector, "SpatVector"))
  
  # print posibilities
  cols <- names(vector)
  cat("Columns in object:\n")
  for (i in seq_along(cols)) {
    cat(i, ":", cols[i], "\n")
  }
  
  # input prompt
  input <- readline(prompt = "Paste names or columns indexes (separeted by comma [,]): ")
  
  # NA handle
  if (input == "") {
    cat("No columns selected, returning original data.\n")
    return(vector)
  }
  
  # if valid
  input <- strsplit(input, ",")[[1]] # split string to identifiers
  input <- trimws(input) # trim white space
  # transfer to names 
  input <- sapply(input, function(x) {
    if (grepl("^[0-9]+$", x)) {
      i <- as.integer(x)
      if (i > 0 && i <= length(cols)) return(cols[i])
      else return(NA)
    } else {
      return(x)
    }
  })
  
  # output creation
  input <- na.omit(input)
  new_cols <- setdiff(cols, input) # difference
  if (length(new_cols) == 0) {
    warning("!!! No columns remaining in data !!!")
  } 
  vector[, new_cols] # updated object
}
