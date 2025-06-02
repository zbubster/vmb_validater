# logical_question

logical_question <- function(prompt_text = "Input [T/F]: ") {
  repeat {
    answer <- toupper(readline(paste(prompt_text, "[T/F]: ")))
    
    if (answer %in% c("T", "TRUE")) return(TRUE)
    if (answer %in% c("F", "FALSE")) return(FALSE)
    
    cat("Invalid input. TRUE/FALSE only!\n")
  }
}
