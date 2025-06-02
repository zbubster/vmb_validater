# VMB
# code specific for Vrstva mapování biotopů České Republiky
# i strongly suggest to run this code and walk through it, if you work with this particular data
# it creates new collumns, filter FSB groups if needed etc.

source("scripts/call_out.R", echo = F)

#---#---#---#---#---#---#---#---#---#---#---#---# 
#           Is there a vector already?          #
#---#---#---#---#---#---#---#---#---#---#---#---#

if (!exists("vector")){
  path <- readline(prompt = "Write path to VMB: \n")
  path <- "data/VMB/VMB_Boletice.shp" ######################################################
  vector <- vect(path)
}

#---#---#---#---#---#---#---#---#---#---#---#---# 
#                Random changes                 #
#---#---#---#---#---#---#---#---#---#---#---#---#

#---#---#---#---#---#---#---#---#---#---#---#---#
# BIOTOP_CODES & HABIT_CODES
new_cols <- data.frame(
  BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", vector$BIOTOP_SEZ)),
  HABIT_CODES = as.factor(gsub(" \\(\\d+\\)", "", vector$HABIT_SEZ))
)
vector <- cbind(vector, new_cols)
vector <- vector[, c("SEGMENT_ID", "FSB", "BIOTOP_CODES", "HABIT_CODES", "SHAPE_Area", "DATUM")]

#---#---#---#---#---#---#---#---#---#---#---#---#
# FSB filtration

what_to_filter <- NULL #what_to_filter <- "T"
co <- NA
while(call_out("any FSB group") == TRUE){
  cat("Ve vrstve zbyva:", unique(vector$FSB[!(vector$FSB %in% what_to_filter)]), "\n")
  co <- toupper(readline(prompt = "Jake skupiny FSB chcete odstranit? \n"))
  if(toupper(co) %in% toupper(vector$FSB)){
    what_to_filter <- c(what_to_filter, co)
    cat("Odstarneno bude:", what_to_filter)
  }else{
    cat("FSB not found!\n")
    next
  }
}
vector <- vector[!toupper(vector$FSB) %in% what_to_filter,]

#---#---#---#---#---#---#---#---#---#---#---#---#
# cleaning

rm(path, new_cols, what_to_filter, co)
gc()