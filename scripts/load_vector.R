# load VMB

# path <- readline(prompt = "Zadejte cestu k referencni vektorove Vrstve mapovani biotopu: \n(neco/jako/tohle/vector.shp)\ndata/VMB/VMB_bio_hab.shp\n") # cesta
path <- "data/VMB/VMB_Boletice.shp"
vector <- vect(path)

new_cols <- data.frame(
  BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", vector$BIOTOP_SEZ)),
  HABIT_CODES = as.factor(gsub(" \\(\\d+\\)", "", vector$HABIT_SEZ))
)

vector <- cbind(vector, new_cols)
vector <- vector[, c("SEGMENT_ID", "FSB", "BIOTOP_CODES", "HABIT_CODES", "SHAPE_Area", "DATUM")]

FSBout <- function(){
  repeat{
    vyhazuji <- readline(prompt = "Prejete si vyloucit nejakou FSB z analyzy? [T/F]: ")
    ifelse(toupper(vyhazuji) %in% c("T", "F"),
           break,
           cat("Zadejte prosim pouze T nebo F.\n"))
  }
  vyhazuji <- toupper(vyhazuji) == "T"
  return(vyhazuji)
}

what_to_filter <- NULL #what_to_filter <- "T"
while(FSBout() == TRUE){
    co <- NA
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

rm(path, new_cols, what_to_filter, FSBout, co)
gc()
