# graphs of cover and relative proportions

# INPUT: vector updated by vypocet_plochy.R (it should have cover area and percent of coverage of each polygon by each raster from raster list)
# OUTPUT: graphs


mark_found <- function(vctr, threshold = 10){
  # which colls are percent
  percent_cols <- grep("_percent$", names(vctr), value = TRUE)
  # loop through each percent collumn
  for (col in percent_cols) {
    found_col <- gsub("_percent$", "_found", col)
    vctr[[found_col]] <- ifelse(vctr[[col]] > threshold, 1, 0)
  }
  return(vctr)
}


v <- vector_updated
v <- mark_found(v)
names(v)
writeVector(v, "data/processing/found3.gpkg")

