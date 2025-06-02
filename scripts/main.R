# skript main

source("scripts/knihovnik.R", echo = F)

source("scripts/load_raster.R", echo = F)
raster_list

#source("scripts/cisteni_rastru.R", echo = F) # neni nutne
#raster_list

source("scripts/load_vector.R", echo = F)
vector

source("scripts/CRS.R", echo = F)

source("scripts/analysis_raster.R", echo = F)
# ↑↑↑ misto kappa F1 score
# recall and precision
# user and producer accuracy

source("scripts/area_of_cover.R", echo = F)

source("scripts/rp_graphs.R", echo = F)

vector
vector[vector$SEGMENT_ID == 4020238]
vector[vector$SEGMENT_ID == 5580027]
vector[vector$SEGMENT_ID == 5580043]

writeVector(vector_updated, "data/processing/vector.gpkg", filetype = "GPKG")

# exactextract
?exactextractr

