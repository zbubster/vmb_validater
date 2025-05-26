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


source("scripts/vypocet_plochy.R", echo = F)
source("scripts/rp_graphs.R", echo = F)
#vector_updated <- area_of_raster_value(raster_list, vector)
system.time({vector_updated <- area_of_raster_value(raster_list, vector)})

vector
vector[vector$SEGMENT_ID == 4020238]
vector_updated
vector_updated[vector_updated$SEGMENT_ID == 4020238]
vector_updated[vector_updated$SEGMENT_ID == 5580027]
vector_updated[vector_updated$SEGMENT_ID == 5580043]

writeVector(vector_updated, "data/processing/vector.gpkg", filetype = "GPKG")

