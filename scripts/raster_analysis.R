# raster analysis

source("scripts/knihovnik.R", echo = F)

source("scripts/load_vector.R", echo = F)
vector
plot(vector)


source("scripts/load_raster.R", echo = F)
raster_list

source("scripts/CRS.R", echo = F)

plot(vector[!is.na(vector$BIOTOP_CODES)])
plot(raster_list[[1]], add = T)

######## rasterizace

x <- rasterize(vector, raster_list[[1]], field = "BIOTOP_CODES")
plot(x)
plot(x, xlim = c(4620000, 4621000), ylim = c(2865000, 2866000))
writeRaster(x, "data/processing/x.tif", overwrite = T)

########### prace s rasterizovanou VMB a raster_list

VMBraster <- rast("data/processing/x.tif") # pripraveno z minule

VMB_levels <- levels(VMBraster)[[1]]
print(VMB_levels)

raster_list[[1]][is.na(values(raster_list[[1]]))] <- NA
VMBraster[is.na(values(VMBraster))] <- NA

stacked <- c(raster_list[[1]], VMBraster) # vytvoreni jednoho produktu (raster with 2 bands)
names(stacked) <- c("GT", "VMB") # pojemnovani bands

plot(stacked)
data_tab <- as.data.frame(values(stacked))
head(data_tab)

table(data_tab$VMB, data_tab$GT)
str(data_tab)

df <- data_tab %>% left_join(VMB_levels, by = c("VMB" = "value"))

table(df$VMB, df$BIOTOP_CODES, useNA = "always")
table(df$GT, df$BIOTOP_CODES, useNA = "always")

############################ funguje, mam tabulku, ale k confusion matrix vlastne asi potrebuju neco trochu jineho




