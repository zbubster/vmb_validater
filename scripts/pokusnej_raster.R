# pokusnej

VMBraster <- rast("data/processing/x.tif") # pripraveno z minule

source("scripts/CRS.R", echo = F)

VMB_levels <- levels(VMBraster)[[1]]
print(VMB_levels)

stacked <- c(raster_list[[1]], VMBraster) # vytvoreni jednoho produktu (raster with 2 bands)
stacked
names(stacked) <- c("GT", "VMB") # pojemnovani bands

data_tab <- as.data.frame(values(stacked))

table(data_tab$VMB, data_tab$GT)

table(df$VMB, df$BIOTOP_CODES, useNA = "always")
table(df$GT, df$BIOTOP_CODES, useNA = "always")

df[df$VMB == 1,]

