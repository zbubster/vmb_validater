# raster dalsi dals

source("scripts/knihovnik.R", echo = F)
source("scripts/load_vector.R", echo = F)
vector

source("scripts/load_raster.R", echo = F)
raster_list

source("scripts/CRS.R", echo = F)

plot(vector[!is.na(vector$BIOTOP_CODES)])
plot(raster_list[[1]], add = T)


GT22 <- as.factor(c("T1.1", "T1.2", "T1.3"))
GT23 <- as.factor(c("T1.4", "T1.5", "T1.7", "T1.9", "T1.10"))
GT25 <- as.factor(c("M5", "M7", "A4.1", "A4.2", "A4.3", "T1.6", "T1.8", "T4.1", "T4.2"))
GT27 <- as.factor(character(0))

str(vector$BIOTOP_CODES)
str(GT27)

vector$gt <- NA
vector$gt[vector$BIOTOP_CODES %in% GT22] <- 22
vector$gt[vector$BIOTOP_CODES %in% GT23] <- 23
vector$gt[vector$BIOTOP_CODES %in% GT25] <- 25
vector$gt[vector$BIOTOP_CODES %in% GT27] <- 27
table(vector$BIOTOP_CODES, vector$gt)

r <- rast(ext(vector), res = 10, crs = crs(vector))

r <- rasterize(vector, r, field = "gt")
plot(r)
r

stack <- c(raster_list[[1]], r)
plot(stack)
names(stack) <- c("EUGW_prediction", "VMB_biotopes")

unique(values(stack$EUGW_prediction))

df <- data.frame(values(stack))
head(df)

confusionMatrix(factor(df$EUGW_prediction), factor(df$VMB_biotopes))

################### AAAAAAAAAAAAAAAAAAAAAAAA bozeee

stack[[2]][1] <- 27

############ prasárna ale co mám dělat

df <- data.frame(values(stack))
head(df)

confusionMatrix(factor(df$EUGW_prediction), factor(df$VMB_biotopes))
