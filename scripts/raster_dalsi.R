# raster dalsi

source("scripts/knihovnik.R", echo = F)
source("scripts/load_vector.R", echo = F)
vector
# vyfiltrovat moz a -1
vyloucit <- c("moz.", "-")
vector <- vector[!vector$FSB %in% vyloucit,]
rm(vyloucit)
unique(vector$FSB)

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

vector$gt22 <- ifelse(vector$BIOTOP_CODES %in% GT22, 1, NA)
vector$gt23 <- ifelse(vector$BIOTOP_CODES %in% GT23, 1, NA)
vector$gt25 <- ifelse(vector$BIOTOP_CODES %in% GT25, 1, NA)
vector$gt27 <- ifelse(vector$BIOTOP_CODES %in% GT27, 1, NA)
vector
table(vector$BIOTOP_CODES, vector$gt22)

r <- rast(ext(vector), res = 10, crs = crs(vector))

r22 <- rasterize(vector, r, field = "gt22")
plot(r22)
r23 <- rasterize(vector, r, field = "gt23")
plot(r23)
r25 <- rasterize(vector, r, field = "gt25")
plot(r25)
r27 <- rasterize(vector, r, field = "gt27")
plot(r27)

raster <- c(r22, r23, r25, r27)
plot(raster)
raster

stack <- c(raster_list[[1]], raster)
names(stack) <- c("GT", "biotopes_22", "biotopes_23", "biotopes_25", "biotopes_27")

writeRaster(stack, "data/processing/stack.tif", overwrite = T)

cm_data <- data.frame(values(stack))
table(cm_data$biotopes_22, cm_data$GT, useNA = "always")
table(cm_data$biotopes_23, cm_data$GT, useNA = "always")
table(cm_data$biotopes_25, cm_data$GT, useNA = "always")
table(cm_data$biotopes_27, cm_data$GT, useNA = "always")
table(cm_data)

confusionMatrix(factor(cm_data$GT),
  factor(cm_data$biotopes_22))

cm_data %>%
  filter(GT == 22 & biotopes_22 == 1) %>%
  nrow()
cm_data %>%
  filter(GT == 23 & biotopes_23 == 1) %>%
  nrow()
cm_data %>%
  filter(GT == 25 & biotopes_25 == 1) %>%
  nrow()
cm_data %>%
  filter(GT == 27 & biotopes_27 == 1) %>%
  nrow()
