# raster analysis
# this script computes confusion matrices for classification model outputs, which are usualy in raster format

# INPUT:
# OUTPUT:

################################################################################
#### need of crosswalk integration

GT22 <- as.factor(c("T1.1", "T1.2", "T1.3"))
GT23 <- as.factor(c("T1.4", "T1.5", "T1.7", "T1.9", "T1.10"))
GT25 <- as.factor(c("M5", "M7", "A4.1", "A4.2", "A4.3", "T1.6", "T1.8", "T4.1", "T4.2"))
GT27 <- as.factor(character(0))

vector$gt <- NA
vector$gt[vector$BIOTOP_CODES %in% GT22] <- 22
vector$gt[vector$BIOTOP_CODES %in% GT23] <- 23
vector$gt[vector$BIOTOP_CODES %in% GT25] <- 25
vector$gt[vector$BIOTOP_CODES %in% GT27] <- 27
table(vector$BIOTOP_CODES, vector$gt)
################################################################################

source("scripts/rasterization.R")
rasterized_vector
reference_band

stack <- do.call(c, c(raster_list, list(rasterized_vector)))
plot(stack)
stack
## ↓↓↓ omlouvám se
stack[[1]][1] <- 27 
## ↑↑↑ tohle je zvěrstvo

source("scripts/confusion_matrices.R")
