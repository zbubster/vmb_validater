# cover and relative cover

# IN: raster_list, vector
# OUT: updated vector

area_of_raster_value <- function(rastlist, vctr){
  for(raster_name in names(rastlist)){
    raster <- rastlist[[raster_name]] 
    cat("Processing raster: ", raster_name, "\n") 
    cs <- res(raster)[1]*res(raster)[2] # velikost pixlu
    v <- unique(values(raster)) # unikatni hodnoty rastru
    for(value in v){
      cat("Value: ", value, "\n")
      if (is.na(value)) next
      m <- raster == value # maskovani rastru
      ncell <- terra::zonal(m, vctr, # vypocet poctu pixlu v ramci polygonu
                            fun = "sum",
                            small = T, # pouze pix s centroidem uvnitr polygonu
                            na.rm = T)
      
      ncell[is.na(ncell)] <- 0 # make 0 from NaN
      a <- ncell * cs  # vypocet plochy
      p <- (a / vctr$SHAPE_Area) * 100 # vypocet procenta
      p[p > 100] <- 100  # max 100 %
      
      cn_are <- paste0(raster_name, "_", gsub("\\.", "_", as.character(value)), "_area")
      cn_perc <- paste0(raster_name, "_", gsub("\\.", "_", as.character(value)), "_percent")
      
      names(a) <- cn_are
      names(p) <- cn_perc
      
      vctr[[cn_are]] <- a
      vctr[[cn_perc]] <- p
    }
  }
  return(vctr)
}

vector <- area_of_raster_value(raster_list, vector)