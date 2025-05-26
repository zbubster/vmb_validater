# rasterize
# this function takes vector polygon layer and rasterizes it. output is singleband layer with wanted resloution (res = res(focal classification raster))

rasterize_reference <- function(vec, resolution = 10){
  
  # which collumn should be taken as band value
  cat("Vector layer colnames:\n", names(vec))
  repeat{
    reference <- readline(prompt = "Name of the reference column, which contain 'ground truth' information: ")
    if (reference %in% names(vec)){
      break
    }else{
      cat("Column name not found!\n")
    }
  }
  
  # prepare empty raster
  r <- rast(ext(vec), res = resolution, crs = crs(vec))
  
  # rasterize
  r <- rasterize(vec, r, field = reference)
  return(list(raster = r, reference_collumn_name = reference))
}

result <- rasterize_reference(vector, res(raster_list[[1]]))
rasterized_vector <- result$raster
reference_band <- result$reference_collumn_name

rm(result)
gc()
