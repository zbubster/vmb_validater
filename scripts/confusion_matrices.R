# confusion matrix extraction
# this code takes all rasters from delivered raster_list, compare it to rasterized ground truth data (former vector) and produce confusion matrices

# INPUT: stack

check_layers <- function(stack){
  crs_l <- lapply(stack, crs)
  if(all(sapply(crs_l, function(x) x == crs_l[[1]]))){
    cat("OK CRS\n")
  }else{
    return("CRS MISMATCH!")
  }
  ext_l <- lapply(stack, ext)
  if(all(sapply(ext_l, function(x) isTRUE(all.equal(x, ext_l[[1]], tolerance = 1e-4))))){
    cat("OK extent\n")
  }else{
    return("Extent MISMATCH!")
  }
  res_l <- lapply(stack, res)
  if(all(sapply(res_l, function(x) isTRUE(all.equal(x, res_l[[1]], tolerance = 1e-6))))){
    cat("OK resolution\n")
  }else{
    return("Resolution MISMATCH!")
  }
}
check_layers(stack)
