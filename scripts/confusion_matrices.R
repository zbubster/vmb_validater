# confusion matrix extraction
# this code takes all rasters from delivered raster_list, compare it to rasterized ground truth data (former vector) and produce confusion matrices

# INPUT: stack, reference_band (which band of raster stack is the ground truth??)
# OUTPUT: just print of confusion matrices for each model band

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


confmat <- function(stack, truth){
  # is that truly the truth HAHA
  if (!truth %in% names(stack)) {
    return("Ground truth band not found!\n")
  }
  
  # drop spatial information
  df <- data.frame(values(stack))
  
  # for which layers compute CM
  model_layers <- names(stack)[names(stack) != truth]
  
  # iterative computing of confusion matrices
  result <- list() # prepare empty result template
  for (layer in model_layers) {
    pred <- factor(df[[layer]]) # extract model prediction
    ref <- factor(df[[truth]]) # extract true values
    cm <- confusionMatrix(pred, ref) # confusion matix creation
    result[[layer_name]] <- cm 
  }
  return(result)
}

check_layers(stack)
confmat(stack, reference_band)
