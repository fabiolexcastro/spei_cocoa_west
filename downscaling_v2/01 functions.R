
# Source ------------------------------------------------------------------
extract_by_mask <- function(dir){

  # Start with the code
  cat(dir, '\n')
  
  # Listing the datasets
  cat(basename(dir))
  fle <- dir_ls(dir) %>% as.character() 
  fle <- grep('.tif$', fle, value = TRUE)
  nms <- basename(fle)
  
  # Precipitation
  cat(basename(fle), '\n')
  rs <- grep('pr_day', fle, value = TRUE)
  rs <- purrr::map(.x = 1:length(rs), .f = function(i){
    terra::rast(rs[i]) %>% terra::crop(., zone)
  })
  nm <- grep('pr_day', fle, value = TRUE) %>% basename()
  
  dir_create(glue('../tif/cm6/zone/{basename(dir)}'))
  
  for(i in 1:length(nm)){
    terra::writeRaster(x = rs[[i]], 
                       filename = glue('../tif/cm6/zone/{basename(dir)}/{nm[i]}'), 
                       overwrite = TRUE)
  }
  
  # T max
  cat(basename(fle), '\n')
  rs <- grep('tasmax', fle, value = TRUE)
  rs <- purrr::map(.x = 1:length(rs), .f = function(i){
    terra::rast(rs[i]) %>% terra::crop(., zone)
  })
  nm <- grep('tasmax_day', fle, value = TRUE) %>% basename()
  for(i in 1:length(nm)){
    terra::writeRaster(x = rs[[i]], 
                       filename = glue('../tif/cm6/zone/{basename(dir)}/{nm[i]}'), 
                       overwrite = TRUE)
  }
  
  # T min
  rs <- grep('tasmin_day', fle, value = TRUE)
  rs <- purrr::map(.x = 1:length(rs), .f = function(i){
    terra::rast(rs[i]) %>% terra::crop(., zone)
  })
  nm <- grep('tasmin_day', fle, value = TRUE) %>% basename()
  for(i in 1:length(nm)){
    terra::writeRaster(x = rs[[i]], 
                       filename = glue('../tif/cm6/zone/{basename(dir)}/{nm[i]}'), 
                       overwrite = TRUE)
  }

}

