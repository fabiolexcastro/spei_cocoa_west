
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
path <- '//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/climate/CMIP6/interim/rotated'
gcms <- dir_ls(path) %>% as.character()

# Limit -------------------------------------------------------------------
limt <- rnaturalearthdata::countries110 %>% st_as_sf()
cntr <- c("Cameroon", "Côte d'Ivoire", "Ghana", "Liberia", "Nigeria", 
          "Sierra Leone", "Togo", 'Benin')
limt <- filter(limt, name %in% cntr)
limt <- vect(limt)
writeVector(limt, '../shp/countries_africa.gpkg', overwrite = T)

# Function ----------------------------------------------------------------
my_crop <- function(gcm){
  
  # gcm <- gcms[2]
  
  # Listing the datasets
  cat(basename(gcm))
  fle <- dir_ls(gcm) %>% as.character() 
  fle <- grep('.tif$', fle, value = TRUE)
  nms <- basename(fle)
  
  # Precipitation
  cat(basename(fle), '\n')
  rs <- grep('pr_day', fle, value = TRUE)
  rs <- purrr::map(.x = 1:length(rs), .f = function(i){
    terra::rast(rs[i]) %>% terra::crop(., limt)
  })
  nm <- grep('pr_day', fle, value = TRUE) %>% 
    basename()
  for(i in 1:length(nm)){
    terra::writeRaster(x = rs[[i]], 
                       filename = glue('../tif/cm6/zone/{basename(gcm)}/{nm[i]}'), 
                       overwrite = TRUE)
  }
  
  # T max
  cat(basename(fle), '\n')
  rs <- grep('tasmax', fle, value = TRUE)
  rs <- purrr::map(.x = 1:length(rs), .f = function(i){
     terra::rast(rs[i]) %>% terra::crop(., limt)
  })
  nm <- grep('tasmax_day', fle, value = TRUE) %>% 
    basename()
  for(i in 1:length(nm)){
    terra::writeRaster(x = rs[[i]], 
                       filename = glue('../tif/cm6/zone/{basename(gcm)}/{nm[i]}'), 
                       overwrite = TRUE)
  }
  
  
  # T min
  rs <- grep('tasmin_day', fle, value = TRUE)
  rs <- purrr::map(.x = 1:length(rs), .f = function(i){
      terra::rast(rs[i]) %>% terra::crop(., limt)
  })
  nm <- grep('tasmin_day', fle, value = TRUE) %>% basename()
  for(i in 1:length(nm)){
    terra::writeRaster(x = rs[[i]], 
                       filename = glue('../tif/cm6/zone/{basename(gcm)}/{nm[i]}'), 
                       overwrite = TRUE)
  }
  

  
}


# Apply the function ------------------------------------------------------
miss <- setdiff(basename(gcms), list.files('../tif/cm6/zone'))
gcms <- grep(miss, gcms, value = TRUE)
purrr::map(gcms[5:length(gcms)], my_crop)
my_crop(gcm = gcms[3])

dir_create(paste0('../tif/cm6/zone/', gcms))

