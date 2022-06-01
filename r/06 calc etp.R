

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, crayon, rnaturalearthdata, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

source('./PET.R')

# Load data ---------------------------------------------------------------
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
zone <- terra::vect('../shp/countries_africa.gpkg')
july <- read_csv('../tbl/julian_days.csv')

# Files
root <- '../tif/dwn/cm6/EC-Earth3-Veg'
fles <- dir_ls(root) %>% as.character()
dtes <- basename(fles) %>% str_sub(., start = 6, end = nchar(.) - 4) %>% str_split(., pattern = '_')
dtes <- tibble(year = sapply(dtes, '[[', 1), month = sapply(dtes, '[[', 2))
dtes <- distinct(dtes)

# Mask to resample
mask <- worldclim_global(var = 'prec', res = 2.5, path = '../tmpr')
mask <- mask[[1]] * 0 + 1
names(mask) <- 'mask'
mask <- terra::crop(mask, zone) %>% terra::mask(., zone)

# Get the altitude 
srtm <- geodata::elevation_global(res = 2.5, path = '../tmpr')
srtm <- terra::crop(srtm, zone) %>% terra::mask(., zone)

# To calculate the ETP ----------------------------------------------------

i <- 1

purrr::map(.x = 1:nrow(dtes), .f = function(i){
  
  cat(green('Start ', dtes[i,1], dtes[i,2]), '\t')
  dte <- dtes[i,]
  yea <- dte$year
  mnt <- dte$month
  
  # Grepping
  fls <- grep(yea, fles, value = T) %>% grep(mnt, ., value = T)
  ppt <- grep('prec', fls, value = T)
  tmn <- grep('tmax', fls, value = T)
  tmx <- grep('tmin', fls, value = T)
  
  # Read as a raster file 
  ppt <- terra::rast(ppt)
  tmx <- terra::rast(tmx)
  tmn <- terra::rast(tmn)
  
  # Resampling
  ppt <- terra::resample(ppt, mask)
  tmx <- terra::resample(tmx, mask)
  tmn <- terra::resample(tmn, mask)
  
  bse <- ppt * 0 + 1
  rst <- c(ppt, tmx, tmn, srtm)
  names(rst) <- c('prec', 'tmax', 'tmin', 'srtm')
  
  # Get the doy 
  mnt
  day <- as.numeric(july[15,as.numeric(mnt)+1])
  
  # Raster to table 
  tbl <- terra::as.data.frame(rst, xy = TRUE)
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, day = day)
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  
  tb2 <- mutate(tbl, rsds = 12)
  tb2 <- mutate(tbl, et0 = ETo(lat = y, doy = day, TN = tmin, z = srtm, TX = tmax, method_ETo = 'Hs'))
  
  
  purrr::map(.x = 1:nrow(tbl), .f = function(i){
    
    tb <- tbl[i,]
    ETo(lat = 13.9, z = 292, TN = 11.1, TX = 27.5, doy = 12, method_ETo = 'Hs')

  })

  
  
  ETo(lat = 13.9, doy = 15, TN = 27.5, z = 292, rsds = 5, TX = 11.1, method_ETo = 'Hs')
  
  
  
})



