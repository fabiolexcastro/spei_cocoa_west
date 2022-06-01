
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, crayon, rnaturalearthdata, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
zone <- terra::vect('../shp/countries_africa.gpkg')

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
mask <- raster(mask)
writeRaster(mask, '../tif/bse/mask_5km.tif')

# Get the solar radiation
srad <- dir_ls('//catalogue/workspace-cluster9/DATA/1.Data/SolarRadiationExtraterrestre/ET_SolRad')
srad <- as.character(srad)
srad <- grep('et_solrad_', srad, value = T)
srad <- mixedsort(srad)
srad <- terra::rast(srad)
srad <- srad * 1
srdt <- terra::crop(srad, zone)
srdt <- terra::mask(srdt, zone)
srdt <- raster::stack(srdt)

# Resampling 
srdt <- raster::resample(srdt, mask)
Map('writeRaster', x = srdt, filename = glue('../tif/srd/srad_{1:12}.tif'), overwrite = TRUE)

# To calculate ETP --------------------------------------------------------
year <- unique(dtes$year)
purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\t')
  yea <- year[i]
  
  fls <- grep(yea, fles, value = T) 
  ppt <- grep('prec', fls, value = T) %>% stack()
  tmx <- grep('tmin', fls, value = T) %>% stack() # Los archivos estan invertidos, y asi esta bien
  tmn <- grep('tmax', fls, value = T) %>% stack() # Los archivos estan invertidos, y asi esta bien
  srd <- raster::resample(srdt, tmx)
  tav <- (tmx + tmn) / 2

  # Change the names
  names(ppt) <- glue('ppt_{1:12}')
  names(tmx) <- glue('tmx_{1:12}')
  names(tmn) <- glue('tmn_{1:12}')
  names(tav) <- glue('tav_{1:12}')
  names(srd) <- glue('srd_{1:12}')
  
  etp <- 0.0013 * 0.408 * srd * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  purrr::map(.x = 1:nlayers(etp), .f = function(j){
    raster::writeRaster(x = etp[[j]], filename = glue('../tif/dwn/cm6/EC-Earth3-Veg/etp_{yea}_{j}.tif'), overwrite = TRUE)  
  })
  
  
})


