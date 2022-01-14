
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Load data ---------------------------------------------------------------

# Future data
ftre <- dir_ls('./data/raster/terraclimate/future') 
ftre <- as.character(ftre)
ftre <- mixedsort(ftre)

tmax <- grep('tmax', ftre, value = TRUE)
tmin <- grep('tmin', ftre, value = TRUE)
prec <- grep('ppt', ftre, value = TRUE)

year <- parse_number(basename(tmax))

# Solar radiation
srad <- '//catalogue/workspace-cluster9/Data/1.Data/SolarRadiationExtraterrestre/ET_SolRad'
srad <- dir_ls(srad)
srad <- mixedsort(srad)
srad <- grep('et_solrad', srad, value = TRUE)
srad <- as.character(srad)
srad <- raster::stack(srad)

# Limit
limt <- raster::shapefile('./data/shp/base/countries_target_4.shp')

# Extract by mask SRAD ----------------------------------------------------
srad <- raster::crop(srad, limt)
srad <- raster::mask(srad, limt)
srad <- raster::resample(srad, raster::stack(ftre[1]), method = 'bilinear')

# Make etp variables ------------------------------------------------------
map(.x = 1:length(year), .f = function(i){
  
  cat('Start: ', i, '\n')
  tmx <- raster::stack(grep(year[i], tmax, value = TRUE))
  tmn <- raster::stack(grep(year[i], tmin, value = TRUE))
  tav <- (tmx + tmn) / 2
  ppt <- raster::stack(grep(year[i], prec, value = TRUE))
  srd <- srad
  
  etp <- 0.0013 * 0.408 * srad * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  etp <- round(etp, 0)
  
  for(j in 1:12){
    etp[[j]][which(is.nan(etp[[j]][]))] <- 0
  }
  
  cat('To write the raster\n')
  writeRaster(x = etp, filename = glue('./data/raster/terraclimate/baseline/TerraClimate_etp_{year[i]}.nc'), overwrite = TRUE)
  cat('Done!\n')
  
})

# Check the results -------------------------------------------------------
etps <- dir_ls('./data/raster/terraclimate/baseline', regexp = 'etp') %>% 
  as.character()
etps <- map(.x = etps, .f = raster::stack)

