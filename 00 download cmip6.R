
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, geodata, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Load data ---------------------------------------------------------------
zone <- raster::shapefile('./data/shp/base/countries_target_4.shp')
ssps <- c("BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", "GFDL-ESM4", "IPSL-CM6A-LR",
          "MIROC-ES2L", "MIROC6", "MRI-ESM2-0")

# Download data -----------------------------------------------------------
map(.x = 1:length(ssps), .f = function(i){
  
  cmip6_world(model = ssps[i], 
              ssp = '370', 
              time = '2021-2040', 
              var = 'tmin', 
              res = 2.5, 
              path = glue('./data/raster/cmip6/ssp370/2020_2040'))  
  
})


