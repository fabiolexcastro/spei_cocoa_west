
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
fles <- dir_ls('./data/raster/cmip5/2020_2049', regexp = '.tif$') %>% 
  mixedsort()

gcms <- grep('prec_1.tif', fles, value = TRUE) %>% 
  as.character() %>% 
  basename() %>% 
  gsub('_prec_1.tif$', '', .)

vars <- c(glue('prec_{1:12}.tif'), glue('tmax_{1:12}.tif'), glue('tmean_{1:12}.tif'), glue('tmin_{1:12}.tif'))

# Calc ensemble -----------------------------------------------------------
map(.x = 1:length(vars), .f = function(k){
  
  cat(vars[k], '\n')
  rst <- vars[k] %>% 
    grep(., fles, value = TRUE) %>% 
    as.character() %>% 
    raster::stack() %>% 
    raster::mean()
  
  writeRaster(x = rst, filename = glue('./data/raster/cmip5/2020_2049/ensemble_{vars[k]}'))
  cat('Done!\n')
  
})

