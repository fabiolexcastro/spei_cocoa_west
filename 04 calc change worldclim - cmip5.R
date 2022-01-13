
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 12th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Load --------------------------------------------------------------------
ftre <- dir_ls('./data/raster/cmip5/2020_2049', regexp = 'ensemble') %>% 
  grep('.tif$', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  raster::stack() 

vars <- c('prec', 'tmax', 'tmin', 'tmean')

crnt <- dir_ls('./data/raster/worldclim/v2.0') %>% 
  grep('.tif$', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  grep(paste0(vars, collapse = '|'), ., value = TRUE) %>% 
  raster::stack()

# Calc difference ---------------------------------------------------------
map(.x = 1:nlayers(ftre), .f = function(k){
  
    cat('Start ', k, '\n')
    name <- names(ftre[[k]])
    name <- gsub('ensemble_', '', name)
    name <- str_split(string = name, pattern = '_')
    name <- name[[1]][1]
    name <- str_sub(string = name, start = 1, end = 2)
    
    crn <- crnt[[k]]
    var <- names(crn)
      
    if(name == 'tm'){
      ftr <- ftre[[k]] / 10
    } else{
      ftr <- ftre[[k]]
      print('Precipitation variable')
    }
    
    cat('To calc the difference\n')
    dfr <- ftr - crn
    prc <- (dfr / crn) * 100
    writeRaster(x = prc, filename = glue('./data/raster/difference/cmip5/ensemblePorc_{var}.tif'), overwrite = TRUE)
    cat('Done!\n')
  
})

# Finish ------------------------------------------------------------------




