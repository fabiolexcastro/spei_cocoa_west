

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

# Load data ---------------------------------------------------------------
root <- './data/raster/cmip6/ssp370/2020_2040'
fles <- dir_ls(root, regexp = '.tif$')

# Get the name of each SSP
ssps <- basename(fles) %>% 
  grep('.1.tif', ., value = TRUE) %>% 
  grep('prec', ., value = TRUE) %>% 
  str_split(., pattern = '_') %>% 
  sapply(.,  `[[`, 4) %>% 
  unique()

# Current climate
crnt <- dir_ls('./data/raster/worldclim/v2.0') %>% 
  as.character() %>% 
  mixedsort() %>% 
  grep(paste0(c('prec', 'tmax', 'tmin'), collapse = '|'), ., value = TRUE) 

# Calc change by each SSP -------------------------------------------------
map(1:length(ssps), function(k){
  
  cat('Start: ', ssps[k])
  vrs <- c('prec', 'tmax', 'tmin')
  fls <- ssps[k] %>% 
    grep(., fles, value = TRUE) %>% 
    as.character() %>% 
    grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
    mixedsort()
  
  map(.x = 1:3, .f = function(j){
    
    cat('Start ', vrs[j], '\n')
    ft <- grep(vrs[j], fls, value = TRUE) %>% 
      mixedsort() %>% 
      grep('.12.tif$', ., value = TRUE) %>% 
      str_sub(string = ., start = 1, end = nchar(.) - 7) %>% 
      glue('.{1:12}.tif') %>% 
      raster::stack(.)
    
    names(ft) <- glue('{vrs[j]}_{1:12}')

    cr <- grep(vrs[j], crnt, value = TRUE) %>% 
      mixedsort() %>% 
      raster::stack(.)
    
    cat('To calculate the difference\n')
    df <- ft - cr
    pr <- (df / cr) * 100
    
    cat('To write the raster\n')
    Map('writeRaster', x = unstack(pr),  filename = glue('./data/raster/difference/cmip6/{vrs[j]}_{ssps[k]}_ssp370_2020.2040_{1:12}.tif'), overwrite = TRUE)
    cat('Done!\n')
    
  })
  
  cat('Done!\n')
  
})

# Check the results -------------------------------------------------------
rslt <- dir_ls('./data/raster/difference/cmip6') %>% 
  as.character() %>% 
  mixedsort() %>% 
  grep('2020', ., value = TRUE)
rstr <- grep(gsub('-', '\\.', ssps[1]), rslt, value = TRUE)
rstr <- raster::stack(rstr)



