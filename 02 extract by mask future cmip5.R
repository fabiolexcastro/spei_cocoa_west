

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
root <- '//alliancedfs.alliance.cgiar.org/data_cluster_2/gcm/cmip5/downscaled/rcp60/global_2_5min'
gcms <- dir_ls(root)
prds <- c('2020_2049', '2040_2069')
limt <- raster::shapefile('./data/shp/base/countries_target_4.shp')
vars <- c('prec', 'tmax', 'tmin', 'tmean')

# Extract by mask  --------------------------------------------------------
map(.x = 1:length(gcms), .f = function(k){
  
  cat(basename(gcms[k]), '\n')
  rst <- glue('{gcms[k]}/r1i1p1/{prds[2]}') %>% 
    dir_ls() %>% 
    grep(paste0(vars, collapse = '|'), ., value = TRUE) %>% 
    mixedsort() %>% 
    as.character() %>% 
    raster::stack() %>% 
    raster::crop(., limt) %>% 
    raster::mask(., limt)
  
  out <- glue('./data/raster/cmip5/{prds[2]}/{basename(gcms[k])}_{names(rst)}.tif')
  Map('writeRaster', x = unstack(rst), filename = out, overwrite = TRUE)
  
})

