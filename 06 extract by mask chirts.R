

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
root <- '//catalogue/Workspace_cluster_13/GATES/chirts'
fles <- dir_ls(root)
fles <- grep('Africa', fles, value = TRUE)
fles <- as.character(fles)
varb <- 'Tmax'
fles <- grep(varb, fles, value = TRUE)
limt <- raster::shapefile('./data/shp/base/countries_target_4.shp')

# Testing
test <- readRDS(file = fles[1])
test <- rasterFromXYZ(xyz = test[,1:3])
test <- raster::crop(test, limt)
test <- raster::mask(test, limt)


# Raster getting CHIRTS ---------------------------------------------------
map(.x = 1:length(fles), .f = function(k){
  
  cat('Start', k, '\n')
  fle <- fles[k]
  tbl <- readRDS(file = fle)
  rsl <- map(.x = 3:ncol(tbl), .f = function(j){
    cat(j, '\n')
    tbl[,c(1, 2, j)] %>% 
      raster::rasterFromXYZ() %>% 
      raster::crop(., limt) %>% 
      raster::mask(., limt) %>% 
      rasterToPoints() %>% 
      as_tibble()
  })
  rsl <- purrr::reduce(rsl, inner_join, by = c('x', 'y'))
  out <- './data/rds/climate/chirts'
  saveRDS(object = rsl, file = glue('{out}/{basename(fle)}'))
  cat('Done!\n')
  
})


# Check the results -------------------------------------------------------
dir_ls('./data/rds/climate/chirps', regexp = '.rds') %>% 
  as.character() %>% 
  length()

