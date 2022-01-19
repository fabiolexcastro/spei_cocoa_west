
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fs, 
               glue, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fles.prec <- dir_ls('../raster/prec') %>% 
  mixedsort() %>%
  as.character()

fles.etps <- dir_ls('../raster/etps') %>% 
  mixedsort() %>% 
  as.character()

# Administrative data -----------------------------------------------------
ghna <- raster::getData(name = 'GADM', country = 'GHA', level = 1)
ashn <- ghna[ghna@data$NAME_1 == 'Ashanti',]

# Read the rasters --------------------------------------------------------
prec <- raster::stack(fles.prec) %>% raster::crop(., ashn) %>% raster::mask(., ashn)
etps <- raster::stack(fles.etps) %>% raster::crop(., ashn) %>% raster::mask(., ashn)

dout <- '../raster'
writeRaster(x = prec, filename = glue('{dout}/prec/ashanti/prec_ashanti.tif'))
writeRaster(x = etps, filename = glue('{dout}/prec/ashanti/etps_ashanti.tif'))

