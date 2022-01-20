
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, ncdf4)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Load data ---------------------------------------------------------------
fles <- dir_ls('./data/raster/terraclimate/baseline', regexp = '.nc$')
etps <- grep('etp', fles, value = TRUE)
etps <- as.character(etps)
prec <- grep('ppt', fles, value = TRUE)
prec <- as.character(prec)
prec <- prec[-grep('2020', prec)]
shpf <- raster::shapefile('./data/shp/base/countries_target_4.shp')
ghna <- shpf[shpf@data$NAME == 'Ghana',]

# Climate process ---------------------------------------------------------
etps <- raster::stack(etps)
prec <- raster::stack(prec)
prec <- raster::crop(prec, ghna)
prec <- raster::mask(prec, ghna)
etps <- raster::crop(etps, ghna)
etps <- raster::mask(etps, ghna)

# A simple plot
plot(prec[[1]])
plot(etps[[1]])

# Calc the balance --------------------------------------------------------
baln <- prec - etps

# Raster to table ---------------------------------------------------------
tble <- rasterToPoints(baln, spatial = FALSE)
tble <- as_tibble(tble)
names(tble)

# Create sequence dates
dtes <- glue('y{1980:2019}')
dtes <- as.character(dtes)
dtes <- map(1:length(dtes), function(k){
  glue('{dtes[k]}_{1:12}')
})
dtes <- flatten(dtes)
dtes <- unlist(dtes)
colnames(tble) <- c('x', 'y', dtes)

# Write the results -------------------------------------------------------
dir.create('./workspace/calc_spei_v1/raster/prec', recursive = TRUE)
dir.create('./workspace/calc_spei_v1/raster/etps', recursive = TRUE)
Map('writeRaster', x = unstack(prec), filename = glue('./workspace/calc_spei_v1/raster/prec/prec_{dtes}.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(etps), filename = glue('./workspace/calc_spei_v1/raster/etps/etps_{dtes}.tif'), overwrite = TRUE)

# Add gids and tidy the table
tble <- mutate(tble, gid = 1:nrow(tble))
tble <- gather(tble, date, value, -x, -y, -gid)
tble <- mutate(tble, year = str_sub(date, 2, 5), month = str_sub(date, 7, nchar(date)))
tble <- mutate(tble, year = as.numeric(year), month = as.numeric(month))
gids <- unique(tble$gid)
base <- tble %>% distinct(gid, x, y, date, year, month)
saveRDS(object = tble, file = './workspace/calc_spei_v1/rds/tble_baln.rds')




