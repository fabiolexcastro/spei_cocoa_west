

# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr)

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

# Calc etp  ---------------------------------------------------------------
etps <- raster::stack(etps)
prec <- raster::stack(prec)
baln <- etps - prec
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

# Add gids and tidy the table
tble <- mutate(tble, gid = 1:nrow(tble))
tble <- gather(tble, date, value, -x, -y, -gid)
tble <- mutate(tble, year = str_sub(date, 2, 5), month = str_sub(date, 7, nchar(date)))
tble <- mutate(tble, year = as.numeric(year), month = as.numeric(month))
gids <- unique(tble$gid)
base <- tble %>% distinct(gid, x, y, date, year, month)

options(future.globals.maxSize= 8912896000)
plan(cluster, workers = 40, gc = TRUE)
spi <- furrr::future_map(.x = gids, .f = function(k){
  cat('Start ', k, '\n')
  bl <- filter(tble, gid == k)
  bs <- filter(base, gid == k)
  lt <- unique(bl$y)
  bl <- pull(bl, value)
  bl <- ts(bl)
  sp <- spei(bl, 1)
  sp <- as.numeric(sp$fitted)
  rs <- mutate(bs, spei = sp)
  cat('Done!\n')
  return(rs)
})
  
  

