
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fs, 
               glue, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
prec <- raster::stack('../raster/prec/ashanti/prec_ashanti.tif')
etps <- raster::stack('../raster/prec/ashanti/etps_ashanti.tif')

# Administrative data -----------------------------------------------------
ghna <- raster::getData(name = 'GADM', country = 'GHA', level = 1)
ashn <- ghna[ghna@data$NAME_1 == 'Ashanti',]

# Create sequence dates ---------------------------------------------------
dtes <- glue('y{1980:2019}')
dtes <- as.character(dtes)
dtes <- map(1:length(dtes), function(k){
  glue('{dtes[k]}_{1:12}')
})
dtes <- flatten(dtes)
dtes <- unlist(dtes)

# Change the names --------------------------------------------------------
names(prec) <- dtes
names(etps) <- dtes

# Calc the balance --------------------------------------------------------
baln <- prec- etps
  
# Raster to table ---------------------------------------------------------
tble <- rasterToPoints(baln)
tble <- as_tibble(tble)
tble <- mutate(tble, gid = 1:nrow(tble))
tble <- gather(tble, date, value, -x, -y, -gid)
tble <- mutate(tble, year = str_sub(date, 2, 5), month = str_sub(date, 7, nchar(date)))
tble <- mutate(tble, year = as.numeric(year), month = as.numeric(month))
gids <- unique(tble$gid)
base <- tble %>% distinct(gid, x, y, date, year, month)
  
# Dates -------------------------------------------------------------------
dtes <- glue('y{1980:2019}')
dtes <- map(1:length(dtes), function(k){
  glue('{dtes[k]}_{1:12}')
}) %>% 
  flatten() %>% 
  unlist() %>% 
  as.character()
colnames(tble) <- c('x', 'y', dtes)
  
  