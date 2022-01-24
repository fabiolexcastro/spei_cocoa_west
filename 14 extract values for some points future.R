

# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 24 - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, RColorBrewer, colorspace, 
               ggthemes, terra)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
get_values <- function(yr){
  
  cat(yr, '!', '\n')
  tb <- filter(spei, year == yr)
  rs <- map(1:12, function(i){tb %>% filter(month == i) %>% dplyr::select(x, y, spei) %>% terra::rast(., type = 'xyz')})
  rs <- terra::rast(rs)
  vl <- terra::extract(rs, trgt[,1:2])
  colnames(vl) <- c('id', month.abb)
  vl <- as_tibble(vl)
  vl <- gather(vl, variable, spei, -id)
  vl <- mutate(vl, year = yr)
  cat('Done!\n')
  return(vl)
  
}


# Load data ---------------------------------------------------------------
cntr <- st_read('//catalogue/workspace-cluster9/CLIMA_LOCA/1.Data/shp/base/all_countries.shp')

# Study zone 
limt <- st_read('./data/shp/base/countries_target_4.shp')
ghna <- st_read('//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_ghana/Felix/Data/Production/Ghana_Admin/GHA_admbndp1_1m_GAUL.shp')

# Points
trgt <- read_csv('./data/tbl/points/trgt_gha.csv')
trgt <- mutate(trgt, gid = 1:nrow(trgt))

# Get the values ----------------------------------------------------------
spei <- readRDS('./data/rds/climate/spei/spei_future_BCC.CSM2.MR_v1.rds')
mdl  <- basename('./data/rds/climate/spei/spei_future_BCC.CSM2.MR_v1.rds') %>% str_split(., '_') 
mdl  <- mdl[[1]][3]
vles <- map(.x = 2020:2050, .f = get_values)
vles <- bind_rows(vles)
vles <- mutate(vles, model = mdl)
write.csv(vles, glue('./data/tbl/points/trgt_gha_future_{mdl}_vles.csv'), row.names = FALSE)








