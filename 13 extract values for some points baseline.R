

# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 24 - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, RColorBrewer, colorspace, 
               ggthemes, terra, ggrepel)

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
root <- '//catalogue/workspace-cluster9/COCOA_SOILS/project_v2/data/tbl/run_4'
tble <- glue('{root}/02 points wcl 2 0.csv')
tble <- read_csv(tble)
pnts <- st_as_sf(tble, coords = c('X', 'Y'), crs = st_crs(4326))
cntr <- st_read('//catalogue/workspace-cluster9/CLIMA_LOCA/1.Data/shp/base/all_countries.shp')

# Study zone 
limt <- st_read('./data/shp/base/countries_target_4.shp')
ghna <- filter(limt, ENGLISH == 'Ghana')
ghna <- st_read('//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_ghana/Felix/Data/Production/Ghana_Admin/GHA_admbndp1_1m_GAUL.shp')

# Intersect points and study zone -----------------------------------------
pnts <- st_intersection(pnts, limt)
pnts <- filter(pnts, ENGLISH == 'Ghana')
pnts <- st_coordinates(pnts)
pnts <- as_tibble(pnts)
trgt <- sample_n(tbl = pnts, size = 3, replace = FALSE)
trgt <- mutate(trgt, gid = 1:nrow(trgt))
write.csv(trgt, './data/tbl/points/trgt_gha.csv', row.names = FALSE)

trgt <- read_csv('./data/tbl/points/trgt_gha.csv')
trgt <- mutate(trgt, gid = 1:nrow(trgt))

# Make a simple map  ------------------------------------------------------
gpnt <- ggplot() + 
  geom_sf(data = limt, fill = NA, col = 'grey10') +
  geom_sf(data = ghna, fill = NA, col = 'black') +
  geom_point(data = pnts, aes(x = X, y = Y), col = 'grey50', size = 0.1) + 
  geom_sf(data = ghna, fill = NA, col = 'black') +
  geom_sf_text(data = ghna, aes(label = ADM1_NAME), size = 2, col = 'grey40') +
  geom_point(data = trgt, aes(x = X, y = Y), col = 'red', size = 3) + 
  geom_text_repel(data = trgt, aes(x = X, y = Y, label = gid)) +
  coord_sf(xlim = extent(ghna)[1:2], ylim = extent(ghna)[3:4]) +
  theme_pander() +
  labs(x = 'Longitude', y = 'Latitude')

ggsave(plot = gpnt, filename = './png/maps/base/points_target_ghana_v1.png', units = 'in', width = 6, height = 8, dpi = 300)

# Get the values ----------------------------------------------------------
spei <- readRDS('./data/rds/climate/spei/spei_baseline_v1.rds')
vles <- map(.x = 1980:2019, .f = get_values)
vles <- bind_rows(vles)
write.csv(vles, './data/tbl/points/trgt_gha_baseline_vles.csv', row.names = FALSE)








