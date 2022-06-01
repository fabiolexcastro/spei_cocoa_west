  
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, future, RColorBrewer, furrr, glue, crayon, qs, SPEI, rnaturalearthdata, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/clima/spei/spei_06_EC-Earth3-Veg.qs')
year <- unique(tble$year)
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
zone <- terra::vect('../shp/countries_africa.gpkg')

# Make maps ---------------------------------------------------------------
purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\n')
  yea <- year[i]
  tbl <- filter(tble, year == yea) %>% 
    dplyr::select(gid, x, y, year, month_abb, spei_6) %>% 
    mutate(month_abb = factor(month_abb, levels = month.abb))
  
  sub <- tbl %>% filter(month_abb == 'Aug') %>% dplyr::select(x, y, spei_6)
  sub <- rasterFromXYZ(sub)
  
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = spei_6)) + 
    facet_wrap(.~month_abb) +
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG'), na.value = 'white') +
    geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey50') + 
    labs(x = 'Longitude', y = 'Latitude', fill = 'SPEI 06') +
    ggtitle(label = yea) +
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.7, 'line'), 
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5))
  
  ggsave(plot = gmp, 
         filename = glue('../png/maps/spei_06/EC-Earth3-Veg/spei_06_{yea}.png'), 
         units = 'in', width = 12, height = 6, dpi = 300)
  
  
})

