
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, raster, sf, showtext, ggspatial, colourpicker, tidyverse, gtools, rnaturalearthdata, rnaturalearth, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
cntr <- st_read('../gpkg/countries_zones.gpkg')
wrld <- ne_countries(scale = 50, returnclass = 'sf')
wrld <- filter(wrld, region_un == 'Africa')
cffe <- st_read('../gpkg/zona_buffer.gpkg')

cntr <- wrld %>% filter(iso_a3 %in% cntr$sov_a3)

# To make the map ---------------------------------------------------------

colourWidget()

font_add_google("Roboto Condensed", "RobotoCondensed")
showtext::showtext_auto()

gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.3) + 
  geom_sf(data = cntr, fill = NA, col = '#B03333', lwd = 0.6) + 
  geom_sf(data = cffe, fill = NA, col = '#8F8B11') +
  geom_sf_text(data = cntr, aes(label = name), family = 'RobotoCondensed', size = 9) +
  geom_sf_text(data = wrld %>% filter(!iso_a3 %in% cntr$sov_a3), aes(label = name), col = 'grey40', size = 7, family = 'RobotoCondensed') +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) +
  ggtitle(label = '   Study zone (West africa - cocoa zones)') +
  theme_void() + 
  theme(plot.title = element_text(family = 'RobotoCondensed', size = 58)) +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 3.7, text_family = "RobotoCondensed")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_family = 'RobotoCondensed', text_size = 20))

gmap
ggsave(plot = gmap, filename = '../png/maps/base/mapa_ubicacion.png', units = 'in', width = 9, height = 4.8, dpi = 300)


