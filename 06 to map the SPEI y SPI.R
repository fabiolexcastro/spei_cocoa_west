
# Fabio Alexander Castro Llanos  ------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, qs, showtext, RColorBrewer, gtools, rgeos, stringr, glue, SPEI)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------

# Zones
cffe <- terra::vect('../gpkg/zona_buffer.gpkg') %>% terra::project(., '+proj=longlat +datum=WGS84 +no_defs +type=crs')
cntr <- terra::vect('../gpkg/countries_zones.gpkg')
cntr <- st_as_sf(cntr)
cffe <- st_as_sf(cffe)

# Final table
tble <- qs::qread('../qs/climate/cocoaZones/spei_spi_ssp585_ACCESS-ESM1-5.qs')
gcme <- 'ACCESS-ESM1-5'

# For make the map --------------------------------------------------------

font_add_google("Roboto Condensed", "RobotoCondensed")
showtext::showtext_auto()

# Tidy simple feature objects
sf_use_s2(FALSE)

# Summary statistics 
summary(tble$spei_6)
?summary

tble[apply(tble, 1, function(x) all(is.finite(x))), ]

# Function ----------------------------------------------------------------
make_map <- function(yr){
  
  # yr <- 1961
  
  cat('Start ', yr, '\n')
  tbl <- filter(tble, year == yr)
  gse <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = spei_6)) + 
    facet_wrap(.~month) +
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG'), limits = c(-3, 3)) + 
    geom_sf(data = cntr, fill = NA, col = '#B03333', lwd = 0.6) + 
    # geom_sf(data = cffe, fill = NA, col = '#8F8B11') +
    coord_sf() + 
    ggtitle(label = glue('  SPEI 6 - {yr}')) +
    labs(fill = 'SPEI 6') +
    theme_void() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(family = 'RobotoCondensed', size = 58),
          strip.text = element_text(family = 'RobotoCondensed', size = 48),
          legend.text = element_text(family = 'RobotoCondensed', size = 48),
          legend.title = element_text(family = 'RobotoCondensed', size = 52, face = 'bold'),
          legend.key.width = unit(3.0, 'line'))
  ggsave(plot = gse, filename = glue('../png/maps/spei/spei6_{yr}.png'), units = 'in', width = 9, height = 4.8, dpi = 300)
  cat('Done!\n')
  
  
}

# Apply the function ------------------------------------------------------
map(1960:2060, make_map)
make_map(yr = 1960)

# To make the gif  --------------------------------------------------------
library(magick)

pngs <- dir_ls('../png/maps/spei', regexp = '.png$')
pngs <- map(pngs, image_read)
join <- image_join(pngs)
anmd <- magick::image_animate(join, fps = 5)

# To write
image_write(image = anmd, path = glue('../gif/spei6_{gcme}.gif'))
