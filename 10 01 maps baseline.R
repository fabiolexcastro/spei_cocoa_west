
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, raster, sf, RColorBrewer, showtext, ggspatial, colourpicker, tidyverse, gtools, rnaturalearthdata, rnaturalearth, glue, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
cntr <- terra::vect('../gpkg/countries_zones.gpkg')
wrld <- ne_countries(scale = 50, returnclass = 'sf')
wrld <- filter(wrld, region_un == 'Africa')
cffe <- terra::vect('../gpkg/zona_buffer.gpkg')

# To make the map ---------------------------------------------------------

# Baseline
prec_bsln <- terra::rast('../tif/baseline/countries/terraclimate/prec_X1960.01.tif')
tmax_bsln <- terra::rast('../tif/baseline/countries/terraclimate/tmax_X1960.01.tif')
tmin_bsln <- terra::rast('../tif/baseline/countries/terraclimate/tmin_X1960.01.tif')

bsln <- c(prec_bsln, tmax_bsln, tmin_bsln) %>% terra::crop(., cntr) %>% terra::mask(., cntr)

# Future 
sspe <- 'ACCESS-ESM1-5'
prec_ftre <- terra::rast('../tif/future/ssp585/ACCESS-ESM1-5/2030s/countries/pr-tx-tn_2030_01.tif')[[3]]
tmax_ftre <- terra::rast('../tif/future/ssp585/ACCESS-ESM1-5/2030s/countries/pr-tx-tn_2030_01.tif')[[1]]
tmin_ftre <- terra::rast('../tif/future/ssp585/ACCESS-ESM1-5/2030s/countries/pr-tx-tn_2030_01.tif')[[2]]

ftre <- c(prec_ftre, tmax_ftre, tmin_ftre) %>% terra::crop(., cntr) %>% terra::mask(., cntr)

# Raster to table ---------------------------------------------------------
bsln_tble <- terra::as.data.frame(bsln, xy = TRUE) %>% as_tibble() %>% setNames(c('x', 'y', 'prec', 'tmax', 'tmin'))
ftre_tble <- terra::as.data.frame(ftre, xy = TRUE) %>% as_tibble() %>% setNames(c('x', 'y', 'prec', 'tmax', 'tmin'))
bsln_tble
ftre_tble

# To make the map ---------------------------------------------------------

font_add_google("Roboto Condensed", "RobotoCondensed")
showtext::showtext_auto()

# Baseline
gmap <- ggplot() + 
  geom_tile(data = bsln_tble, aes(x = x, y = y, fill = prec)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.3) + 
  geom_sf(data = st_as_sf(cntr), fill = NA, col = '#B03333', lwd = 0.6) + 
  geom_sf(data = st_as_sf(cffe), fill = NA, col = '#8F8B11') +
  geom_sf_text(data = st_as_sf(cntr), aes(label = sovereignt), family = 'RobotoCondensed', size = 9) +
  # geom_sf_text(data = wrld %>% filter(!iso_a3 %in% cntr$sov_a3), aes(label = name), col = 'grey40', size = 7, family = 'RobotoCondensed') +
  labs(fill = 'Prec (mm)') +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) +
  ggtitle(label = '   Precipitation - 1960 Jan') +
  theme_void() + 
  theme(plot.title = element_text(family = 'RobotoCondensed', size = 58), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'RobotoCondensed', size = 24), 
        legend.title = element_text(family = 'RobotoCondensed', size = 32),
        legend.key.width = unit(3, 'line')) +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "RobotoCondensed")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_family = 'RobotoCondensed', text_size = 20))

summary(bsln_tble)

ggsave(plot = gmap, filename = '../png/maps/base/mapa_prec_1960.png', units = 'in', width = 9, height = 5, dpi = 300)


# Future

gmap <- ggplot() + 
  geom_tile(data = ftre_tble, aes(x = x, y = y, fill = prec)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.3) + 
  geom_sf(data = st_as_sf(cntr), fill = NA, col = '#B03333', lwd = 0.6) + 
  geom_sf(data = st_as_sf(cffe), fill = NA, col = '#8F8B11') +
  geom_sf_text(data = st_as_sf(cntr), aes(label = sovereignt), family = 'RobotoCondensed', size = 9) +
  # geom_sf_text(data = wrld %>% filter(!iso_a3 %in% cntr$sov_a3), aes(label = name), col = 'grey40', size = 7, family = 'RobotoCondensed') +
  labs(fill = 'Prec (mm)') +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) +
  ggtitle(label = '   Precipitation - 2030 Jan') +
  theme_void() + 
  theme(plot.title = element_text(family = 'RobotoCondensed', size = 58), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'RobotoCondensed', size = 24), 
        legend.title = element_text(family = 'RobotoCondensed', size = 32),
        legend.key.width = unit(3, 'line')) +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "RobotoCondensed")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_family = 'RobotoCondensed', text_size = 20))

summary(bsln_tble)

ggsave(plot = gmap, filename = glue('../png/maps/base/mapa_prec_2030_{sspe}.png'), units = 'in', width = 9, height = 5, dpi = 300)

# Potential Evapotranspiration --------------------------------------------

# Baseline
etp_bsln <- terra::rast('../tif/baseline/countries/terraclimate/etp_1960_1.tif') %>% terra::as.data.frame(., xy = T) %>% as_tibble()

gmap <- ggplot() + 
  geom_tile(data = etp_bsln, aes(x = x, y = y, fill = etp_1)) + 
  scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG'))) +
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.3) + 
  geom_sf(data = st_as_sf(cntr), fill = NA, col = '#B03333', lwd = 0.6) + 
  geom_sf(data = st_as_sf(cffe), fill = NA, col = '#8F8B11') +
  geom_sf_text(data = st_as_sf(cntr), aes(label = sovereignt), family = 'RobotoCondensed', size = 9) +
  # geom_sf_text(data = wrld %>% filter(!iso_a3 %in% cntr$sov_a3), aes(label = name), col = 'grey40', size = 7, family = 'RobotoCondensed') +
  labs(fill = 'ETP (mm)') +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) +
  ggtitle(label = '   Potential Evapotranspiration - 1960 Jan') +
  theme_void() + 
  theme(plot.title = element_text(family = 'RobotoCondensed', size = 58), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'RobotoCondensed', size = 24), 
        legend.title = element_text(family = 'RobotoCondensed', size = 32),
        legend.key.width = unit(3, 'line')) +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "RobotoCondensed")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_family = 'RobotoCondensed', text_size = 20))

ggsave(plot = gmap, filename = glue('../png/maps/base/mapa_etp_1960_baseline.png'), units = 'in', width = 9, height = 5, dpi = 300)

# Future
etp_ftre <- terra::rast('../tif/future/ssp585/ACCESS-ESM1-5/2030s/countries/etp_2030_1.tif') %>% terra::as.data.frame(., xy = T) %>% as_tibble()

gmap <- ggplot() + 
  geom_tile(data = etp_ftre, aes(x = x, y = y, fill = etp_1)) + 
  scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG'))) +
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.3) + 
  geom_sf(data = st_as_sf(cntr), fill = NA, col = '#B03333', lwd = 0.6) + 
  geom_sf(data = st_as_sf(cffe), fill = NA, col = '#8F8B11') +
  geom_sf_text(data = st_as_sf(cntr), aes(label = sovereignt), family = 'RobotoCondensed', size = 9) +
  # geom_sf_text(data = wrld %>% filter(!iso_a3 %in% cntr$sov_a3), aes(label = name), col = 'grey40', size = 7, family = 'RobotoCondensed') +
  labs(fill = 'ETP (mm)') +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) +
  ggtitle(label = '   Potential Evapotranspiration - 2030 Jan') +
  theme_void() + 
  theme(plot.title = element_text(family = 'RobotoCondensed', size = 58), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'RobotoCondensed', size = 24), 
        legend.title = element_text(family = 'RobotoCondensed', size = 32),
        legend.key.width = unit(3, 'line')) +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "RobotoCondensed")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_family = 'RobotoCondensed', text_size = 20))

ggsave(plot = gmap, filename = glue('../png/maps/base/mapa_etp_2030_ACCESS-ESM1-5.png'), units = 'in', width = 9, height = 5, dpi = 300)
