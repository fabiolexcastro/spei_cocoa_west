
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, qs, classInt, showtext, trend, RColorBrewer, gtools, rgeos, stringr, glue, SPEI)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Functions ---------------------------------------------------------------


# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/climate/cocoaZones/countEvents_spei_spi_ssp585_ACCESS-ESM1-5.qs')
colnames(tble)[4] <- 'lustro'
lstr <- unique(tble$lustro)
gids <- unique(tble$gid)

# Zones
cffe <- st_read('../gpkg/zona_buffer.gpkg') %>% st_transform(., st_crs(4326))
cntr <- st_read('../gpkg/countries_zones.gpkg')

# Font
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext::showtext_auto()


# To make the slope -------------------------------------------------------

rslt <- purrr::map(.x = 1:length(gids), .f = function(i){
  
  cat(gids[i], '\n')
  tbl <- filter(tble, gid == gids[i])
  
  # Wet SPEI / dry SPEI
  wet_spei <- pull(tbl, wet_spei) %>% ts() %>% sens.slope()
  dry_spei <- pull(tbl, dry_spei) %>% ts() %>% sens.slope()
  
  # Wet SPI / dry SPI
  wet_spi <- pull(tbl, wet_spi) %>% ts() %>% sens.slope()
  dry_spi <- pull(tbl, dry_spi) %>% ts() %>% sens.slope()
  
  rsl <- tibble(pv_wetSPEI = wet_spei$p.value, sl_wetSPEI = wet_spei$estimates, 
                pv_drySPEI = dry_spei$p.value, sl_drySPEI = dry_spei$estimates,
                pv_wetSPI = wet_spi$p.value, sl_wetSPI = wet_spi$estimates,
                pv_drySPI = dry_spi$p.value, sl_drySPI = dry_spi$estimates, 
                gid = as.character(gids[i]))
  rsl <- gather(rsl, variable, value, -gid)
  rsl <- separate(rsl, col = 'variable', into = c('type', 'index'), sep = '_', remove = TRUE)
  return(rsl)
  
})

rslt <- bind_rows(rslt)
rslt <- rslt %>% spread(index, value) %>% mutate(gid = as.numeric(gid)) %>% arrange(gid)

qs::qsave(x = rslt, file = '../qs/climate/cocoaZones/spei_spi_slope_ACCESS-ESM1-5.qs')

rslt

rslt <- tble %>% distinct(gid, x, y) %>% full_join(., rslt, by = c('gid'))

# To make the maps --------------------------------------------------------

# SPEI --------------------------------------------------------------------

# Dry
g_speiDry <- ggplot() + 
  geom_tile(data = rslt %>% filter(type == 'sl'), aes(x = x, y = y, fill = drySPEI)) + 
  scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG'))) +
  geom_sf(data = cffe, fill = NA, col = 'grey60', lwd = 0.4) +
  geom_sf(data = cntr, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) + 
  theme_void() +
  labs(fill = 'Slope (dry months per each 5 year)') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(family = 'Roboto Condensed', size = 58, hjust = 0.5),
        strip.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.title = element_text(family = 'Roboto Condensed', size = 52, face = 'bold'))

ggsave(plot = g_speiDry, filename = '../png/maps/slope/slope_SPEIdry_ACCESS-ESM1-5.png', units = 'in', width = 9, height = 7, dpi = 300)

# Wet 
g_speiWet <- ggplot() + 
  geom_tile(data = rslt %>% filter(type == 'sl'), aes(x = x, y = y, fill = wetSPEI)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = cffe, fill = NA, col = 'grey60', lwd = 0.4) +
  geom_sf(data = cntr, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) + 
  theme_void() +
  labs(fill = 'Slope (wet months per each 5 year)') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(family = 'Roboto Condensed', size = 58, hjust = 0.5),
        strip.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.title = element_text(family = 'Roboto Condensed', size = 52, face = 'bold'))

ggsave(plot = g_speiWet, filename = '../png/maps/slope/slope_SPEIwet_ACCESS-ESM1-5.png', units = 'in', width = 9, height = 7, dpi = 300)


# SPI ---------------------------------------------------------------------

# Dry
g_spiDry <- ggplot() + 
  geom_tile(data = rslt %>% filter(type == 'sl'), aes(x = x, y = y, fill = drySPI)) + 
  scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG'))) +
  geom_sf(data = cffe, fill = NA, col = 'grey60', lwd = 0.4) +
  geom_sf(data = cntr, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) + 
  theme_void() +
  labs(fill = 'Slope (dry months per each 5 year)') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(family = 'Roboto Condensed', size = 58, hjust = 0.5),
        strip.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.title = element_text(family = 'Roboto Condensed', size = 52, face = 'bold'))

ggsave(plot = g_spiDry, filename = '../png/maps/slope/slope_SPIdry_ACCESS-ESM1-5.png', units = 'in', width = 9, height = 7, dpi = 300)

# Wet 
g_spiWet <- ggplot() + 
  geom_tile(data = rslt %>% filter(type == 'sl'), aes(x = x, y = y, fill = wetSPI)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = cffe, fill = NA, col = 'grey60', lwd = 0.4) +
  geom_sf(data = cntr, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(cntr)[1:2], ylim = ext(cntr)[3:4]) + 
  theme_void() +
  labs(fill = 'Slope (wet months per each 5 year)') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(family = 'Roboto Condensed', size = 58, hjust = 0.5),
        strip.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.text = element_text(family = 'Roboto Condensed', size = 48),
        legend.title = element_text(family = 'Roboto Condensed', size = 52, face = 'bold'))

ggsave(plot = g_spiWet, filename = '../png/maps/slope/slope_SPIwet_ACCESS-ESM1-5.png', units = 'in', width = 9, height = 7, dpi = 300)

