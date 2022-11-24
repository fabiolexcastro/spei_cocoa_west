

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, qs, classInt, showtext, RColorBrewer, gtools, rgeos, stringr, glue, SPEI)
g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Functions ---------------------------------------------------------------
makeIntervals <- function(var){
  
  # var <- 'wet_spei'
  
  cat(var, '\t')
  tbl <- dplyr::select(tble, gid, x, y, decada, all_of(var))
  colnames(tbl)[5] <- 'value'
  
  brk <- classInt::classIntervals(var = pull(tbl, value), n = 6, style = 'fisher')$brks
  lbl <- tibble(class = 1:6, breaks = brk[2:7], min = brk[1:6], max = brk[2:7], interval = glue('{min}-{max}'))
  tbl <- mutate(tbl, class = findInterval(x = pull(tbl, value), vec = brk[2:7], all.inside = TRUE))
  tbl <- inner_join(tbl, lbl, by = 'class')
  tbl <- mutate(tbl, interval = factor(interval, levels = lbl$interval))
  return(tbl)
  
}
makeMap <- function(tbl, clr, vrb){
  
  # tbl <- wet_spei
  # clr <- brewer.pal(n = 6, name = 'BrBG')
  # vrb <- 'wet_spei'
  
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = interval)) +
    facet_wrap(.~decada) +
    scale_fill_manual(values = clr) + 
    geom_sf(data = cntr, fill = NA, col = 'grey50', lwd = 0.5) +
    ggtitle(label = glue('Wet events')) +
    labs(x = '', y = '', fill = 'Months (n)') +
    coord_sf() +
    theme_void() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(family = 'RobotoCondensed', size = 58, hjust = 0.5),
          strip.text = element_text(family = 'RobotoCondensed', size = 48),
          legend.text = element_text(family = 'RobotoCondensed', size = 48),
          legend.title = element_text(family = 'RobotoCondensed', size = 52, face = 'bold'))
  
  ggsave(plot = gmp, filename = glue('../png/maps/spei_events/{vrb}.png'), units = 'in', width = 11, height = 6.7, dpi = 300)
  cat('Map done\n')
  
}

# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/climate/cocoaZones/countEvents_spei_spi_ssp585_ACCESS-ESM1-5.qs')
dcds <- unique(tble$decada)

# Zones
cffe <- st_read('../gpkg/zona_buffer.gpkg') %>% terra::project(., '+proj=longlat +datum=WGS84 +no_defs +type=crs')
cntr <- st_read('../gpkg/countries_zones.gpkg')

# Font
font_add_google("Roboto Condensed", "RobotoCondensed")
showtext::showtext_auto()

# SPEI wet  ---------------------------------------------------------------
wet_spei <- makeIntervals(var = 'wet_spei')
makeMap(tbl = wet_spei, clr = brewer.pal(n = 6, name = 'BrBG'), vrb = 'wet_spei')

# SPEI dry ----------------------------------------------------------------
dry_spei <- makeIntervals(var = 'dry_spei')
makeMap(tbl = dry_spei, clr = rev(brewer.pal(n = 6, name = 'BrBG')), vrb = 'dry_spei')

# SPI wet -----------------------------------------------------------------
wet_spi <- makeIntervals(var = 'wet_spi')
makeMap(tbl = wet_spi, clr = brewer.pal(n = 6, name = 'BrBG'), vrb = 'wet_spi')

# SPI dry -----------------------------------------------------------------
dry_spi <- makeIntervals(var = 'dry_spi')
makeMap(tbl = dry_spi, clr = rev(brewer.pal(n = 6, name = 'BrBG')), vrb = 'dry_spi')




