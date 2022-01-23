
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, RColorBrewer, colorspace)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Functions to use --------------------------------------------------------
make_map <- function(yr){
  
  # yr <- 1980
  
  cat('Start ', yr, '!', '\n')
  
  tb <- spi %>% filter(year == yr) %>% inner_join(., tibble(month = 1:12, month_abb = month.abb))
  tb <- tb %>% mutate(month_abb = factor(month_abb, levels = month.abb))
  
  gg <- ggplot() + 
    geom_tile(data = tb, aes(x = x, y = y, fill = spei)) + 
    facet_wrap(.~month_abb) +
    scale_fill_gradientn(limits = c(rnge[1], rnge[2]), colors = brewer.pal(n = 10, name = 'BrBG')) +
    geom_sf(data = limt, fill = NA, col = '#D5D5D5', lwd = 0.1) + 
    coord_sf() + 
    ggtitle(label = glue('SPEI {yr} {mdl}')) +
    theme_void() +
    theme(legend.position = 'bottom', 
          legend.key.width = unit(x = 1.5, units = 'cm'), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
    labs(x = '', y = '', fill = 'SPEI')
  
  ggsave(plot = gg, 
         filename = glue('./png/maps/spei/spei_{yr}_scale_01.jpg'), 
         units = 'in', width = 7, height = 4, dpi = 300)
  
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
limt <- st_read('./data/shp/base/countries_target_4.shp')
fles <- dir_ls('./data/raster/terraclimate/future', regexp = '.tif$')
mdel <- basename(fles) %>% 
  str_split(., pattern = '_') %>% 
  sapply(., `[[`, 2) %>% 
  unique()
mdl  <- mdel[1]
fles <- grep(mdl, fles, value = TRUE)
fles <- as.character(fles)
etps <- grep('etp', fles, value = TRUE)
etps <- as.character(etps)
prec <- grep('ppt', fles, value = TRUE)
prec <- as.character(prec)

# Calc balance ------------------------------------------------------------
etps <- raster::stack(etps)
prec <- raster::stack(prec)
baln <- prec - etps
tble <- rasterToPoints(baln, spatial = FALSE)
tble <- as_tibble(tble)
names(tble)

# Create sequence dates
dtes <- glue('y{2020:2050}')
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

tble %>% drop_na()

options(future.globals.maxSize= 8912896000)
plan(cluster, workers = 40, gc = TRUE)
spi <- furrr::future_map(.x = gids, .f = function(k){
  cat('Start ', k, '\n')
  bl <- filter(tble, gid == k)
  bs <- filter(base, gid == k)
  lt <- unique(bl$y)
  bl <- pull(bl, value)
  bl <- ts(bl)
  
  if(any(is.na(bl))){
    
    rs <- NA
    
  } else {
    sp <- spei(bl, 1)
    sp <- as.numeric(sp$fitted)
    rs <- mutate(bs, spei = sp)
  }
  
  cat('Done!\n')
  return(rs)
  
})
future:::ClusterRegistry(action = 'stop')

sp2 <- spi
spi <- spi[-which(is.na(spi) == TRUE)]
spi <- bind_rows(spi)
saveRDS(object = spi, file = './data/rds/climate/spei/spei_v1.rds')

# Process this table ------------------------------------------------------
range(spi$spei)
rnge <- c(-2.477300, 3.660859)
map(.x = 1980:2019, .f = make_map)
