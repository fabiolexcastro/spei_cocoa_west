
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 19th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, MetBrewer, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
make_map <- function(yr){
  
  # yr <- 2000
  
  cat(yr, '\n')
  tble <- filter(spei, year == yr)
  tble <- inner_join(tble, tibble(month = 1:12, month_abb = month.abb), by = 'month')
  tble <- mutate(tble, month_abb = factor(month_abb, levels = month.abb))
  
  cat('Make the map\n')
  gmap <- ggplot() + 
    geom_tile(data = tble, aes(x = x, y = y, fill = spei)) + 
    # scale_fill_gradientn(colors = met.brewer("Pissaro",n = 12, type="continuous")) +
    scale_fill_gradientn(colors = brewer.pal(name = 'BrBG', n = 10)) +
    facet_wrap(.~month_abb) + 
    coord_sf() + 
    theme_void() + 
    labs(x = '', y = '', title = glue('SPEI {yr}'), fill = 'SPEI') +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'), 
          legend.position = 'bottom', 
          legend.key.width = unit(1.5, 'cm'))
  
  ggsave(plot = gmap, 
         filename = glue('../png/spei/ashanti/spei_{yr}.jpg'), 
         units = 'in', 
         width = 7, height = 6, dpi = 300)
  cat('Done!\n')
  
}


# Load data ---------------------------------------------------------------
fles <- dir_ls('../raster')
prec <- raster::stack(grep('prec', fles, value = TRUE))
etps <- raster::stack(grep('etps', fles, value = TRUE))

# Change the names --------------------------------------------------------
dtes <- as.character(glue('y{1980:2019}'))
dtes <- map(1:length(dtes), function(k){
  glue('{dtes[k]}_{1:12}')
})
dtes <- unlist(flatten(dtes))

# Calc the balance --------------------------------------------------------
baln <- prec - etps
writeRaster(baln, filename = '../raster/baln_ashanti.tif', spatial = FALSE)

# Read these result -------------------------------------------------------
baln <- raster::stack('../raster/baln_ashanti.tif')
names(baln) <- dtes

# Select 2000 to 2019 -----------------------------------------------------
baln <- baln[[grep(paste0(2000:2019, collapse = '|'), names(baln))]]

# Raster to table ---------------------------------------------------------
tble <- map(1:nlayers(baln), function(i){
  cat(i, '\n')
  as_tibble(raster::rasterToPoints(baln[[i]], spatial = FALSE))
})
tble <- purrr::reduce(tble, inner_join, by = c('x', 'y'))
tble <- mutate(tble, gid = 1:nrow(tble))
tble <- gather(tble, date, value, -x, -y, -gid)
tble <- mutate(tble, year = str_sub(date, 2, 5), month = str_sub(date, 7, nchar(date)))
tble <- mutate(tble, year = as.numeric(year), month = as.numeric(month))
gids <- unique(tble$gid)
base <- tble %>% distinct(gid, x, y, date, year, month)

plan(cluster, workers = 4, gc = TRUE)
spei <- furrr::future_map(.x = gids, .f = function(k){
  cat('Start ', k, '\n')
  bl <- filter(tble, gid == k)
  bs <- filter(base, gid == k)
  bl <- pull(bl, value)
  bl <- ts(bl)
  sp <- spei(bl, 1)
  sp <- as.numeric(sp$fitted)
  rs <- mutate(bs, spei = sp)
  cat('Done!\n')
  return(rs)
})
future:::ClusterRegistry(action = 'stop')
spei <- bind_rows(spei)
write.csv(spei, '../tble/spei_1.csv', row.names = FALSE)

# To make the maps --------------------------------------------------------
map(.x = 2000:2019, .f = make_map)

