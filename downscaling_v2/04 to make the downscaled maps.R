
# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions ---------------------------------------------------------------

# Load data ---------------------------------------------------------------
zone <- st_read('../gpkg/countries_africa.gpkg')

fles <- dir_ls('../tif/cm6/zone/MPI-ESM1-2-HR/down', regexp = '.tif$') %>% as.character()
tmax <- grep('tmax', fles, value = TRUE) 
tmin <- grep('tmin', fles, value = TRUE)
prec <- grep('prec', fles, value = TRUE)

# Get the dates
dtes <- str_split(tmax, pattern = '_') %>% map(2) %>% unlist()
year <- 1995:2014

purrr::map(.x = 1:length(year), .f = function(i){
  
  # To make the filter for the years
  cat(year[i], '\n')
  
  yea <- year[i]
  yea <- gsub('y', '', yea)
  yea <- gsub('.tif$', '', yea)
  
  tmx <- grep(yea, tmax, value = T) %>% terra::rast()
  tmn <- grep(yea, tmin, value = T) %>% terra::rast()
  ppt <- grep(yea, prec, value = T) %>% terra::rast()
  
  stk <- c(tmx, tmn, ppt)
  tbl <- as_tibble(terra::as.data.frame(stk, xy = T))
  tbl <- gather(tbl, var, value, -x, -y)
  tbl <- mutate(tbl, variable = str_sub(var, 1, 4))
  tbl <- mutate(tbl, var = str_sub(var, 6, nchar(var)))
  tbl <- spread(tbl, variable, value)
  tbl <- mutate(tbl, month = str_sub(var, 7, 8))
  tbl <- mutate(tbl, month = as.numeric(month))
  tbl <- inner_join(tbl, tibble(month = 1:12, month_abb = month.abb), by = 'month')
  tbl <- mutate(tbl, month_abb = factor(month_abb, levels = month.abb))
  
  # To make the maps
  gtx <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = tmax)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd'), 
                         limits = c(20, 45)) + 
    facet_wrap(.~month_abb) +
    geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
    ggtitle(label = yea) +
    coord_sf() + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.8, 'line'),
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
          legend.key.height = unit(0.6, 'line'))
  
  gtn <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = tmin)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd'),
                         limits = c(10, 35)) + 
    facet_wrap(.~month_abb) +
    geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
    ggtitle(label = yea) +
    coord_sf() + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.8, 'line'),
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
          legend.key.height = unit(0.6, 'line'))
  
  gpp <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = prec)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG'),
                         limits = c(0, 820)) + 
    facet_wrap(.~month_abb) +
    ggtitle(label = yea) +
    geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
    coord_sf() + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
          legend.key.width = unit(1.8, 'line'),
          legend.key.height = unit(0.6, 'line'))
  
  
  # To save these maps
  out <- '../png/maps/down_climate'
  ggsave(plot = gtx, filename = glue('{out}/tmax_{yea}.png'), units = 'in', width = 9, height = 4, dpi = 300)
  ggsave(plot = gtn, filename = glue('{out}/tmin_{yea}.png'), units = 'in', width = 9, height = 4, dpi = 300)
  ggsave(plot = gpp, filename = glue('{out}/prec_{yea}.png'), units = 'in', width = 9, height = 4, dpi = 300)
  cat('Done!\n')
  
})


# To make the gif ---------------------------------------------------------
make_gif('prec')
make_gif('tmax')
make_gif('tmin')

