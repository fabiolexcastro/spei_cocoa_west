
# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/climate/down/tble_ghan.qs')
years <- 1995:2014
zone <- st_read('../gpkg/countries_africa.gpkg')
ghan <- filter(zone, sov_a3 == 'GHA')

summary(tble$spei_6)
summary(tble$spi_6)


# To make the map  --------------------------------------------------------
purrr::map(.x = 1:length(years), .f = function(i){
  
  yea <- years[i]
  
  # To make the filter and order the dataframe
  cat(yea, '\n')
  tbl <- filter(tble, year == yea)
  tbl <- dplyr::select(tbl, x, y, month, spei_6, spi_6)
  tbl <- inner_join(tbl, tibble(month = 1:12, month_abb = month.abb), by = 'month')
  tbl <- mutate(tbl, month_abb = factor(month_abb, levels = month.abb))
  
  # To make the map
  gs1 <- ggplot() +
    geom_tile(data = tbl, aes(x = x, y = y, fill = spei_6)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG'), 
                         limits = c(-1.8, 2.3)) +
    facet_wrap(.~month_abb) + 
    geom_sf(data = zone, fill = NA, col = 'grey40') + 
    coord_sf(xlim = ext(ghan)[1:2], ylim = ext(ghan)[3:4]) + 
    labs(fill = 'SPEI 6') +
    ggtitle(label = yea) +
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(2, 'line'), 
          plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
          strip.text.x = element_text(face = 'bold'),
          strip.text.y = element_text(face = 'bold'),
          legend.key.height = unit(0.5, 'line'))
  
  gs2 <- ggplot() +
    geom_tile(data = tbl, aes(x = x, y = y, fill = spi_6)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG'), 
                         limits = c(-5.7, 2.13)) +
    facet_wrap(.~month_abb) + 
    geom_sf(data = zone, fill = NA, col = 'grey40') + 
    coord_sf(xlim = ext(ghan)[1:2], ylim = ext(ghan)[3:4]) + 
    labs(fill = 'SPI 6') +
    ggtitle(label = yea) +
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(2, 'line'), 
          plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
          strip.text.x = element_text(face = 'bold'),
          strip.text.y = element_text(face = 'bold'),
          legend.key.height = unit(0.5, 'line'))
  
  ggsave(plot = gs1, filename = glue('../png/maps/down_climate_2/spei6_{yea}.png'), 
         units = 'in', width = 9, height = 7, dpi = 300)
  
  ggsave(plot = gs2, filename = glue('../png/maps/down_climate_2/spi6_{yea}.png'), 
         units = 'in', width = 9, height = 7, dpi = 300)
  
  
  
})


# To make the maps as a gif file
var <- 'spei'
img <- dir_ls('../png/maps/down_climate_2', regexp = var) %>% as.character()
img <- map(img, image_read)
jnd <- image_join(img)
anm <- magick::image_animate(jnd, fps = 1)

# To write
image_write(image = anm, path = glue('../gif/dwn_{var}.gif'))

# To make the maps as a gif file
var <- 'spi'
img <- dir_ls('../png/maps/down_climate_2', regexp = var) %>% as.character()
img <- map(img, image_read)
jnd <- image_join(img)
anm <- magick::image_animate(jnd, fps = 1)

# To write
image_write(image = anm, path = glue('../gif/dwn_{var}.gif'))






