
# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions to use --------------------------------------------------------
fles <- dir_ls('../tif/cm6/zone/MPI-ESM1-2-HR/down', regexp = '.tif$') %>% as.character()
zone <- terra::vect('../gpkg/countries_africa.gpkg')

fles.etp <- grep('etp', fles, value = T)

fles.etp

# Raster to table and make maps  --------------------------------------------------------
purrr::map(.x = 1:length(fles.etp), .f = function(i){
  
  cat(fles.etp[i], '\n')
  fle <- fles.etp[i]
  yea <- parse_number(basename(fle))
  rst <- terra::rast(fle)
  names(rst) <- glue('etp_{1:12}')
  tbl <- terra::as.data.frame(rst, xy = T)
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  tbl <- as_tibble(tbl)
  tbl <- gather(tbl, var, value, -x, -y, -gid)
  tbl <- mutate(tbl, month = parse_number(var))
  tbl <- inner_join(tbl, tibble(month = 1:12, month_abb = month.abb))
  tbl <- mutate(tbl, month_abb = factor(month_abb, levels = month.abb))
  
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = value)) + 
    facet_wrap(.~month_abb) + 
    scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG')),
                         limits = c(0, 100)) +
    geom_sf(data = st_as_sf(zone), fill = NA, col = 'grey40', lwd = 0.3) + 
    coord_sf() + 
    ggtitle(label = glue('ETP {yea}')) +
    theme_void() +
    labs(fill = 'ETP (mm)') +
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
          legend.key.width = unit(3, 'line'), 
          legend.key.height = unit(0.5, 'line')) 
  
  ggsave(plot = gmp, filename = glue('../png/maps/down_climate_2/v1_etp_{yea}.png'), 
         units = 'in', width = 9, height = 6, dpi = 300)
  cat('Done!\n')
  
})

# To make the maps as a gif file
var <- 'etp'
img <- dir_ls('../png/maps/down_climate_2', regexp = var) %>% as.character()
img <- map(img, image_read)
jnd <- image_join(img)
anm <- magick::image_animate(jnd, fps = 1)
  
# To write
image_write(image = anm, path = glue('../gif/dwn_{var}.gif'))
