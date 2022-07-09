
# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions ---------------------------------------------------------------
source('02 functions.R')

# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/climate/raw/tble_allw.qs')
zone <- st_read('../gpkg/countries_africa.gpkg')

# To add the date ---------------------------------------------------------
tble <- mutate(tble, date = str_sub(date, 2, nchar(date)))
tble <- mutate(tble, date = glue('{date}-01'))
tble <- mutate(tble, date = as.Date(date, format = '%Y-%m-%d'))
tble <- mutate(tble, year = year(date))
tble <- mutate(tble, mnth = month(date))
tble <- inner_join(tble, tibble(mnth = 1:12, month = month.abb))
tble <- mutate(tble, month = factor(month, levels = month.abb))

# To make the maps --------------------------------------------------------
dtes <- unique(tble$date)
yrs  <- unique(tble$year)

purrr::map(.x = 1:length(yrs), .f = function(i){
  
  # To make the filter for the years
  tbl <- filter(tble, year == yrs[i])
  
  # To make the maps
  gtx <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = tmax)) + 
    scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd'), 
                         limits = c(20, 45)) + 
    facet_wrap(.~month) +
    geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
    ggtitle(label = yrs[i]) +
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
    facet_wrap(.~month) +
    geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
    ggtitle(label = yrs[i]) +
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
    facet_wrap(.~month) +
    ggtitle(label = yrs[i]) +
    geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
    coord_sf() + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
          legend.key.width = unit(1.8, 'line'),
          legend.key.height = unit(0.6, 'line'))
  
  
  # To save these maps
  out <- '../png/maps/raw_climate'
  ggsave(plot = gtx, filename = glue('{out}/tmax_{yrs[i]}.png'), units = 'in', width = 9, height = 4, dpi = 300)
  ggsave(plot = gtn, filename = glue('{out}/tmin_{yrs[i]}.png'), units = 'in', width = 9, height = 4, dpi = 300)
  ggsave(plot = gpp, filename = glue('{out}/prec_{yrs[i]}.png'), units = 'in', width = 9, height = 4, dpi = 300)
  cat('Done!\n')
  
})


# To make the gif ---------------------------------------------------------
make_gif('prec')
make_gif('tmax')
make_gif('tmin')


# Try to make the gif -----------------------------------------------------
tble
tble <- tble %>% mutate(anio = year(date))

ggif_pp <- ggplot() + 
  geom_tile(data = tble %>% filter(anio == 1996), aes(x = x, y = y, fill = prec)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) + 
  facet_wrap(.~month) +
  geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.8) +
  # ggtitle(label = anio) +
  geom_text(data = tble, aes(x = 1, y = 1.3, label = anio), label.padding = unit(50, 'pt'), color = 'black') +
  coord_sf() + 
  theme_void() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.key.width = unit(1.8, 'line'),
        legend.key.height = unit(0.6, 'line')) +
  transition_manual(frames = anio, cumulative = FALSE)

gganimate::animate(ggif_pp, width = 8, height = 4, unit = "in", res = 300,)
anim_save(ggif_pp, filename = "../gif/prec_raw.gif")

imgs <- dir_ls('../png/maps/raw_climate', regexp = 'prec') %>% as.character()


save_animation(ggif_pp, '../gif/prec.gif')

animate(a, width=4.155, height=4.5, unit="in", res=300,)
anim_save("figures/climate_spiral.gif")

animate()

animate(a, width=4.155, height=4.5, unit="in", res=300,
        renderer = av_renderer("figures/climate_spiral.mp4")
)





