

# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions ---------------------------------------------------------------
get_tble <- function(yea){
  
  cat(green(yea), '\n')
  
  # Filtering the year files 
  fls <- grep(yea, fles, value = T)
  ppt <- terra::rast(grep('prec', fls, value = T))
  tmx <- terra::rast(grep('tmax', fls, value = T))
  tmn <- terra::rast(grep('tmin', fls, value = T))
  tav <- (tmx + tmn) / 2
  etp <- terra::rast(grep('etp', fls, value = T))
  
  # To calc the balance -----------------------------------------------------
  bal <- ppt - etp
  names(bal) <- glue('baln_{1:12}')
  
  # Raster to table  --------------------------------------------------------
  names(ppt) <- glue('prec_{1:12}')
  names(tmx) <- glue('tmax_{1:12}')
  names(tmn) <- glue('tmin_{1:12}')
  names(tav) <- glue('tavg_{1:12}')
  names(etp) <- glue('etps_{1:12}')
  
  tbl <- as_tibble(terra::as.data.frame(c(ppt, tmx, tmn, tav, etp, bal), xy = TRUE))
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  tbl <- gather(tbl, var, value, -x, -y, -gid)
  tbl <- mutate(tbl, month = parse_number(var))
  tbl <- mutate(tbl, var = str_sub(var, 1, 4))
  tbl <- spread(tbl, var, value)
  tbl <- mutate(tbl, year = yea)
  tbl <- dplyr::select(tbl, gid, x, y, year, month, prec, tmax, tavg, tmin, baln, etps)
  return(tbl)
  
}

# To list the results again -----------------------------------------------
fles <- dir_ls('../tif/cm6/zone/MPI-ESM1-2-HR/down', regexp = '.tif$') %>% as.character()

# To get the table --------------------------------------------------------
tbls <- map(1995:2014, get_tble)
tbls <- bind_rows(tbls)
dir_create('../qs/climate/down')
qs::qsave(x = tbls, file = '../qs/climate/down/tble_allt_ghan.qs')

# To get the table only for Ghana -----------------------------------------
ghan <- zone[zone$sov_a3 == 'GHA',]
year <- 1995:2014

tbls_ghan <- purrr::map(.x = 1:length(year), .f = function(i){
  
  yea <- year[i]
  
  dfr <- filter(tbls, year == yea)
  rsl <- purrr::map(.x = 1:12, .f = function(j){
    
    cat(month.abb[j], '\n')
    rst <- dfr %>% filter(month == j) %>% dplyr::select(x, y, prec:etps) %>% terra::rast(., type = 'xyz')
    rst <- terra::crop(rst, ghan) %>% terra::mask(ghan)
    rsl <- terra::as.data.frame(rst, xy = TRUE)
    rsl <- mutate(rsl, gid = 1:nrow(rsl))
    rsl <- as_tibble(rsl)
    rsl <- mutate(rsl, month = j)
    
  }) %>% 
    bind_rows() %>% 
    mutate(year = yea)
  
  cat('Done!\n')
  return(rsl)
  
}) %>% 
  bind_rows()

qs::qsave(x = tbls_ghan, file = '../qs/climate/down/tble_ghan.qs')


# To calc the SPEI / SPI --------------------------------------------------
calc_spei <- function(id){
  
  cat(id, '\n')
  tbl <- filter(tbls_ghan, gid == id)
  ppt <- pull(tbl, prec) %>% ts()
  bal <- pull(tbl, baln) %>% ts()
  
  spei <- SPEI::spei(data = bal, scale = 6)$fitted %>% as.numeric()
  spi  <- SPEI::spi(data = ppt, scale = 6)$fitted %>% as.numeric()
  
  tbl <- mutate(tbl, spei_6 = spei, spi_6 = spi)
  return(tbl)
  
}

gids <- unique(tbls_ghan$gid)
options(future.globals.maxSize = 8000 * 1024^2)
plan(cluster, workers = 30, gc = TRUE)
dfrm <- furrr::future_map(.x = 1:length(gids), .f = function(i){
  calc_spei(id = gids[i])
}) %>% 
  bind_rows()
future:::ClusterRegistry('stop')
qs::qsave(x = dfrm, file = '../qs/climate/down/tble_ghan.qs')

dfrm %>% 
  filter(year == 2012 & month == 1) %>% 
  dplyr::select(x, y, spei_6, spi_6) %>% 
  rasterFromXYZ() %>% 
  plot()





