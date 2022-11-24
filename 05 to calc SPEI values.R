
# Fabio Alexander Castro Llanos  ------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, gtools, rgeos, stringr, glue, SPEI)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------

# Countries and cocoa zone 
cntr <- '../gpkg/countries_zones.gpkg' %>% terra::vect()
zone <- '../gpkg/zona_buffer.gpkg' %>% terra::vect()
zone <- terra::project(zone, crs(cntr))

# Variables
vars <- c('prec', 'tmax', 'tmin', 'etp')

# Baseline 
bsln <- dir_ls('../tif/baseline/countries/terraclimate/') %>% as.character() %>% grep('.tif', ., value = T)
bsln <- grep(paste0(vars, collapse = '|'), bsln, value = T)

# Future 
sspe <- 'ssp585'
gcm  <- 'ACCESS-ESM1-5'
ftre <- as.character(unlist(map(glue('../tif/future/{sspe}/{gcm}/{c("2030s", "2050s")}/countries'), dir_ls)))

# Join baseline and future
fles <- c(bsln, ftre)
fles <- grep('.tif$', fles, value = T)
year <- 1960:2060

# To create the table -----------------------------------------------------

# Precipitation / ETP 
prec <- grep('/prec', fles, value = T)
etps <- grep('/etp', fles, value = T)

# Baseline
prec_bsln <- grep(paste0(paste0('X', 1960:2020), collapse = '|'), prec, value = T)
prec_bsln <- purrr::map(.x = prec_bsln, .f = terra::rast)
prec_bsln <- do.call('c', prec_bsln)
names(prec_bsln) <- paste0('prec_', str_sub(names(prec_bsln), 7, 10), '_', str_sub(names(prec_bsln), 12, 13))

etps_bsln <- grep(paste0(paste0(paste0('_', 1960:2020), collapse = '|'), '.tif'), etps, value = T) %>% mixedsort()
etps_bsln <- terra::rast(etps_bsln)
names(etps_bsln) <- gsub('.tif', '', basename(terra::sources(etps_bsln)))

# Future
prec_ftre <- grep(paste0(paste0('_', 2021:2060), collapse = '|'), prec, value = T)
names_ftre <- list(); for(i in 1:length(prec_ftre)){names_ftre[[i]] <- paste0(str_sub(basename(prec_ftre[i]), 1, 9), '_', c(paste0("0", 1:9), 10:12))}
names_ftre <- do.call('c', names_ftre)
prec_ftre <- purrr::map(.x = prec_ftre, .f = terra::rast)
prec_ftre <- do.call('c', prec_ftre)
names(prec_ftre) %>% length()
names(prec_ftre) <- names_ftre

# 2021 - 2040
etps_ftre <- grep(paste0(paste0('_', 2021:2040), collapse = '|'), etps, value = T) %>% mixedsort()
etps_ftre <- grep(paste0(paste0('_', 1:12, '.tif$'), collapse = '|'), etps_ftre, value = T)
nmes_etps <- basename(etps_ftre) %>% gsub('.tif$', '', .)
etps_ftre <- map(etps_ftre, terra::rast)
etps_ftre <- do.call('c', etps_ftre)
names(etps_ftre) <- nmes_etps

# 2041 - 2060
etps_ftr2 <- grep(paste0(paste0('_', 2041:2060), collapse = '|'), etps, value = T) %>% mixedsort()
nmes_etps <- basename(etps_ftr2) %>% gsub('.tif$', '', .)
years <- 2041:2060
nmes_etps <- do.call('c', map(.x = 1:20, .f = function(i){paste0(years[i], '_', 1:12)}))
etps_ftr2 <- map(etps_ftr2, rast)
etps_ftr2 <- do.call('c', etps_ftr2)  
names(etps_ftr2) <- paste0('etp_', nmes_etps)

# etps_ftr2 <- terra::resample(etps_ftr2, etps_ftre[[1]])
# etps_ftre <-c(etps_ftre, etps_ftr2)


etps_ftre[[1]] + etps_ftr2[[1]]

mask_etps <- etps_ftre[[1]] + etps_ftre[[241]]
mask_etps <- mask_etps * 0 + 1
mask_etps <- terra::as.polygons(mask_etps)

etps_ftre <- terra::crop(etps_ftre, mask_etps) %>% terra::mask(., mask_etps)

plot(etps_ftre[[241]])

# Join prec and etp (baseline and future)
prec <- c(prec_bsln, prec_ftre)
etps <- c(etps_bsln, etps_ftre)
prec <- terra::resample(prec, etps[[1]])

stck <- c(prec, etps)
tble <- terra::as.data.frame(stck, xy = T) %>% as_tibble %>% mutate(gid = 1:nrow(.))
tble <- gather(tble, variable, value, -gid, -x, -y)
tble <- separate(tble, col = 'variable', into = c('variable', 'year', 'month'), sep = '_')

qs::qsave(tble, glue('../qs/climate/cocoaZones/prec_etp_{sspe}_{gcm}.qs'))

gids <- unique(tble$gid)
tble <- spread(tble, variable, value)
nrow(tble)
drop_na(tble) %>% nrow()

tble %>% filter(gid == 1)

# To calc the SPEI --------------------------------------------------------
rslt <- purrr::map(.x = 1:length(gids), .f = function(i){
  
  cat(gids[i], '\t')
  
  # Filtering for the gid
  tbl <- filter(tble, gid == gids[i])
  
  # To calc the SPEI 
  spei <- SPEI::spei(pull(tbl, bal), scale = 6)$fitted %>% as.numeric()
  spi  <- SPEI::spi(pull(tbl, prec), scale = 6)$fitted %>% as.numeric()
  
  # plot(spei, type = 'l')
  # plot(spi,  type = 'l')
  
  # Add these columns to the table
  tbl <- mutate(tbl, spei_6 = spei, spi_6 = spi)
  # out <- '../qs/climate/countries/terraclimate/spei'
  # qs::qsave(tbl, glue('{out}/spei_{gids[i]}.qs'))
  cat('Done!\n')
  return(tbl)
  
}) 










