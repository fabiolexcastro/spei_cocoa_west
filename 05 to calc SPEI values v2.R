
# Fabio Alexander Castro Llanos  ------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, gtools, rgeos, stringr, glue, SPEI)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------

# Variables
vars <- c('prec', 'tmax', 'tmin', 'etp')

# Zones
zone <- terra::vect('../gpkg/zona_buffer.gpkg') %>% terra::project(., '+proj=longlat +datum=WGS84 +no_defs +type=crs')
cntr <- terra::vect('../gpkg/countries_zones.gpkg')

# Baseline -----------------------------------------------------------------
bsln <- dir_ls('../tif/baseline/countries/terraclimate/') %>% as.character() %>% grep('.tif$', ., value = T)
bsln <- grep(paste0(vars, collapse = '|'), bsln, value = T) %>% mixedsort()

prbs <- grep('prec', bsln, value = T) %>% terra::rast()
etbs <- grep('etp', bsln, value = T) %>% terra::rast()

names(prbs) <- unlist(purrr::map(.x = 1:61, .f = function(i) paste0('prec_', c(1960:2020)[i], '_', 1:12)))
names(etbs) <- unlist(purrr::map(.x = 1:61, .f = function(i) paste0('etp_', c(1960:2020)[i], '_', 1:12)))

# Future ------------------------------------------------------------------
sspe <- 'ssp585'; gcm  <- 'ACCESS-ESM1-5'
ftre <- as.character(unlist(map(glue('../tif/future/{sspe}/{gcm}/{c("2030s", "2050s")}/countries'), dir_ls)))

ft30 <- grep(paste0(2020:2040, collapse = '|'), ftre, value = TRUE) %>% grep('2030s', ., value = T)
ft50 <- grep(paste0(2040:2060, collapse = '|'), ftre, value = TRUE) %>% grep('2050s', ., value = T)

# 2030s --------------------------
pr30 <- grep('pr', ft30, value = T) %>% purrr::map(.x = ., .f = terra::rast)
pr30 <- do.call('c', purrr::map(.x = 1:length(pr30), .f = function(i){pr30[[i]][[3]]}))
names(pr30) <- unlist(purrr::map(.x = 1:20, .f = function(i) paste0('prec_', c(2021:2040)[i], '_', 1:12)))

et30 <- grep('etp', ft50, value = T) %>% purrr::map(.x = ., .f = terra::rast) %>% do.call('c', .)
names(et30) <- unlist(purrr::map(.x = 1:20, .f = function(i) paste0('etp_', c(2021:2040)[i], '_', 1:12)))

# 2050s --------------------------
pr50 <- grep('pr', ft50, value = T) %>% purrr::map(.x = ., .f = terra::rast)
pr50 <- do.call('c', purrr::map(.x = 1:length(pr50), .f = function(i){pr50[[i]][[3]]}))
names(pr50) <- unlist(purrr::map(.x = 1:20, .f = function(i) paste0('prec_', c(2041:2060)[i], '_', 1:12)))

et50 <- grep('etp', ft50, value = T) %>% purrr::map(.x = ., .f = terra::rast) %>% do.call('c', .)
names(et50) <- unlist(purrr::map(.x = 1:20, .f = function(i) paste0('etp_', c(2041:2060)[i], '_', 1:12)))

# To make the stack  ------------------------------------------------------
prec <- c(prbs, pr30, pr50)


# ETP
etbs <- terra::resample(etbs, et30[[1]], method = 'bilinear')

etbs_c <- terra::crop(etbs, zone, snap = 'in') %>% terra::mask(., zone, touches = TRUE)
et30_c <- terra::crop(et30, zone, snap = 'in') %>% terra::mask(., zone, touches = TRUE)
et50_c <- terra::crop(et50, zone, snap = 'in') %>% terra::mask(., zone, touches = TRUE)

# PREC
prbs <- terra::resample(prbs, pr30[[1]], method = 'bilinear')

prbs_c <- terra::crop(prbs, zone, snap = 'in') %>% terra::mask(., zone, touches = TRUE)
pr30_c <- terra::crop(pr30, zone, snap = 'in') %>% terra::mask(., zone, touches = TRUE)
pr50_c <- terra::crop(pr50, zone, snap = 'in') %>% terra::mask(., zone, touches = TRUE)

c(etbs_c, et30_c)

# Join baseline and future ------------------------------------------------
prbs <- terra::crop(prbs, ext(pr30)) %>% terra::mask(., cntr)

prec <- c(prbs, pr30, pr50)
prec <- terra::crop(prec, zone) %>% terra::mask(., zone)

etps <- c(etbs, et30, et50)
etps <- terra::crop(etps, zone) %>% terra::mask(., zone)

stck <- c(prec, etps)
names(stck) <- gsub('\\.', '_', names(stck))
names(stck) <- gsub('_X', '_', names(stck))

terra::writeRaster(x = stck, filename = '../tif/stack/prec_etps.tif')

# Convert to table --------------------------------------------------------
tble <- terra::as.data.frame(stck, xy = TRUE) %>% as_tibble()
tble <- mutate(tble, gid = 1:nrow(tble))
tble <- gather(tble, var, value, -gid, -x, -y)
tble <- separate(tble, col = 'var', into = c('variable', 'year', 'month'), sep = '_')

# Convert to numeric the month column 
tbl2 <- mutate(tble, month = parse_number(month))
nrow(tbl2)
nrow(drop_na(tbl2))
tbl2 %>% filter(year == 1960 & month == 1) %>% dplyr::select(x, y, value) %>% terra::rast(., type = 'xyz') %>% plot()
qs::qsave(x = tbl2, file = glue('../qs/climate/cocoaZones/prec_etp_ssp585_{gcm}.qs'))

# To calculate the SPEI -----------------------------------------------
gids <- unique(tbl2$gid)

tbl2 <- spread(tbl2, variable, value)

# Parallelization
library(future); library(furrr)
plan(cluster, workers = 20, gc = TRUE)
options(future.globals.maxSize = 20000 * 1024^2)

rslt <- furrr::future_map(.x = 1:length(gids), .f = function(i){
  
  cat(gids[i], '\t')
  
  # Filtering for the gid
  tbl <- filter(tbl2, gid == gids[i])
  tbl <- mutate(tbl, balance = prec - etp)
  
  # To calc the SPEI 
  spei <- SPEI::spei(pull(tbl, balance), scale = 6)$fitted %>% as.numeric()
  spi  <- SPEI::spi(pull(tbl, prec), scale = 6)$fitted %>% as.numeric()
  
  # Add these columns to the table
  tbl <- mutate(tbl, spei_6 = spei, spi_6 = spi)
  cat('Done!\n')
  return(tbl)
  
}) 

rslt <- bind_rows(rslt)
qs::qsave(rslt, glue('../qs/climate/cocoaZones/spei_spi_ssp585_ACCESS-ESM1-5.qs'))



