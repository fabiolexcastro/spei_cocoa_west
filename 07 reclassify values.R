
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, qs, showtext, RColorBrewer, gtools, rgeos, stringr, glue, SPEI)
g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------

# Zones
cffe <- terra::vect('../gpkg/zona_buffer.gpkg') %>% terra::project(., '+proj=longlat +datum=WGS84 +no_defs +type=crs')
cntr <- terra::vect('../gpkg/countries_zones.gpkg')

# Final table
tble <- qs::qread('../qs/climate/cocoaZones/spei_spi_ssp585_ACCESS-ESM1-5.qs')
gcme <- 'ACCESS-ESM1-5'

# To make the reclassify --------------------------------------------------
tble <- mutate(tble, spei_6_rcl = ifelse(spei_6 <= -1.5, -1, ifelse(spei_6 >= 1.5, 1, 0)))
tble <- mutate(tble, spi_6_rcl = ifelse(spi_6 <= -1.5, -1, ifelse(spi_6 >= 1.5, 1, 0)))

tble %>% filter(spei_6_rcl == 1)
tble %>% filter(spi_6_rcl == 1)

unique(tble$month)

dtes <- tble %>% distinct(year, month)
lbls <- tibble(class = c('wet', 'normal', 'dry'), value = c(-1, 0, 1))
qs::qsave(x = tble, file = glue('../qs/climate/cocoaZones/spei_spi_ssp585_ACCESS-ESM1-5_rcl.qs'))

# To make the counting ----------------------------------------------------
dtes %>% filter(year %in% c(1961:1965))
dcds <- seq(1961, 2056, 5)
dtes <- dtes %>% filter(year != 1960) %>% mutate(decada = unlist(purrr::map(.x = 1:length(dcds), .f = function(i) rep(dcds[i] - 1, 60))))

# To count ----------------------------------------------------------------
rslt <- purrr::map(.x = 1:length(dcds), .f = function(i){
  
  cat(dcds[i], '\n')
  dcd <- dcds[i]
  dte <- filter(dtes, decada == dcd - 1)
  yrs <- pull(dte, year) %>% unique()
  tbl <- filter(tble, year %in% yrs)
  dts <- distinct(tbl, year, month)
  tbl <- inner_join(tbl, lbls, by = c('spei_6_rcl' = 'value'))
  colnames(tbl)[13] <- 'spei_6_rcl_class'
  tbl <- inner_join(tbl, lbls, by = c('spi_6_rcl' = 'value'))
  colnames(tbl)[14] <- 'spi_6_rcl_class'
  
  spei <- tbl %>% 
    # filter(gid == 1) %>% 
    group_by(gid, x, y, spei_6_rcl_class) %>% 
    count() %>% 
    ungroup() %>% 
    spread(spei_6_rcl_class, n)
  
  spi <- tbl %>% 
    # filter(gid == 1) %>% 
    group_by(gid, x, y, spi_6_rcl_class) %>% 
    count() %>% 
    ungroup() %>% 
    spread(spi_6_rcl_class, n)
  
  colnames(spei)[4:6] <- c('dry_spei', 'normal_spei', 'wet_spei')
  colnames(spi)[4:6] <- c('dry_spi', 'normal_spi', 'wet_spi')
  
  rsl <- inner_join(spei, spi, by = c('gid', 'x', 'y'))
  rsl <- mutate(rsl, decada = dcd - 1)  
  rsl <- dplyr::select(rsl, gid, x, y, decada, dry_spei:wet_spi)
  
  cat('Done!\n')
  return(rsl)
  
})
rslt <- bind_rows(rslt)
rslt[is.na(rslt)] <- 0
qs::qsave(x = rslt, file = glue('../qs/climate/cocoaZones/countEvents_spei_spi_ssp585_ACCESS-ESM1-5.qs'))

