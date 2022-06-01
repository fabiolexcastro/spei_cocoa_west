

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, future, furrr, glue, crayon, qs, SPEI, rnaturalearthdata, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
zone <- terra::vect('../shp/countries_africa.gpkg')

# Files
root <- '../tif/dwn/cm6/EC-Earth3-Veg'
fles <- dir_ls(root, regexp = '.tif$') %>% as.character()

# Get the years
dtes <- basename(fles) %>% .[-grep('etp', ., value = FALSE)] %>% str_sub(., start = 6, end = nchar(.) - 4) %>% str_split(., pattern = '_')
dtes <- sapply(dtes, '[[', 1) %>% unique()

# Mask
mask <- terra::rast('../tif/bse/mask_5km.tif')

# Function ----------------------------------------------------------------
get_table <- function(year){
  
  # year <- dtes[1]
  
  # To make the filter
  cat(year, '\n')
  fle <- grep(year, fles, value = TRUE) %>% 
    grep('baln', ., value = TRUE)
  rst <- terra::rast(fle)
  rst <- terra::resample(rst, mask)
  tbl <- terra::as.data.frame(rst, xy = TRUE)
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  tbl <- gather(tbl, var, value, -gid, -x, -y)
  tbl <- mutate(tbl, month = parse_number(var))
  tbl <- inner_join(tbl, tibble(month = 1:12, month_abb = month.abb), by = 'month')
  gds <- pull(tbl, gid) %>% unique()
  return(tbl)

}

calc_spei <- function(id, sc){
  
  cat(id, '\n')
  tbl <- filter(rslt, gid == id)
  tss <- pull(tbl, value)
  
  spe <- spei(data = tss, scale = sc)
  spe <- spe$fitted
  spe <- as.numeric(spe)
  tbl <- mutate(tbl, spei = spe)
  colnames(tbl)[9] <- glue('spei_{sc}')
  return(tbl)
  
}

# To apply ----------------------------------------------------------------
rslt <- purrr::map(.x = dtes, .f = get_table)

# Add years
rslt <- purrr::map(.x = 1:length(rslt), .f = function(i){rslt[[i]] %>% mutate(year = dtes[i])})
rslt <- bind_rows(rslt)
rslt %>% filter(var == 'baln_1') %>% filter(year == 1995) %>% dplyr::select(x, y, value) %>% rasterFromXYZ() %>% plot()
qs::qsave(rslt, file = '../qs/clima/tble_01_EC-Earth3-Veg.qs')

# To calculate the SPEI ---------------------------------------------------
plan(cluster, workers = 30, gc = TRUE)
options(future.globals.maxSize = 8000 * 1024^2)
fnal <- furrr::future_map(.x = unique(rslt$gid), .f = calc_spei, sc = 6)
bsal <- bind_rows(fnal)
future:::ClusterRegistry('stop')
gc(reset = TRUE)

qs::qsave(bsal, file = '../qs/clima/spei/spei_06_EC-Earth3-Veg.qs')

