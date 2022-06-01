

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, crayon, SPEI, rnaturalearthdata, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Function ----------------------------------------------------------------
calc_bdgt <- function(year){
  
  cat(year, '\n')
  
  # List the files and filtering for the main year
  fls <- grep(year, fles, value = TRUE)
  ppt <- grep('prec', fls, value = TRUE)
  etp <- grep('etp_', fls, value = TRUE)
  etp <- mixedsort(etp)
  
  # Read as a raster 
  ppt <- terra::rast(ppt)
  etp <- terra::rast(etp)
  
  # Calc the difference
  bln <- ppt - etp
  names(bln) <- glue('baln_{1:12}')
  
  # To write these rasters files
  terra::writeRaster(x = bln, filename = glue('{root}/baln_{year}.tif'), overwrite = TRUE)
  
}

# Load data ---------------------------------------------------------------
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
zone <- terra::vect('../shp/countries_africa.gpkg')

# Files
root <- '../tif/dwn/cm6/EC-Earth3-Veg'
fles <- dir_ls(root) %>% as.character() %>% grep('.tif$', ., value = T)
dtes <- basename(fles) %>% .[-grep('etp', ., value = FALSE)] %>% str_sub(., start = 6, end = nchar(.) - 4) %>% str_split(., pattern = '_')
dtes <- tibble(year = sapply(dtes, '[[', 1), month = sapply(dtes, '[[', 2))
dtes <- distinct(dtes)
yers <- pull(dtes, 1) %>% unique()

# To apply the function ---------------------------------------------------
purrr::map(.x = yers, .f = calc_bdgt)
