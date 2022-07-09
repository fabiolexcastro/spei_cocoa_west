

# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions to use --------------------------------------------------------
calc_etp <- function(yea){
  
  cat(green(yea), '\n')
  
  # Filtering the year files 
  fls <- grep(yea, fles, value = T)
  ppt <- terra::rast(grep('prec', fls, value = T))
  tmx <- terra::rast(grep('tmax', fls, value = T))
  tmn <- terra::rast(grep('tmin', fls, value = T))
  srd <- terra::resample(srad, tmx)
  tav <- (tmx + tmn) / 2
  
  # Change the names
  names(ppt) <- glue('ppt_{1:12}')
  names(tmx) <- glue('tmx_{1:12}')
  names(tmn) <- glue('tmn_{1:12}')
  names(tav) <- glue('tav_{1:12}')
  names(srd) <- glue('srd_{1:12}')
  
  # To calc the ETP
  etp <- 0.0013 * 0.408 * srd * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  
  etp_2 <- purrr::map(.x = 1:12, .f = function(i){
    
    etp_2 <- etp[[i]]
    etp_2 <- raster(etp_2)
    etp_2[is.nan(etp_2[])] <- 0
    etp_2[is.na(etp_2[])] <- 0
    return(etp_2)
    
  })
  
  etp_2 <- raster::stack(etp_2)
  etp_2 <- etp_2 * c(31,29,31,30,31,30,31,31,30,31,30,31)
  etp_2 <- raster::crop(etp_2, as(zone, 'Spatial')) %>% raster::mask(., as(zone, 'Spatial'))
  
  # To write these raste as only one
  out <- '../tif/cm6/zone/MPI-ESM1-2-HR/down'
  raster::writeRaster(x = etp_2, filename = glue('{out}/etp_{yea}.tif'), overwrite = TRUE)
  
}


# List the files ----------------------------------------------------------

fles <- dir_ls('../tif/cm6/zone/MPI-ESM1-2-HR/down', regexp = '.tif$') %>% as.character()
year <- 1995:2014
srad <- terra::rast('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA/workspace/downscaling/v1/tif/wcl/2_5min/srad.tif')
yea <- 1995
zone <- terra::vect('../gpkg/countries_africa.gpkg')

# To calc the ETP  --------------------------------------------------------
map(1995:2014, calc_etp)

