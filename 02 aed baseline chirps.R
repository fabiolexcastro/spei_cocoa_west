
# Fabio Alexander Castro Llanos  ------------------------------------------
# October 18th / 2022
# To write the raster files for climate data (daily to monthly)

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, gtools, rgeos, stringr, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
path <- '//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data' 
dirs <- c(paste0(path, '/Chirps'), paste0(path, '/Chirts'))
cntr <- terra::vect('../gpkg/countries_zones.gpkg')

# Precipitation -----------------------------------------------------------
prec <- dir_ls(dirs, regexp = '.tif') %>% as.character()
dtes <- basename(prec) %>% str_split(., pattern = '-') %>% map(., 2) %>% unlist() %>% str_sub(., 6, nchar(.) - 4)
mnth <- dtes %>% str_sub(., 1, 7) %>% unique()

purrr::map(.x = 1:length(mnth), .f = function(i){
  
  cat(dtes[i], '\n')
  dte <- mnth[[i]]
  ppt <- grep(dte, prec, value = T) %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  ppt <- sum(ppt)
  ppt[which.lyr(ppt < 0)] <- NA
  out <- '../tif/baseline/countries'
  yea <- str_sub(dte, 1, 4); mnt <- str_sub(dte, 6, 7)
  terra::writeRaster(x = ppt, filename = glue('{out}/prec_{yea}_{mnt}.tif'), overwrite = TRUE)
  cat('Done!\n')
  
})

# Maximum temperature ----------------------------------------------------
tmax <- dir_ls(paste0(dirs[2], '/Tmax'), regexp = '.tif') %>% as.character()
dtes <- basename(tmax) %>% str_split(., pattern = '-') %>% map(., 2) %>% unlist() %>% str_sub(., 6, nchar(.) - 4)
mnth <- dtes %>% str_sub(., 1, 7) %>% unique()

purrr::map(.x = 1:length(mnth), .f = function(i){
  
  cat(dtes[i], '\t')
  dte <- mnth[[i]]
  tmx <- grep(dte, tmax, value = T) %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  tmx <- mean(tmx)
  
  out <- '../tif/baseline/countries'
  yea <- str_sub(dte, 1, 4); mnt <- str_sub(dte, 6, 7)
  terra::writeRaster(x = tmx, filename = glue('{out}/tmax_{yea}_{mnt}.tif'), overwrite = TRUE)
  cat('Done!\n')
  
})

dir_ls('//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/ERA5/2m_temperature-24_hour_maximum')

# Minimum temperature -----------------------------------------------------
tmin <- dir_ls(paste0(dirs[2], '/Tmin'), regexp = '.tif') %>% as.character()
dtes <- basename(tmin) %>% str_split(., pattern = '-') %>% map(., 2) %>% unlist() %>% str_sub(., 6, nchar(.) - 4)
mnth <- dtes %>% str_sub(., 1, 7) %>% unique()

purrr::map(.x = 1:length(mnth), .f = function(i){
  
  cat(dtes[i], '\n')
  dte <- mnth[[i]]
  tmn <- grep(dte, tmin, value = T) %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  tmn <- mean(tmn)
  
  out <- '../tif/baseline/countries'
  yea <- str_sub(dte, 1, 4); mnt <- str_sub(dte, 6, 7)
  terra::writeRaster(x = tmn, filename = glue('{out}/tmin_{yea}_{mnt}.tif'), overwrite = TRUE)
  cat('Done!\n')
  
})

# Solar radiation  --------------------------------------------------------
srad <- dir_ls('//catalogue/workspace-cluster9/DATA/1.Data/SolarRadiationExtraterrestre/ET_SolRad')
srad <- as.character(srad)
srad <- grep('et_solrad_', srad, value = T)
srad <- mixedsort(srad)
srad <- terra::rast(srad)
srad <- srad * 1
srdt <- terra::crop(srad, cntr)
srdt <- terra::mask(srdt, cntr)

# To write the solar radiation
dout_srad <- '../tif/srad/countries'
terra::writeRaster(srdt, glue('{dout_srad}/srad_{1:12}.tif'), overwrite = TRUE)

# To calculate ETP hargreaves ---------------------------------------------
fles <- dir_ls('../tif/baseline/countries', regexp = '.tif$')
fles <- as.character(fles)

tmax <- grep('tmax', fles, value = T)
tmin <- grep('tmin', fles, value = T)
prec <- grep('prec', fles, value = T)
prec <- grep(paste0(1995:2014, collapse = '|'), prec, value = T)
year <- 1995:2014

purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\t')
  yea <- year[i]
  
  fls <- grep(yea, fles, value = T) 
  ppt <- grep('prec', fls, value = T) %>% terra::rast()
  tmx <- grep('tmax', fls, value = T) %>% terra::rast() # Los archivos estan invertidos, y asi esta bien
  tmn <- grep('tmin', fls, value = T) %>% terra::rast() # Los archivos estan invertidos, y asi esta bien
  srd <- raster::resample(srdt, tmx)
  tav <- (tmx + tmn) / 2
  
  # Change the names
  names(ppt) <- glue('ppt_{1:12}')
  names(tmx) <- glue('tmx_{1:12}')
  names(tmn) <- glue('tmn_{1:12}')
  names(tav) <- glue('tav_{1:12}')
  names(srd) <- glue('srd_{1:12}')
  
  etp <- 0.0013 * 0.408 * srd * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  
  for(i in 1:12){
    etp[[i]][which.lyr(is.na(etp[[1]]))] <- 0
  }
  
  etp <- terra::crop(etp, cntr)
  etp <- terra::mask(etp, cntr)
  
  # To write the raster of potential evapotranspiration
  purrr::map(.x = 1:nlyr(etp), .f = function(j){
    terra::writeRaster(x = etp[[j]], filename = glue('../tif/baseline/countries/etp_{yea}_{j}.tif'), overwrite = TRUE)  
  })
  
})
