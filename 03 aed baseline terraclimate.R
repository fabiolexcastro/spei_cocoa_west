
# Fabio Alexander Castro Llanos  ------------------------------------------
# October 18th / 2022
# Terraclimate

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, parallel, fs, qs, raster,future, furrr, tidyverse, gtools, rgeos, stringr, glue, climateR)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
cntr <- terra::vect('../gpkg/countries_zones.gpkg')
zone <- terra::vect('../gpkg/zona_buffer.gpkg')
zone <- terra::project(zone, crs(cntr))

# Get terraclimate --------------------------------------------------------
start <- as.Date(x = '2020-01-01', format = '%Y-%m-%d')
end <- as.Date(x = '2020-12-01', format = '%Y-%m-%d')
param_meta$terraclim

# Precipitation
prec <- getTerraClim(AOI = st_as_sf(cntr), param = 'prcp', startDate = start, endDate = end)
prec <- prec$terraclim_prcp

# Maximum temperature 
tmax <- getTerraClim(AOI = st_as_sf(cntr), par = 'tmax', startDate = start, endDate = end)
tmax <- tmax$terraclim_tmax

# Minimum temperature
tmin <- getTerraClim(AOI = st_as_sf(cntr), par = 'tmin', startDate = start, endDate = end)
tmin <- tmin$terraclim_tmin

# To write these results --------------------------------------------------
dout <- '../tif/baseline/countries/terraclimate'
dir_create(dout)

purrr::map(.x = 1:nlayers(prec), .f = function(i){
  cat(i, '\t')
  raster::writeRaster(x = prec[[i]], filename = glue('{dout}/prec_{names(prec[[i]])}.tif'), overwrite = TRUE)
  raster::writeRaster(x = tmax[[i]], filename = glue('{dout}/tmax_{names(tmax[[i]])}.tif'), overwrite = TRUE)
  raster::writeRaster(x = tmin[[i]], filename = glue('{dout}/tmin_{names(tmin[[i]])}.tif'), overwrite = TRUE)
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
fles <- dir_ls('../tif/baseline/countries/terraclimate', regexp = '.tif$')
fles <- as.character(fles)
fles <- grep('X2020', fles, value = TRUE)
fles <- as.character(fles)

tmax <- grep('tmax', fles, value = T)
tmin <- grep('tmin', fles, value = T)
prec <- grep('prec', fles, value = T)
year <- 1960:2019

purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\t')
  yea <- year[i]
  
  fls <- grep(yea, fles, value = T) 
  ppt <- grep('prec', fls, value = T) %>% terra::rast()
  tmx <- grep('tmax', fls, value = T) %>% terra::rast() 
  tmn <- grep('tmin', fls, value = T) %>% terra::rast() 
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
    etp[[i]][which.lyr(is.na(etp[[i]]))] <- 0
  }
  
  etp <- terra::crop(etp, cntr)
  etp <- terra::mask(etp, cntr)
  
  # To write the raster of potential evapotranspiration
  purrr::map(.x = 1:nlyr(etp), .f = function(j){
    terra::writeRaster(x = etp[[j]], filename = glue('../tif/baseline/countries/terraclimate/etp_{yea}_{j}.tif'), overwrite = TRUE)  
  })
  
})

# To calculate Balance  ------------------------------------------------------
fles <- dir_ls('../tif/baseline/countries/terraclimate', regexp = '.tif$')
fles <- as.character(fles)
# fles <- grep('2020', fles, value = T)

tmax <- grep('tmax', fles, value = T)
tmin <- grep('tmin', fles, value = T)
prec <- grep('prec', fles, value = T)
etps <- grep('etp', fles, value = T)
year <- 1960:2020

tbls <- purrr::map_dfr(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\n')
  yea <- year[i]
  fls <- grep(yea, fles, value = T) 
  ppt <- grep('prec', fls, value = T) %>% mixedsort() %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  tmx <- grep('tmax', fls, value = T) %>% mixedsort() %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  tmn <- grep('tmin', fls, value = T) %>% mixedsort() %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  etp <- grep('etp', fls, value = T) %>% mixedsort() %>% terra::rast()
  tav <- (tmx + tmn) / 2
  
  bal <- ppt - etp
  
  names(bal) <- glue('bal_{yea}_{1:12}')
  names(ppt) <- glue('prec_{yea}_{1:12}')
  names(tmx) <- glue('tmax_{yea}_{1:12}')
  names(tav) <- glue('tavg_{yea}_{1:12}')
  names(tmn) <- glue('tmin_{yea}_{1:12}')
  names(etp) <- glue('etp_{yea}_{1:12}')
  
  stk <- c(ppt, tmx, tav, tmn, etp, bal)
  tbl <- terra::as.data.frame(stk, xy = T)
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  tbl <- gather(tbl, variable, value, -gid, -x, -y)
  tbl <- separate(data = tbl, col = 'variable', into = c('variable', 'year', 'month'), sep = '_')
  tbl <- spread(tbl, variable, value)
  cat('Done!\n')
  return(tbl)
  
})

dout <- '../qs/climate/countries/terraclimate'
dir_create(dout)

tbls %>% filter(year == 2020 & month == '12') %>% dplyr::select(x, y, bal) %>% terra::rast(., type = 'xyz') %>% plot()

qs::qsave(x = tbls, file = glue('{dout}/balance_1960-2020_terraclimate.qs'))

tbls <- qs::qread(glue('{dout}/balance_1960-2020_terraclimate.qs'))

# To calculate SPEI  ------------------------------------------------------
gids <- tbls$gid %>% unique()

# Extract by mask  --------------------------------------------------------
# Cocoa zones -------------------------------------------------------------
dtes <- tbls %>% distinct(year, month)

rslt <- purrr::map(.x = 1:nrow(dtes), .f = function(i){
  
  cat(i, '\n')
  dte <- dtes[i,]
  tbl <- filter(tbls, year == dte$year & month == dte$month) %>% dplyr::select(x, y, bal:tmin)
  rst <- terra::rast(tbl, type = 'xyz')
  rst <- terra::crop(rst, zone)
  rst <- terra::mask(rst, zone)
  tbl <- terra::as.data.frame(rst, xy = TRUE)
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  tbl <- mutate(tbl, year = dte$year, month = dte$month)
  tbl <- dplyr::select(tbl, gid, everything())
  return(tbl)
  
})

rslt <- bind_rows(rslt)

qs::qsave(rslt, '../qs/climate/countries/terraclimate/balance_1960-2020_terraclimate_cocoaZones.qs')

rslt <- qs::qread('../qs/climate/countries/terraclimate/balance_1960-2020_terraclimate_cocoaZones.qs')
gids <- unique(rslt$gid)


# Extract by mask for the cocoa zones -------------------------------------------
fles <- dir_ls('../tif/baseline/countries/terraclimate', regexp = '.tif$') %>% as.character()

purrr::map(.x = 1:length(fles), .f = function(i){
  
  cat(basename(fles[i]), '\n')
  fle <- fles[i]
  rst <- terra::rast(fle)
  rst <- terra::crop(rst, zone)
  rst <- terra::mask(rst, zone)
  out <- glue('../tif/baseline/cocoa_zones/terraclimate/{basename(fle)}')
  terra::writeRaster(x = rst, filename = out, overwrite = TRUE)
  
})

# To calc SPEI / SPI -------------------------------------------------------
no_cores <- detectCores() - 1
plan(cluster, workers = 36, gc = TRUE)
options(future.globals.maxSize = 28000 * 1024^2)

rslt <- purrr::map(.x = 1:length(gids), .f = function(i){
  
  cat(gids[i], '\t')
  
  # Filtering for the gid
  tbl <- filter(rslt, gid == gids[i])
  
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

fnal <- bind_rows(rslt)
qs::qsave(fnal, file = '../qs/climate/countries/terraclimate/speispi_1960-2019_terraclimate_cocoaZones.qs')

# Table to raster ---------------------------------------------------------
purrr::map(.x = 7:nrow(dtes), .f = function(i){
  
  dte <- dtes[i,]
  yea <- dte$year
  mnt <- dte$month
  
  cat(yea, ' ', mnt, '\t')
  fnl <- fnal %>% filter(year == yea, month == mnt)
  fnl <- fnl %>% dplyr::select(x, y, spei_6, spi_6)
  rst <- terra::rast(fnl, type = 'xyz')
  names(rst) <- c(glue('spei_6_{yea}_{mnt}'), glue('spi_6_{yea}_{mnt}'))
  
  out <- '../tif/baseline/cocoa_zones/terraclimate'
  terra::writeRaster(x = rst, filename = glue('{out}/spei6_spi6_{yea}_{mnt}.tif'), overwrite = TRUE)
  cat('Done!\n')
  
})

# Finish ------------------------------------------------------------------

