
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fles <- dir_ls('../qs/clima')
fles <- as.character(fles)
gcms <- basename(fles) %>% gsub('.qs', '', .) %>% gsub('tb_', '', .)
limt <- vect('../shp/countries_africa.gpkg')

# Baseline ----------------------------------------------------------------
cpp1 <- geodata::worldclim_tile(var = 'prec', res = 0.5, lon = 0, lat = 6, path = '../tmpr')
cpp2 <- geodata::worldclim_tile(var = 'prec', res = 0.5, lon = -3, lat = 6, path = '../tmpr')
cppt <- terra::mosaic(cpp1, cpp2) %>% terra::crop(., limt) %>% terra::mask(., limt)

ctx1 <- geodata::worldclim_tile(var = 'tmax', res = 0.5, lon = 0, lat = 6, path = '../tmpr')
ctx2 <- geodata::worldclim_tile(var = 'tmax', res = 0.5, lon = -3, lat = 6, path = '../tmpr')
ctmx <- terra::mosaic(ctx1, ctx2) %>% terra::crop(., limt) %>% terra::mask(., limt)

ctn1 <- geodata::worldclim_tile(var = 'tmin', res = 0.5, lon = 0, lat = 6, path = '../tmpr')
ctn2 <- geodata::worldclim_tile(var = 'tmin', res = 0.5, lon = -3, lat = 6, path = '../tmpr')
ctmn <- terra::mosaic(ctn1, ctn2) %>% terra::crop(., limt) %>% terra::mask(., limt)

# Resampling 
ms <- terra::rast('../tif/bse/mask_ftr.tif')
cppt_r <- terra::resample(cppt, ms)
ctmx_r <- terra::resample(ctmx, ms)
ctmn_r <- terra::resample(ctmn, ms)

# Processing --------------------------------------------------------------
purrr::map(.x = 1:length(gcms), .f = function(g){
  
  gcm <- gcms[g]
  
  tbl <- grep(gcm, fles, value = T) %>% qs::qread()
  dts <- tbl %>% distinct(yea, mnt)
  
  dts %>% 
    filter(yea == 1998)

  which(dts$yea == 1998)
  
  load('./miss.rds')
  dts <- miss
  dts <- dts %>% dplyr::select(2, 3)
  dts <- distinct(dts)
  
  purrr::map(.x = 1:nrow(dts), .f = function(d){ # 
    
    try(expr = {
    # Tidy the table
    cat('Start ---------------', d, '------------------!\n')
    dt <- dts %>% slice(d) %>% as.vector() %>% unlist() %>% as.character()
    mn <- as.numeric(dt[2])
    tb <- tbl %>% filter(yea == dt[1] & mnt == dt[2]) %>% dplyr::select(lon, lat, pr, tasmin, tasmax)
    ms <- terra::rast(tb[,1:3])
    terra::crs(ms) <- terra::crs(cppt_r)
    pp <- dplyr::select(tb, lon, lat, pr) %>% terra::rast(., type = 'xyz') 
    tx <- dplyr::select(tb, lon, lat, tasmax) %>% terra::rast(., type = 'xyz')
    tn <- dplyr::select(tb, lon, lat, tasmin) %>% terra::rast(., type = 'xyz')
    
    terra::crs(pp) <- terra::crs(cppt_r)
    terra::crs(tx) <- terra::crs(cppt_r)
    terra::crs(tn) <- terra::crs(cppt_r)
    
    # Make the difference 
    dpp <- ((pp - cppt_r[[mn]]) / cppt_r[[mn]]) * 100
    dtx <- tx - ctmx_r[[mn]]
    dtn <- tn - ctmn_r[[mn]]
    
    dpp <- terra::as.data.frame(dpp, xy = TRUE)
    dtx <- terra::as.data.frame(dtx, xy = TRUE)
    dtn <- terra::as.data.frame(dtn, xy = TRUE)
    
    dpp[is.infinite(dpp$pr),'pr'] <- 0
    dtx[is.infinite(dtx$pr),'tasmax'] <- 0
    dtn[is.infinite(dtn$pr),'tasmin'] <- 0
    
    # Mask 30 arc-seconds
    mask <- cppt[[1]] * 0 + 1
    mask <- terra::as.data.frame(mask, xy = TRUE) %>% rasterFromXYZ()
    
    # Make the interpolation 
    lst <- list(dpp, dtn, dtx)
    
    rsl <- purrr::map(.x = 1:length(lst), .f = function(l){
      
      cat(l, '\n')
      tps <- fields::Tps(x = lst[[l]][,1:2], Y = lst[[l]][,3])
      trp <- raster::interpolate(mask, tps)
      rsl <- raster::crop(trp, as(limt, 'Spatial'))
      rsl <- raster::mask(trp, as(limt, 'Spatial'))
      return(rsl)
      
    })
    
    
    # Tidy the raster files
    tst <- rsl
    rsl <- map(rsl, rast)
    rsl <- map(rsl, function(i){terra::crs(i) <- terra::crs(cppt); return(i)})
    plot(rsl[[1]])
    plot(cppt_r[[mn]])
    
    # Make a simple resampling
    rsl <- map(rsl, resample, cppt[[1]])
    
    # Add the values
    ppt <- cppt[[mn]] + ((cppt[[mn]] * rsl[[1]]) / 100)
    ppt[which.lyr(ppt < 0)] <- 0
    tmn <- ctmn[[mn]] + rsl[[2]]
    tmx <- ctmx[[mn]] + rsl[[3]]
    
    # To write these rasters
    out <- glue('../tif/dwn/cm6/{gcm}')
    out <- gsub('dy_', '', out)
    ifelse(!file.exists(out), dir_create(out), print('Ya existe'))
    terra::writeRaster(ppt, filename = glue('{out}/prec_{dt[1]}_{dt[2]}.tif'), overwrite = TRUE)
    terra::writeRaster(tmn, filename = glue('{out}/tmax_{dt[1]}_{dt[2]}.tif'), overwrite = TRUE)
    terra::writeRaster(tmx, filename = glue('{out}/tmin_{dt[1]}_{dt[2]}.tif'), overwrite = TRUE)
    cat('--------------------------------Done---------------------------------!\n')
    
    })
    
  })

  
  
  
})


