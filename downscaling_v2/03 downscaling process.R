


# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions ---------------------------------------------------------------

# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/climate/raw/tble_allw.qs')

# Baseline climate --------------------------------------------------------
source('03 functions.R')

mask_50k <- tble %>% dplyr::select(x, y, prec) %>% rasterFromXYZ()

# Resampling 5 km to 50 km ------------------------------------------------
prec_50k <- raster::resample(prec, mask_50k)
tmax_50k <- raster::resample(tmax, mask_50k)
tmin_50k <- raster::resample(tmin, mask_50k)

# To make the downscaling -------------------------------------------------
tble
dtes <- unique(tble$date)

purrr::map(.x = 1:length(dtes), .f = function(i){
  
  mnt <- dtes[i] %>% str_sub(., start = 7, end = 8) %>% as.numeric()
  tbl <- tble %>% filter(date == dtes[i])
  
  # Substing baseline data 
  ppt_50k <- prec_50k[[mnt]]
  ppt_50k[which(ppt_50k[] < 0)] <- 0
  tmx_50k <- tmax_50k[[mnt]]
  tmn_50k <- tmin_50k[[mnt]]
  
  # Table to raster 
  ppt <- rasterFromXYZ(tbl[,c('x','y', 'prec')]) %>% raster::crop(., as(zone, 'Spatial')) %>% raster::mask(., as(zone, 'Spatial'))
  tmx <- rasterFromXYZ(tbl[,c('x','y', 'tmax')]) %>% raster::crop(., as(zone, 'Spatial')) %>% raster::mask(., as(zone, 'Spatial'))
  tmn <- rasterFromXYZ(tbl[,c('x','y', 'tmin')]) %>% raster::crop(., as(zone, 'Spatial')) %>% raster::mask(., as(zone, 'Spatial'))
    
  # To calc the difference
  ppt_dfr <- ppt - ppt_50k
  tmx_dfr <- tmx - tmx_50k
  tmn_dfr <- tmn - tmn_50k
  
  ppt_dfr <- rasterToPoints(ppt_dfr)
  tmx_dfr <- rasterToPoints(tmx_dfr)
  tmn_dfr <- rasterToPoints(tmn_dfr)
  
  # To make the downscaling 
  lst <- list(ppt_dfr, tmx_dfr, tmn_dfr)
  
  rsl <- purrr::map(.x = 1:length(lst), .f = function(j){
    
    tps <- fields::Tps(x = lst[[j]][,1:2], Y = lst[[j]][,3])
    trp <- raster::interpolate(mask_5km, tps)
    trp <- raster::crop(trp, as(zone, 'Spatial'))
    trp <- raster::mask(trp, as(zone, 'Spatial'))
    
  })
  
  # To add the differente 
  ppt_rsl <- prec[[mnt]] + rsl[[1]]
  ppt_rsl[which(ppt_rsl[] < 0)] <- 0
  tmx_rsl <- tmax[[mnt]] + rsl[[2]]
  tmn_rsl <- tmin[[mnt]] + rsl[[3]]

  # as.data.frame(cbind(rasterToPoints(tmx_rsl)[,1:3], rasterToPoints(tmn_rsl)[,3])) %>% mutate(dfr = layer < V4) %>% pull(dfr) %>% table()
  
  # To write these rasters
  out <- '../tif/cm6/zone/MPI-ESM1-2-HR/down'; dir_create(out)
  raster::writeRaster(x = ppt_rsl, filename = glue('{out}/prec_{dtes[i]}.tif'))
  raster::writeRaster(x = tmx_rsl, filename = glue('{out}/tmax_{dtes[i]}.tif'))
  raster::writeRaster(x = tmn_rsl, filename = glue('{out}/tmin_{dtes[i]}.tif'))
  
  
})
