


# Fabio Alexander Castro Llanos  ------------------------------------------
# October 18th / 2022
# Terraclimate

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, parallel, fs, qs, raster,future, furrr, tidyverse, gtools, rgeos, stringr, glue, climateR)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------

# Cocoa zones 
zone <- st_read('../gpkg/zona_buffer.gpkg')

# Countries
cntr <- st_read('../gpkg/countries_zones.gpkg')
isos <- cntr$iso_a3
zone <- terra::project(vect(zone), crs(vect(cntr)))

path <- '//CATALOGUE/WFP_ClimateRiskPr1/10.New_Results/bias_corrected'
prds <- dir_ls(path) %>% as.character() %>% basename()

prdo <- prds[2]

# Directories and GCMs
dirs <- dir_ls(glue('{path}/{prdo}/{isos}')) %>% grep('ssp585', ., value = T) %>% as.character()
dirs <- map(dirs, dir_ls) %>% flatten() %>% as.character()
gcms <- basename(dirs) %>% unique()

# To get the right dates 
year <- if(prdo == '2030s'){yrs <- c('2021', '2040')} else{yrs <- c('2041', '2060')}
dtes <- seq(as.Date(paste0(parse_number(yrs[1]), '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(parse_number(yrs[2]), '-12-01'), format = '%Y-%m-%d'), by = 'months')
dtes <- str_sub(dtes, start = 1, end = 7)

# Real mask ---------------------------------------------------------------
mask <- terra::rast('../tif/baseline/countries/terraclimate/etp_1960_1.tif')

# Resampling --------------------------------------------------------------

make_extract <- function(gcm){
  
  gcm <- gcms[1]
  fls <- map(grep(gcm, dirs, value = T), dir_ls) %>% flatten() %>% as.character()
  nms <- basename(fls)
  dte <- str_split(nms, pattern = '-') %>% purrr::map(., 4) %>% unlist() %>% str_sub(., 6, nchar(.) - 4)
  yea <- str_sub(dte, 1, 4) %>% unique()
  mnt <- str_sub(dte, 6, 7) %>% unique()
  dts <- expand.grid(yea, mnt) %>% as_tibble() %>% setNames(c('year', 'month')) %>% mutate(year = as.numeric(as.character(year)), month = as.numeric(as.character(month))) %>% arrange(year, month)
  dts <- mutate(dts, month = ifelse(month < 10, paste0('0', month), month))
  colnames(dts) <- c('year_base', 'month_base')
  dates <- cbind(dts, date_full = dtes) %>% separate(data = ., col = 'date_full', sep = '-', into = c('year_ftre', 'month_ftre'))
  
  stk <- purrr::map(.x = 1:nrow(dts), .f = function(i){
    
    cat(i, '\n')
    dt <- dts[i,]
    fl <- grep(glue('v2.0.{dt$year_base}.{dt$month_base}.'), fls, value = T)
    tx <- grep('tasmax', fl, value = T)  
    tn <- grep('tasmin', fl, value = T)
    pr <- grep('pr', fl, value = T)
    
    ls <- purrr::map(.x = 1:length(isos), .f = function(j){
      
      iso <- isos[j]; cat(iso, '\t')
      x <- grep(iso, tx, value = T) %>% terra::rast() %>% mean()
      n <- grep(iso, tn, value = T) %>% terra::rast() %>% mean()
      p <- grep(iso, pr, value = T) %>% terra::rast() %>% sum()
      names(x) <- glue('tmax_{dt$year_base}_{dt$month_base}')
      names(n) <- glue('tmin_{dt$year_base}_{dt$month_base}')
      names(p) <- glue('prec_{dt$year_base}_{dt$month_base}')
      return(list(x, n, p))
      
    })
    
    ls <- purrr::map(.x = 1:3, .f = function(k){mosaic(sprc(purrr::map(.x = 1:length(ls), .f = function(l)ls[[l]][[k]])))})
    tx <- ls[[1]]
    tn <- ls[[2]]
    pr <- ls[[3]]
    st <- c(tx, tn, pr)
    
    st <- terra::resample(st, mask)
    
    # To write the results 
    ou <- glue('../tif/future/ssp585/{gcm}/{prdo}/countries')
    yea_ftr <- dates %>% filter(year_base == dt$year_base & month_base == dt$month_base)
    mnt_ftr <- yea_ftr$month_ftre
    yea_ftr <- yea_ftr$year_ftre
    terra::writeRaster(x = st, filename = glue('{ou}/pr-tx-tn_{yea_ftr}_{mnt_ftr}.tif'), overwrite = TRUE)
    return(st)
    
  })
  
}

# To calc the ETP ---------------------------------------------------------
fles <- dir_ls('../tif/future/ssp585/ACCESS-ESM1-5/2030s/countries') %>% as.character()
srad <- dir_ls('../tif/srad/countries') %>% as.character %>% mixedsort() %>% terra::rast()
year <- 2021:2040
prdo <- '2030s'

purrr::map(.x = 2:length(year), .f = function(i){
  
  cat(year[i], '\t')
  yea <- year[i]
  yea <- glue('_{yea}_')
  
  fls <- grep(yea, fles, value = T) 
  fls <- grep('pr-tx-tn', fls, value = T)
  stk <- terra::rast(fls)
  
  ppt <- stk[[grep('prec', names(stk), value = F)]]
  tmx <- stk[[grep('tmax', names(stk), value = F)]]
  tmn <- stk[[grep('tmin', names(stk), value = F)]]
  srd <- raster::resample(srad, tmx)
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
  
  for(j in 1:12){
    etp[[j]][which.lyr(is.na(etp[[j]]))] <- 0
  }
  
  etp <- terra::crop(etp, vect(cntr))
  etp <- terra::mask(etp, vect(cntr))
  
  out <- glue('../tif/future/ssp585/{gcm}/{prdo}/countries')
  
  # To write the raster of potential evapotranspiration
  purrr::map(.x = 1:nlyr(etp), .f = function(j){
    terra::writeRaster(x = etp[[j]], filename = glue('{out}/etp{yea}{j}.tif'), overwrite = TRUE)  
  })
  
})

# To extract by mask for the cocoa zones ----------------------------------
prdo <- c('2030s', '2050s')
fles <- c(as.character(c(dir_ls('../tif/baseline/countries/terraclimate'))), as.character(dir_ls(glue('../tif/future/ssp585/{gcm}/{prdo}/countries'))))
fles <- grep('.tif$', fles, value = T)

purrr::map(.x = 1:length(fles), .f = function(i){
  
  cat(basename(fles[i]), '\t')
  fle <- fles[i]
  rst <- terra::rast(fle)
  rst <- terra::crop(rst, zone)
  rst <- terra::mask(rst, zone)
  out <- glue('../tif/future/ssp585/{gcm}/{prdo}/cocoa_zones/{basename(fle)}')
  terra::writeRaster(x = rst, filename = out, overwrite = TRUE)
  cat('Done!\n')
  
})



