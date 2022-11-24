
# Fabio Alexander Castro Llanos  ------------------------------------------
# October 18th / 2022
# Terraclimate

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, parallel, fs, qs, raster,future, furrr, tidyverse, gtools, rgeos, stringr, glue, climateR)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)


# Testing -----------------------------------------------------------------
r1 <- terra::rast('../tif/baseline/countries/terraclimate/etp_1960_1.tif')
r2 <- terra::rast('../tif/future/ssp585/ACCESS-ESM1-5/2030s/countries/etp__2021__1.tif')

plot(r1)
plot(r2)

c(r1, r2)
r1
r2

# Load data ---------------------------------------------------------------

# Cocoa zones 
zone <- st_read('../gpkg/zona_buffer.gpkg')

# Countries
cntr <- st_read('../gpkg/countries_zones.gpkg')
isos <- cntr$iso_a3
zone <- terra::project(vect(zone), crs(vect(cntr)))

path <- '//CATALOGUE/WFP_ClimateRiskPr1/10.New_Results/bias_corrected'
prds <- dir_ls(path) %>% as.character() %>% basename()

prdo <- prds[1]

# Directories and GCMs
dirs <- dir_ls(glue('{path}/{prdo}/{isos}')) %>% grep('ssp585', ., value = T) %>% as.character()
dirs <- map(dirs, dir_ls) %>% flatten() %>% as.character()
gcms <- basename(dirs) %>% unique()

# To get the right dates 
year <- if(prdo == '2030s'){yrs <- c('2021', '2040')} else{yrs <- c('2041', '2060')}
strt <- as.Date(paste0(parse_number(yrs[1]), '-01-01'), format = '%Y-%m-%d')
endn <- as.Date(paste0(parse_number(yrs[2]), '-12-01'), format = '%Y-%m-%d')
dtes <- seq(strt, endn, by = 'months')
dtes <- str_sub(dtes, start = 1, end = 7)

# Mosaic and extract by mask for the countries ----------------------------
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
  
  stk <- purrr::map(.x = 3:nrow(dts), .f = function(i){
    
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
    
    # To write the results 
    ou <- glue('../tif/future/ssp585/{gcm}/{prdo}/countries')
    yea_ftr <- dates %>% filter(year_base == dt$year_base & month_base == dt$month_base)
    mnt_ftr <- yea_ftr$month_ftre
    yea_ftr <- yea_ftr$year_ftre
    terra::writeRaster(x = st, filename = glue('{ou}/pr-tx-tn_{yea_ftr}_{mnt_ftr}.tif'), overwrite = TRUE)
    return(st)
    
  })
  
}

# To calc ETP Hargreaves  -----------------------------------------------------------

fles <- dir_ls('../tif/future/ssp585/ACCESS-ESM1-5/2050s/countries') %>% as.character()

# Solar radiation
srad <- dir_ls('../tif/srad/countries') %>% as.character %>% mixedsort() %>% terra::rast()
# year <- 2021:2040
year <- 2041:2060
prdo <- '2050s' 

# Potential Evapotranspiration
purrr::map(.x = 1:length(year), .f = function(i){
  
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



# To extract by mask  -----------------------------------------------------
prdo <- '2050s'
fles <- dir_ls(glue('../tif/future/ssp585/ACCESS-ESM1-5/{prdo}/countries'))
fles <- as.character(fles)
year <- 2041:2060
sspe <- 'ACCESS-ESM1-5'

purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\n')
  yea <- year[i]
  fls <- grep(glue('_{yea}_'), fles, value = T)
  
  stk <- grep('pr-tx-tn', fls, value = T) %>% terra::rast()
  ppt <- stk[[grep('prec', names(stk), value = F)]]
  tmx <- stk[[grep('tmax', names(stk), value = F)]]
  tmn <- stk[[grep('tmin', names(stk), value = F)]]
  etp <- grep('etp_2', fls, value = T) %>% mixedsort() %>% terra::rast()
  
  ppt <- terra::crop(ppt, zone) %>% terra::mask(., zone)
  tmx <- terra::crop(tmx, zone) %>% terra::mask(., zone)
  tmn <- terra::crop(tmn, zone) %>% terra::mask(., zone)
  etp <- terra::crop(etp, zone) %>% terra::mask(., zone)
  
  out <- glue('../tif/future/ssp585/ACCESS-ESM1-5/{prdo}/cocoa_zones')
  terra::writeRaster(ppt, glue('{out}/prec_{yea}.tif'), overwrite = T)
  terra::writeRaster(tmx, glue('{out}/tmax_{yea}.tif'), overwrite = T)
  terra::writeRaster(tmn, glue('{out}/tmin_{yea}.tif'), overwrite = T)
  terra::writeRaster(etp, glue('{out}/etp_{yea}.tif'), overwrite = T)
  
  
})

# Raster to table
prdo <- '2030s'
fles <- dir_ls(glue('../tif/future/ssp585/ACCESS-ESM1-5/{prdo}/cocoa_zones'))
fles <- as.character(fles)
year <- 2021:2040

tbls <- purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\n')
  yea <- year[i]
  fls <- grep(glue('_{yea}'), fles, value = T)
  
  ppt <- terra::rast(grep('prec', fls, value = T))
  tmx <- terra::rast(grep('tmax', fls, value = T))
  tmn <- terra::rast(grep('tmin', fls, value = T))
  etp <- grep('etp_2', fls, value = T) %>% mixedsort() %>% terra::rast()
  etp <- terra::resample(etp, tmx)
  
  names(ppt) <- paste0(gsub('.tif', '', basename(grep('prec', fls, value = T))), '_', 1:12)
  names(tmx) <- paste0(gsub('.tif', '', basename(grep('tmax', fls, value = T))), '_', 1:12)
  names(tmn) <- paste0(gsub('.tif', '', basename(grep('tmin', fls, value = T))), '_', 1:12)
  names(etp) <- paste0(gsub('.tif', '', basename(grep('etp', fls, value = T))), '_', 1:12)
  
  ext(ppt)
  ext(tmx)
  ext(tmn)
  ext(etp)
  stk <- c(ppt, tmx, tmn, etp)
  tbl <- terra::as.data.frame(stk, xy = TRUE) %>% as_tibble %>% mutate(gid = 1:nrow(.)) %>% dplyr::select(gid, x, y, everything())
  tbl <- gather(tbl, variable, value, -gid, -x, -y) %>% separate(., col = variable, sep = '_', into = c('variable', 'year', 'month'))
  tbl <- inner_join(tbl, tibble(month = as.character(1:12), month_abb = month.abb), by = 'month')
  tbl <- dplyr::select(tbl, -month)
  tbl <- spread(tbl, variable, value)
  tbl <- mutate(tbl, bal = prec - etp)
  return(tbl)
  
})

tbls <- bind_rows(tbls)
period <- c('2021-2040')
qs::qsave(tbls, file = glue('../qs/climate/countries/terraclimate/balance_{period}_{sspe}_cocoaZones.qs'))

# To calc SPEI  -----------------------------------------------------------
tbl1 <- qs::qread(glue('../qs/climate/countries/terraclimate/balance_2021-2040_{sspe}_cocoaZones.qs'))
tbl2 <- qs::qread(glue('../qs/climate/countries/terraclimate/balance_2041-2060_{sspe}_cocoaZones.qs'))

tble <- rbind(tbl1, tbl2)

gids <- pull(tble, gid) %>% unique()
length(gids)

rslt <- purrr::map(.x = 1:length(gids), .f = function(i){
  
  cat(gids[i], '\t')
  
  # Filtering for the gid
  tbl <- filter(tble, gid == gids[i])
  
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

rslt[[1]]
length(rslt)

rsl2 <- bind_rows(rslt)
rsl2

qs::qsave(x = rsl2, file = glue('../qs/climate/countries/future/speispi_2021-2040_{gcm}_cocoaZones.qs'))

# Join all the tables -----------------------------------------------------
tbl1 <- qs::qread('../qs/climate/countries/terraclimate/balance_1960-2019_terraclimate_cocoaZones.qs')
tbl2 <- qs::qread('../qs/climate/countries/terraclimate/balance_2021-2040_ACCESS-ESM1-5_cocoaZones.qs')
tbl3 <- qs::qread('../qs/climate/countries/terraclimate/balance_2041-2060_ACCESS-ESM1-5_cocoaZones.qs')
tbl1 <- inner_join(tbl1, tibble(month = as.character(1:12), month_abb = month.abb), by = 'month')
tbl1 <- dplyr::select(tbl1, -month)

colnames(tbl1)
colnames(tbl2)

nrow(tbl1); nrow(tbl2); nrow(tbl3)
length(unique(tbl1$gid))
length(unique(tbl2$gid))
length(unique(tbl3$gid))

# Stacking the rasters --------------------------------------------------------
startDate <- as.Date('1960-01-01', format = '%Y-%m-%d')
endDate <- as.Date('2060-12-31', format = '%Y-%m-%d')
Dates <- seq(startDate, endDate, by = 'month') %>% str_sub(., 1, 7)
Dates <- Dates[-grep('2020-', Dates, value = F)]

purrr::map(.x = 1:length(Dates), .f = function(i){
  
  Date <- Dates[i]
  
  tb1 <- inner_join(tbl1, tibble(month = 1:12, month_abb = month.abb), by = 'month_abb')
  tb1 <- mutate(tb1, month = ifelse(month < 10, paste0('0', month), as.character(month)))
  tb1 <- filter(tb1, year == str_sub(Date, 1, 4) & month == str_sub(Date, 6, 7))
  tb1 <- dplyr::select(tb1, x, y, tmax, tmin, tavg, prec, etp, bal) 
  rs1 <- terra::rast(tb1, type = 'xyz')
  names(rs1) <- glue('{names(rs1)}_{Date}')
  
  
  
  
})


rst1 <- tbl1 %>% filter(year == '1960' & month_abb == 'Jul') %>% dplyr::select(x, y, bal) %>% terra::rast(., type = 'xyz')
rst2 <- tbl2 %>% filter(year == '2021' & month_abb == 'Jul') %>% dplyr::select(x, y, bal) %>% terra::rast(., type = 'xyz')
rst3 <- tbl3 %>% filter(year == '2041' & month_abb == 'Jul') %>% dplyr::select(x, y, bal) %>% terra::rast(., type = 'xyz')

tbl1 <- dplyr::select(tbl1, colnames(tbl2))
fnal <- rbind(tbl1, tbl2, tbl3)



gids <- pull(tble, gid) %>% unique()
length(gids)

rslt <- purrr::map(.x = 1:length(gids), .f = function(i){
  
  cat(gids[i], '\t')
  
  # Filtering for the gid
  tbl <- filter(tble, gid == gids[i])
  
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

