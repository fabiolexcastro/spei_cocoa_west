

# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Functions ---------------------------------------------------------------
source('01 functions.R')

# Load data ---------------------------------------------------------------
zone <- terra::vect('../gpkg/countries_africa.gpkg')
path <- '//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/climate/CMIP6/interim/rotated'
gcmf <- 'MPI-ESM1-2-HR'
dire <- glue('{path}/{gcmf}') %>% as.character()

# Apply the function  -----------------------------------------------------
extrat_by_mask(dir = dire)

# Check the results -------------------------------------------------------
fles <- dir_ls('../tif/cm6/zone/MPI-ESM1-2-HR', regexp = '.tif$')
fles <- as.character(fles)

# Sequence date -----------------------------------------------------------
seq <- seq(as.Date('1995-1-1'), as.Date('2014-12-30'), by = 'day')
mnt <- str_sub(seq, 1, 7) %>% unique()

# Precipitation -----------------------------------------------------------
prec <- grep('pr', fles, value = T)
prec_hstr <- grep('historical', prec, value = T) %>% terra::rast()
names(prec_hstr) <- seq

prec_hstr_mnth <- purrr::map(.x = 1:length(mnt), .f = function(i){
  cat(mnt[i], '\n')
  pp <- prec_hstr[[grep(mnt[i], names(prec_hstr), value = F)]]
  pp <- pp * 86400
  pp <- sum(pp)
  return(pp)
})

# Raster to table 
prec_tble <- do.call('c', prec_hstr_mnth)
prec_tble <- terra::as.data.frame(prec_tble, xy = T) 
colnames(prec_tble) <- c('x', 'y', glue('y{mnt}'))
prec_tble <- mutate(prec_tble, gid = 1:nrow(prec_tble))
prec_tble <- gather(prec_tble, date, value, -gid, -x, -y)
prec_tble <- as_tibble(prec_tble)
colnames(prec_tble)[5] <- 'prec'

# T max -------------------------------------------------------------------
tmax <- grep('tasmax', fles, value = T)
tmax_hstr <- grep('historical', tmax, value = T) %>% terra::rast()
names(tmax_hstr) <- seq

tmax_hstr_mnth <- purrr::map(.x = 1:length(mnt), .f = function(i){
  cat(mnt[i], '\n')
  tx <- tmax_hstr[[grep(mnt[i], names(tmax_hstr), value = F)]]
  tx <- tx - 273.15
  tx <- mean(tx)
  return(tx)
})

# Raster to table
tmax_tble <- do.call('c', tmax_hstr_mnth)
tmax_tble <- terra::as.data.frame(tmax_tble, xy = T) 
colnames(tmax_tble) <- c('x', 'y', glue('y{mnt}'))
tmax_tble <- mutate(tmax_tble, gid = 1:nrow(tmax_tble))
tmax_tble <- gather(tmax_tble, date, value, -gid, -x, -y)
tmax_tble <- as_tibble(tmax_tble)
colnames(tmax_tble)[5] <- 'tmax'

# T tmin -------------------------------------------------------------------
tmin <- grep('tasmin', fles, value = T)
tmin_hstr <- grep('historical', tmin, value = T) %>% terra::rast()
names(tmin_hstr) <- seq

tmin_hstr_mnth <- purrr::map(.x = 1:length(mnt), .f = function(i){
  cat(mnt[i], '\n')
  tn <- tmin_hstr[[grep(mnt[i], names(tmin_hstr), value = F)]]
  tn <- tn - 273.15
  tn <- mean(tn)
  return(tn)
})

# Raster to table
tmin_tble <- do.call('c', tmin_hstr_mnth)
tmin_tble <- terra::as.data.frame(tmin_tble, xy = T) 
colnames(tmin_tble) <- c('x', 'y', glue('y{mnt}'))
tmin_tble <- mutate(tmin_tble, gid = 1:nrow(tmin_tble))
tmin_tble <- gather(tmin_tble, date, value, -gid, -x, -y)
tmin_tble <- as_tibble(tmin_tble)
colnames(tmin_tble)[5] <- 'tmin'

# Join the three dataframes -----------------------------------------------
tble <- list(prec_tble, tmax_tble, tmin_tble) %>% 
  purrr::reduce(., inner_join, by = c('x', 'y', 'gid', 'date'))

dir_create('../qs/climate/raw')
qs::qsave(tble, '../qs/climate/raw/tble_allw.qs')

# End ---------------------------------------------------------------------


