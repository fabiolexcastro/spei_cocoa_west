
# Load libraries ----------------------------------------------------------
options(warn = -1, scipen = 999)
require(pacman)
pacman::p_load(terra, tidyverse, crayon, glue, fs, sf, glue)

g <- gc(reset = T)
rm(list = ls())

# Function ----------------------------------------------------------------
download <- function(mdel, type){
  
  # mdel <- 'ACCESS-CM2'
  cat(bgRed(mdel, ' ', type, '\n'))
  
  base <- 'https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6'
  vars <- c('pr', 'tasmin', 'tasmax', 'tas')
  type <- 'gn'
  
  # # Baseline ----------------------------------------------------------------
  # year <- 1980:2010
  # purrr::map(.x = 1:length(vars), .f = function(i){
  #   
  #   path <- glue('{base}/{mdel}/historical/r1i1p1f1/{vars[i]}/{vars[i]}_day_{mdel}_historical_r1i1p1f1_{type}_{year}.nc')
  #   dest <- glue('../tif/nasa/baseline/{mdel}/{basename(path)}')
  #   dir_create(dirname(dest))
  #   download.file(path, dest, mode = 'wb')
  #   cat(dest, '\n')
  #   
  # })
  
  # Future ------------------------------------------------------------------
  year <- 2015:2050
  purrr::map(.x = 2:length(vars), .f = function(i){
    
    path <- glue('{base}/{mdel}/ssp585/r1i1p1f1/{vars[i]}/{vars[i]}_day_{mdel}_ssp585_r1i1p1f1_{type}_{year}.nc')
    dest <- glue('../tif/nasa/future/{mdel}/world/{basename(path)}')
    # path <- 'https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6/ACCESS-CM2/ssp585/r1i1p1f1/tasmin/tasmin_day_ACCESS-CM2_ssp585_r1i1p1f1_gn_2015.nc'
    dir_create(dirname(dest))
    
    purrr::map(.x = 1:length(year), .f = function(yr){
      download.file(url = path[yr], destfile = dest[yr])
      cat(dest, '\n')
    })
    
  })

  
}

# -------------------------------------------------------------------------
# Download data -----------------------------------------------------------
# -------------------------------------------------------------------------

# Climate data
# URL https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6

# Models ------------------------------------------------------------------
mdls <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CESM2', 'CESM2-WACCM', 'CMCC-CM2-SR5',
          'CMCC-CM2-SR5', 'CMM-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'CanESM5', 'EC-Earth3', 'EC-Earth3-Veg-LR',
          'FGOALS-g3', 'GFDL-CM4', 'GFDL-CM4_gr2', 'GFDL-ESM4', 'GISS-E2-1-G', 'HadGEM3-GC31-LL', 'HadGEM3-GC31-MM', 
          'IITM-ESM', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR', 'KACE-1-0-G', 'KIOST-ESM', 'MIRCO-ES2L', 'MIROC6', 
          'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'NESM3', 'NorESM2-LM', 'NorESM2-MM', 'TaiESM1', 'UKESM1-0-LL')


# Down --------------------------------------------------------------------
purrr::map(.x = 1:length(years), .f = function(j){
  download(year = years[j], mdel = 'ACCESS-CM2', type = 'gn')
})




