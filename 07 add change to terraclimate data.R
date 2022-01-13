
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Load data ---------------------------------------------------------------
chng <- dir_ls('./data/raster/difference/cmip5', regexp = 'ensemble')
tclm <- dir_ls('./data/raster/terraclimate/baseline')
vars <- data.frame(var_tc = c('tmax', 'tmin', 'ppt'), var_ch = c('tmax', 'tmin', 'prec'))
year <- 1980:2010

# Functions to use --------------------------------------------------------
add_change <- function(var_tcl){
  
  var_tcl <- 'ppt'
  
  cat('Start ', var_tcl, '\n')
  var_chn <- filter(vars, var_tc == var_tcl) %>% pull(var_ch)
  var_tcl <- filter(vars, var_tc == var_tcl) %>% pull(var_tc)
  
  # Terraclimate files
  fls_tcl <- grep(var_tcl, tclm, value = TRUE)
  fls_tcl <- as.character(fls_tcl)
  
  # Changing files
  fls_chn <- grep(var_chn, chng, value = TRUE)
  fls_chn <- as.character(fls_chn)
  fls_chn <- mixedsort(fls_chn)
  
  map(.x = year, .f = function(i){
    
    cat('Processing ', i, '\n')
    fle_tcl <- grep(i, fls_tcl, value = TRUE)
    rst_tcl <- raster::stack(fle_tcl)
    
    cat('Repetition\n')
    rst_ftr <- map(.x = 1:12, .f = function(k){
      
      # k <- 1
      cat(k, '\n!')
      fle_ch <- grep(glue('_{k}.tif'), fls_chn, value = TRUE)
      
      # Read as a raster
      rst_ch <- raster::raster(fle_ch)
      rst_tc <- rst_tcl[[k]]
      
      # As a stack and convert to table
      stk <- raster::stack(rst_tc, rst_ch)
      tbl <- rasterToPoints(stk, spatial = FALSE)
      tbl <- as_tibble(tbl)
      names(tbl) <- c('lon', 'lat', 'value', 'change')
      tbl <- mutate(tbl, add = value * change / 100, future = value + add)
      
      # Table to raster  
      rsl <- rasterFromXYZ(tbl[,c('lon', 'lat', 'future')])
      
      # Return
      cat('Done!\n')
      return(rsl)
      
    })
    
    rst_ftr <- raster::stack(rst_ftr)
    dou <- glue('./data/raster/terraclimate/future')
    writeRaster(x = rst_ftr, filename = glue('{dou}/Terraclimate_{var_tcl}_{i + 40}.tif'), overwrite = TRUE)
    cat('Done!\n')
    
  })
  
  cat('----Finish----\n')
  
  
}

# Apply the function ------------------------------------------------------
add_change(var_tcl = 'tmax')
add_change(var_tcl = 'tmin')
add_change(var_tcl = 'prec')

