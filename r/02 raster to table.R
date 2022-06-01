

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
path <- '../tif/cm6/zone'
dirs <- dir_ls(path) %>% as.character()

# Function ----------------------------------------------------------------
rs2tb <- function(dir){
  
  # dir <- dirs[1]
  
  # Listing files
  cat(basename(dir), '---------------------------------------------\n')
  fls <- dir_ls(dir, regexp = '.tif$')
  fls <- as.character(fls)
  
  dts <- basename(fls) %>% 
    str_split(., pattern = '_') 
  
  str <- dts %>% 
    sapply(., `[[`, 6) %>% 
    gsub('r1i1p1f1-', '', .) 
  
  end <- dts %>% 
    sapply(., `[[`, 7) %>% 
    gsub('.tif', '', .) 
  
  tbl <- purrr::map(.x = 1:length(fls), .f = function(i){
    
    cat(basename(fls[i]), '\t')
    tb <- fls[i] %>% 
      terra::rast() %>% 
      terra::as.data.frame(., xy = TRUE) %>%
      as_tibble() 
    
    vr <- fls[i] %>% 
      basename() %>% 
      str_split(., '_') %>% 
      sapply(., '[[', 2)
    
    st <- as.Date(str[i], format = '%Y-%m-%d')
    en <- as.Date(end[i], format = '%Y-%m-%d')
    dt <- seq.Date(st, en, by = 'day')
    dt <- as.character(dt)
    colnames(tb) <- c('lon', 'lat', dt)
    tb <- mutate(tb, gid = 1:nrow(tb))
    tb <- gather(tb, date, value, -gid, -lon, -lat)
    colnames(tb)[5] <- vr
    cat('Done!\n')
    return(tb)
    
  })
  
  # Baseline 
  bsl <- tbl[grep('1995', basename(fls))] %>% 
    purrr::reduce(., inner_join) %>% 
    mutate(pr = ifelse(pr < 0, 0, pr) * 86400, 
           tasmax = tasmax - 273.15,
           tasmin = tasmin - 273.15)
  
  ft1 <- tbl[grep('2021', basename(fls))] %>% 
    purrr::reduce(., inner_join) %>% 
    mutate(pr = ifelse(pr < 0, 0, pr) * 86400, 
           tasmax = tasmax - 273.15,
           tasmin = tasmin - 273.15)
  
  ft2 <- tbl[grep('2041', basename(fls))] %>% 
    purrr::reduce(., inner_join) %>% 
    mutate(pr = ifelse(pr < 0, 0, pr) * 86400, 
           tasmax = tasmax - 273.15,
           tasmin = tasmin - 273.15)
  
  all <- rbind(bsl, ft1, ft2)
  qs::qsave(x = all, file = glue('../qs/clima/tb_{basename(dir)}.qs'))
  cat('Finish!\n')
  
}

# All ---------------------------------------------------------------------
purrr::map(.x = dirs[4], .f = rs2tb)

 