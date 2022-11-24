

# Fabio Alexander Castro Llanos  ------------------------------------------
# October 18th / 2022
# To write the raster files for climate data (daily to monthly)

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, gtools, rgeos, stringr, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------

# Interesting paths
path <- tibble(paths = c('//CATALOGUE/WFP_ClimateRiskPr1/10.New_Results/bias_corrected', 
                         '//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/Chirts', 
                         '//CATALOGUE/Workspace14/WFP_ClimateRiskPr/1.Data/climate/CMIP6/download_data', 
                         '//CATALOGUE/WFP_ClimateRiskPr1/7.Results/Colombia'), 
               type = c('Datasets for Africa', 'Baseline climate (CHIRPS - CHIRTS)', 'Raw climate future', 'Future Colombia Bias Corrected'))

# Administrative data
cntr <- st_read('../gpkg/countries_zones.gpkg')
zone <- st_read('../gpkg/zona_buffer.gpkg')
isos <- cntr$sov_a3

# Path --------------------------------------------------------------------
dirc <- path$paths[1]
dirs <- dir_ls(dirc) %>% 
  as.character() %>% 
  map(., dir_ls) %>% 
  flatten() %>% 
  as.character() %>% 
  grep(paste0(isos, collapse = '|'), ., value = TRUE) %>% 
  map(., dir_ls) %>% 
  map(., 2) %>% 
  unlist() %>% 
  map(., dir_ls) %>% 
  unlist() %>% 
  as.character()

gcms <- unique(basename(dirs))

# Read the dataset --------------------------------------------------------

toWrite <- function(gcm){
  
  # Proof
  gcm <- gcms[1]
  i <- 1
  j <- 1
  
  cat('Start ', gcm, '\n')
  drs <- grep(gcm, dirs, value = T) 
  prd <- str_split(drs, pattern = '/') %>% map(7) %>% unlist() %>% unique()
  
  purrr::map(.x = 1:length(prd), .f = function(i){
    
    prd <- prd[i]
    dir <- grep(prd, drs, value = T)
    fls <- map(dir, dir_ls) %>% flatten() %>% as.character()
    isos
    
    # Dates
    yrs <- if(prd == '2030s'){
      yrs <- c('2021', '2040')
    } else{
      yrs <- c('2041', '2060')
    }
    
    str <- as.Date(paste0(parse_number(yrs[1]), '-01-01'), format = '%Y-%m-%d')
    end <- as.Date(paste0(parse_number(yrs[2]), '-01-01'), format = '%Y-%m-%d')
    dts <- seq(str, end, by = 'days')
    
    dts
    length(dts)
    get_dates <- function(x){fls %>% grep(x, ., value = TRUE) %>% str_split(., pattern = '__') %>% purrr::map(., 2) %>% unlist() %>% str_sub(., 13, nchar(.) - 4) %>% gsub('\\.', '-', .)}
    
    dtes <- tibble(prec = get_dates('_pr_')[-grep('-02-29', get_dates('_pr_'), value = FALSE)],
                   tmax = get_dates('_tasmax_')[-grep('-02-29', get_dates('_tasmax_'), value = FALSE)],
                   tmin = get_dates('_tasmin_')[-grep('-02-29', get_dates('_tasmin_'), value = FALSE)] )
    
    pos_prec <- grep('-02-29', get_dates('_pr_'), value = FALSE)
    pos_tmax <- grep('-02-29', get_dates('_tasmax_'), value = FALSE)
    pos_tmin <- grep('-02-29', get_dates('_tasmin_'), value = FALSE)
    
    purrr:map(.x = 1:length(isos), .f = function(j){
      
      iso <- isos[j]
      fls <- grep(iso, drs, value = TRUE) %>% grep(prd, ., value = T) %>% dir_ls() %>% as.character()
      basename(fls)
      
      length(fls)
      length(dts)
      vrs <- basename(fls) %>% str_split(., pattern = '_') %>% purrr::map(., 4) %>% unlist() %>% unique()
      ppt <- grep('pr', fls, value = T)
      tmx <- grep('tasmax', fls, value = T)
      tmn <- grep('tasmin', fls, value = T)
      
      length(dts)
      length(tmx)
      
      
    })
    
    rst <- terra::rast(fls)
    
    
    
  })
  prd <- str_split(fls, pattern = '/') %>% purrr::map(., 5)
  fls <- map(fls, dir_ls) %>% unlist() %>% as.character()
  
  Dts <- basename(fls) %>% 
    str_split(., pattern = '__') %>% 
    purrr::map(., 2) %>% 
    unlist()
  
}








