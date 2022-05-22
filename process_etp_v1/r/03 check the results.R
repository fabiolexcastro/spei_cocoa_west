

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Function ----------------------------------------------------------------
day2mnt <- function(fle){
  
  cat(basename(fle), '\n')
  tbl <- qs::qread(fle)
  tbl <- mutate(tbl, yea = str_sub(date, 1, 4), mnt = str_sub(date, 6, 7))
  smm <- tbl %>% 
    group_by(gid, lon, lat, yea, mnt) %>% 
    dplyr::summarise(pr = sum(pr),
                     tasmin = mean(tasmin),
                     tasmax = mean(tasmax)) %>% 
    ungroup() %>%
    mutate(model = basename(fle), 
           model = gsub('tb_', '', model), 
           model = gsub('.qs', '', model)) %>% 
    dplyr::select(model, gid, lon, lat, everything())
  
  qs::qsave(x = smm, file = glue('../qs/clima/dy_{unique(smm$model)}.qs'))
  
  cat('Done!\n')
  return(smm)
  
}

# Load data ---------------------------------------------------------------
path <- '../qs/clima/'
fles <- dir_ls(path)
limt <- st_read('../shp/countries_africa.gpkg')

# Day to month ------------------------------------------------------------
rslt <- purrr::map(.x = fles, .f = day2mnt)

