


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, glue, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fles <- dir_ls('../tif/dwn/cm6/EC-Earth3-Veg') %>% as.character()

# Grep a testing
tstn <- grep('tmax', fles, value = T) %>% 
  .[1] %>% 
  terra::rast()

# Check the empty files ---------------------------------------------------
tble <- fles %>% 
  basename() %>% 
  as_tibble() 
data <- str_split(string = pull(tble, value), pattern = '_')
tble <- tble %>% 
  mutate(variable = sapply(data, '[[', 1), 
         year = sapply(data, '[[', 2), 
         month = sapply(data, '[[', 3) %>% gsub('.tif', '', .))

tble <- tble %>% 
  dplyr::select(variable, year, month)


full <- expand.grid(variable = c('prec', 'tmax', 'tmin'), year = c(1995:2014, 2021:2060), month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')) %>% 
  arrange(year, month) %>% 
  as_tibble() %>% 
  mutate(month = as.character(month), 
         variable = as.character(variable), 
         year = as.character(year))

miss <- anti_join(full, tble)
save(miss, file = './miss.rds')
load('./miss.rds')

# Summarise ---------------------------------------------------------------

smmr <- tble %>% 
  group_by(variable, year) %>% 
  count() %>% 
  ungroup()

miss <- smmr %>% filter(n != 12)

# Precipitation 
miss %>% filter(variable == 'tmax')
miss %>% filter(variable == 'prec')

tble %>% 
  filter(variable == 'prec')




tble %>% 
  filter(year == 2003) %>% 
  filter(variable == 'prec')

which(tble$variable == 'prec' & tble$year == 2003)



