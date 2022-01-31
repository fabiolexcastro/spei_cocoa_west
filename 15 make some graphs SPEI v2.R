

# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 24 - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, RColorBrewer, colorspace, 
               ggthemes, terra, lubridate, zoo, scales)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
make_graph <- function(gid){
  
  # gid <- 1
  cat('Start ', gid, '\n')
  tbl <- filter(tble, id == gid)
  tbl <- inner_join(tbl, lbls, by = c('variable' = 'month_abb'))
  tbl <- mutate(tbl, month = ifelse(month < 10, paste0('0', month), month))
  tbl <- mutate(tbl, date = as.Date(paste0(year, '-', month, '-01')))
  tbl <- mutate(tbl, dat2 = as.yearmon(date, '%Y-%m'))
  tbl <- tbl %>% gather(type, value, -id, -mdl, -variable, -year, -date, -month, -dat2)
  tbl <- tbl %>% mutate(type = ifelse(type == 'spei_01', 'SPEI 01', ifelse(type == 'spei_03', 'SPEI 03', 'SPEI 06')))
  tbl <- tbl %>% mutate(type = factor(type, levels = c('SPEI 01', 'SPEI 03', 'SPEI 06')))
  
  ggp <- ggplot(data = tbl, aes(x = date, y = value), group = 1) + 
    geom_line(size = 0.1) + 
    facet_wrap(.~type) +
    scale_x_date(breaks = '5 year', labels = date_format('%Y')) +
    geom_hline(yintercept =  0, linetype = 'dashed', color = 'red') +
    geom_hline(yintercept = -1, linetype = 'dashed', color = 'blue') +
    geom_hline(yintercept =  1, linetype = 'dashed', color = 'blue') +
    # ggtitle(label = glue('SPEI (coordinate: {gid}) - Scale: {scl}')) +
    theme_pander() + 
    scale_y_continuous(limits = c(-2.5, 2.5), breaks = c(-2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2.0, 2.5)) +
    theme(axis.text.x = element_text(angle = 90), 
          plot.title = element_text(hjust = 0.5, face = 'bold', size = 12)) + 
    labs(x = '', y = '') 
  
  ggsave(plot = ggp, filename = glue('./png/graphs/values raw/spei_{gid}_allScales.png'), units = 'in', width = 12, height = 6, dpi = 300)
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------

mdel <- 'BCC.CSM2.MR'
scls <- c('01', '03', '06')

# Baseline
bsln <- dir_ls('./data/tbl/points', regexp = 'baseline') %>% 
  as.character %>% 
  map(read_csv)
bsln <- map(.x = 1:length(scls), .f = function(k){bsln[[k]] %>% setNames(c('id', 'variable', glue('spei_{scls[k]}'), 'year'))})
bsln <- bsln %>% purrr::reduce(., inner_join)
bsln <- bsln %>% mutate(mdl = 'baseline')
bsln <- bsln %>% dplyr::select(id, mdl, variable, year, spei_01, spei_03, spei_06)

# Future
ftre <- dir_ls('./data/tbl/points', regexp = 'future') %>%
  grep(mdel, ., value = TRUE) %>% 
  as.character %>% 
  map(read_csv) 
ftre <- map(.x = 1:length(scls), .f = function(k){ftre[[k]] %>% setNames(c('id', 'variable', glue('spei_{scls[k]}'), 'year'))})
ftre <- ftre %>% purrr::reduce(., inner_join)
ftre <- ftre %>% dplyr::select(id, mdl, variable, year, spei_01, spei_03, spei_06)

tble <- rbind(bsln, ftre)
lbls <- tibble(month = 1:12, month_abb = month.abb)

# Make graph --------------------------------------------------------------
map(.x = 1:3, .f = make_graph)

