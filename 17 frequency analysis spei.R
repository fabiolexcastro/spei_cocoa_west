

# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# February 2nd - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, RColorBrewer, colorspace, 
               ggthemes, ggpubr)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)


# Functions to use --------------------------------------------------------
make_freq <- function(tbl, gid, col, yrs){
  
  # tbl <- coor_bsln
  # gid <- 'g1'
  # col <- 'spei_01'
  # yrs <- 1980:2019
  
  cat('Start\n')
  smm <- tbl %>% 
    filter(spei_01 <= -1.5 | spei_01 >= 1.5, 
           spei_03 <= -1.5 | spei_03 >= 1.5,
           spei_06 <= -1.5 | spei_06 >= 1.5) %>% 
    ungroup() %>% 
    dplyr::select(id, month, year, col) %>% 
    setNames(c('id', 'month', 'year', 'value')) %>% 
    group_by(id, year, month) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    group_by(id, year) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(id = glue('g{id}')) %>% 
    spread(id, count) %>% 
    replace(is.na(.), 0) %>% 
    full_join(., tibble(year = yrs)) %>% 
    arrange(year) %>% 
    replace(is.na(.), 0) 
  
  smm_gth <- smm %>% 
    gather(var, value, -year) %>% 
    mutate(var = factor(var, levels = glue('g{1:3}'))) 
  unq <- unique(smm_gth$value) %>% sort()
  smm_gth <- smm_gth %>% mutate(value = factor(value, levels = unq))
  smm_gth <- smm_gth %>% filter(var == gid)
  smm_gth <- smm_gth %>% mutate(value = as.numeric(as.character(value)))
  
  cat('To make the graph\n')
  ggp <- ggplot(data = smm_gth, aes(x = year, y = value)) + 
    geom_col() + 
    theme_pander() + 
    ggtitle(label = glue('Months frequency with SPEI <= -1.5 & SPEI >= 1.5'), 
            subtitle = glue('Coordinate: {parse_number(gid)} - {gsub("_", " ", toupper(col))}')) +
    labs(x = 'Year', y = 'Count (months)') + 
    theme(axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(size = 12, hjust = 0.5, face = 'bold')) +
    scale_y_continuous(limits = c(0, 3))
  
  return(ggp)
  
}

# Load data ---------------------------------------------------------------
scls <- c('01', '03', '06')
fles <- dir_ls('./data/rds/climate/spei', regexp = '.rds$')
mdls <- c('BCC.CSM2.MR', 'CanESM5', 'CNRM.CM6.1', 'CNRM.ESM2.1')
mdel <- mdls[1]

# Baseline
bsln <- grep('baseline', fles, value = TRUE) %>% as.character() %>% map(read_rds)
bsln <- map2(bsln, scls, function(x, y) x %>% setNames(c('x', 'y', 'gid', 'date', 'year', 'month', glue('spei_{y}'))))
bsln <- purrr::reduce(bsln, inner_join)

# Future
ftre <- grep(mdel, fles, value = TRUE) %>% as.character() %>% map(read_rds)
ftre <- map2(ftre, scls, function(x, y) x %>% setNames(c('x', 'y', 'gid', 'date', 'year', 'month', glue('spei_{y}'))))
ftre <- purrr::reduce(ftre, inner_join)

# Coordinates
coor <- read_csv('./data/tbl/points/trgt_gha.csv')
coor_bsln <- dir_ls('./data/tbl/points', regexp = 'baseline') %>% map(., read_csv)
coor_bsln <- map2(coor_bsln, scls, function(x, y) x %>% setNames(c('id', 'month', glue('spei_{y}'), 'year')))
coor_bsln <- purrr::reduce(coor_bsln, inner_join)

coor_ftre <- dir_ls('./data/tbl/points', regexp = 'future') %>% grep('BCC.CSM2.MR', ., value = TRUE) %>% map(., read_csv)
coor_ftre <- map2(coor_ftre, scls, function(x, y) x %>% setNames(c('id', 'month', glue('spei_{y}'), 'year', 'model')))
coor_ftre <- purrr::reduce(coor_ftre, inner_join)

# Analysis function -------------------------------------------------------

# Baseline
gg1.crn.s01 <- make_freq(tbl = coor_bsln, gid = 'g1', col = 'spei_01', yrs = 1980:2019)       
gg1.crn.s03 <- make_freq(tbl = coor_bsln, gid = 'g1', col = 'spei_03', yrs = 1980:2019)       
gg1.crn.s06 <- make_freq(tbl = coor_bsln, gid = 'g1', col = 'spei_06', yrs = 1980:2019)  
gg1.crn <- grid.arrange(gg1.crn.s01, gg1.crn.s03, gg1.crn.s06, ncol = 1, nrow = 3)

gg2.crn.s01 <- make_freq(tbl = coor_bsln, gid = 'g2', col = 'spei_01', yrs = 1980:2019)       
gg2.crn.s03 <- make_freq(tbl = coor_bsln, gid = 'g2', col = 'spei_03', yrs = 1980:2019)       
gg2.crn.s06 <- make_freq(tbl = coor_bsln, gid = 'g2', col = 'spei_06', yrs = 1980:2019)  
gg2.crn <- grid.arrange(gg2.crn.s01, gg2.crn.s03, gg2.crn.s06, ncol = 1, nrow = 3)

gg3.crn.s01 <- make_freq(tbl = coor_bsln, gid = 'g3', col = 'spei_01', yrs = 1980:2019)       
gg3.crn.s03 <- make_freq(tbl = coor_bsln, gid = 'g3', col = 'spei_03', yrs = 1980:2019)       
gg3.crn.s06 <- make_freq(tbl = coor_bsln, gid = 'g3', col = 'spei_06', yrs = 1980:2019)  
gg3.crn <- grid.arrange(gg3.crn.s01, gg3.crn.s03, gg3.crn.s06, ncol = 1, nrow = 3)

# Future
gg1.ftr.s01 <- make_freq(tbl = coor_ftre, gid = 'g1', col = 'spei_01', yrs = 2020:2040)       
gg1.ftr.s03 <- make_freq(tbl = coor_ftre, gid = 'g1', col = 'spei_03', yrs = 1980:2019)       
gg1.ftr.s06 <- make_freq(tbl = coor_ftre, gid = 'g1', col = 'spei_06', yrs = 1980:2019)  
gg1.ftr <- grid.arrange(gg1.ftr.s01, gg1.ftr.s03, gg1.ftr.s06, ncol = 1, nrow = 3)

gg2.ftr.s01 <- make_freq(tbl = coor_ftre, gid = 'g2', col = 'spei_01', yrs = 1980:2019)       
gg2.ftr.s03 <- make_freq(tbl = coor_ftre, gid = 'g2', col = 'spei_03', yrs = 1980:2019)       
gg2.ftr.s06 <- make_freq(tbl = coor_ftre, gid = 'g2', col = 'spei_06', yrs = 1980:2019)  
gg2.ftr <- grid.arrange(gg2.ftr.s01, gg2.ftr.s03, gg2.ftr.s06, ncol = 1, nrow = 3)

gg3.ftr.s01 <- make_freq(tbl = coor_ftre, gid = 'g3', col = 'spei_01', yrs = 1980:2019)       
gg3.ftr.s03 <- make_freq(tbl = coor_ftre, gid = 'g3', col = 'spei_03', yrs = 1980:2019)       
gg3.ftr.s06 <- make_freq(tbl = coor_ftre, gid = 'g3', col = 'spei_06', yrs = 1980:2019)  
gg3.ftr <- grid.arrange(gg3.ftr.s01, gg3.ftr.s03, gg3.ftr.s06, ncol = 1, nrow = 3)

# GGarrange all
gg1.all <- grid.arrange(gg1.crn, gg1.ftr, ncol = 2, nrow = 1)
gg2.all <- grid.arrange(gg2.crn, gg2.ftr, ncol = 2, nrow = 1)
gg3.all <- grid.arrange(gg3.crn, gg3.ftr, ncol = 2, nrow = 1)

ggsave(plot = gg1.all, filename = './png/graphs/index_freq/gid_1_crn_ftr.png', units = 'in', width = 11, height = 9, dpi = 300)
ggsave(plot = gg2.all, filename = './png/graphs/index_freq/gid_2_crn_ftr.png', units = 'in', width = 11, height = 9, dpi = 300)
ggsave(plot = gg3.all, filename = './png/graphs/index_freq/gid_3_crn_ftr.png', units = 'in', width = 11, height = 9, dpi = 300)

