

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
  
  ggp <- ggplot(data = tbl, aes(x = date, y = spei), group = 1) + 
    geom_line(size = 0.1) + 
    scale_x_date(breaks = '5 year', labels = date_format('%Y')) +
    geom_hline(yintercept =  0, linetype = 'dashed', color = 'red') +
    geom_hline(yintercept = -1, linetype = 'dashed', color = 'blue') +
    geom_hline(yintercept =  1, linetype = 'dashed', color = 'blue') +
    ggtitle(label = glue('SPEI (coordinate: {gid})')) +
    theme_pander() + 
    scale_y_continuous(limits = c(-2.5, 2.5), breaks = c(-2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2.0, 2.5)) +
    theme(axis.text.x = element_text(angle = 90), 
          plot.title = element_text(hjust = 0.5, face = 'bold', size = 12)) + 
    labs(x = '', y = '') 
  
  ggsave(plot = ggp, filename = glue('./png/graphs/spei_{gid}_v1.png'), units = 'in', width = 9, height = 6, dpi = 300)
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
bsln <- read_csv('./data/tbl/points/trgt_gha_baseline_vles.csv')
ftre <- read_csv('./data/tbl/points/trgt_gha_future_BCC.CSM2.MR_vles.csv')
bsln <- mutate(bsln, model = 'baseline')

tble <- rbind(bsln, ftre)
lbls <- tibble(month = 1:12, month_abb = month.abb)

# Make graph --------------------------------------------------------------
map(.x = 1:3, .f = make_graph)

