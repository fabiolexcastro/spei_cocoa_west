
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 24 - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, 
               gtools, SPEI, future, furrr, RColorBrewer, colorspace, 
               ggthemes, terra, lubridate, zoo, scales, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
make_slope <- function(i){
  
  # i <- 1
  cat(i, '\n')
  tbl <- filter(tble, id == i)  
  slp <- map(.x = 1:length(month.abb), .f = function(j){
    cat(j, '\n')
    tb <- tbl %>% filter(variable == month.abb[j])
    ts <- ts(pull(tb, spei))
    sl <- sens.slope(ts)
    sl <- tibble(slope = as.numeric(sl$estimates), pvalue = sl$p.value, month = month.abb[j])
    return(sl)
  }) %>% 
    bind_rows() %>% 
    mutate(gid = i)
  cat('Done!\n')
  return(slp)
  
}

make_graph <- function(dfm){
  # dfm <- slp1_sub[1,]
  cat('Start!', '\n')
  sub <- tble %>% filter(id == pull(dfm, 4))
  sub <- sub  %>% filter(variable == pull(dfm, 3))
  ggp <- ggplot(data = sub, aes(x = date, y = spei)) + 
    geom_line() + 
    geom_smooth(se = FALSE, method = 'loess') + 
    scale_x_date(breaks = '5 year', labels = date_format('%Y')) +
    theme_pander() + 
    labs(x = 'Year', y = 'SPEI') + 
    ggtitle(label = glue('SPEI (coordinate: {dfm$gid} - Month = {pull(dfm, month)})'),
            subtitle = glue('Slope: {round(dfm$slope, 4)}/year -/- Pvalue: {round(dfm$pvalue, 4)}')) +
    scale_y_continuous(limits = c(-2.5, 2.5), breaks = c(-2.5, -2.0, -1.5, -1.0, -0.5, 0, 0.5, 1, 1.5, 2.0, 2.5)) +
    geom_hline(yintercept =  0, linetype = 'dashed', color = 'red') +
    geom_hline(yintercept = -1, linetype = 'dashed', color = 'blue') +
    geom_hline(yintercept =  1, linetype = 'dashed', color = 'blue') +
    theme(axis.text.x = element_text(angle = 90), 
          plot.title = element_text(hjust = 0.5, face = 'bold', size = 12)) 
  
  cat('Done!\n')
  return(ggp)
}

# Load data ---------------------------------------------------------------
bsln <- read_csv('./data/tbl/points/trgt_gha_baseline_vles.csv')
ftre <- read_csv('./data/tbl/points/trgt_gha_future_BCC.CSM2.MR_vles.csv')
bsln <- mutate(bsln, model = 'baseline')

# Join both values --------------------------------------------------------
tble <- rbind(bsln, ftre)
lbls <- tibble(month = 1:12, month_abb = month.abb)
tble <- inner_join(tble, lbls, by = c('variable' = 'month_abb'))
tble <- mutate(tble, month = ifelse(month < 10, paste0('0', month), month))
tble <- mutate(tble, date = as.Date(paste0(year, '-', month, '-01')))
tble <- mutate(tble, dat2 = as.yearmon(date, '%Y-%m'))

# Apply the function ------------------------------------------------------
slp1 <- make_slope(i = 1)
slp2 <- make_slope(i = 2)
slp3 <- make_slope(i = 3)

slp1_sub <- slp1 %>% filter(pvalue < 0.05)
slp2_sub <- slp2 %>% filter(pvalue < 0.05)
slp3_sub <- slp3 %>% filter(pvalue < 0.05)

# By month analysis -------------------------------------------------------
slp1_sub
slp1_ggp_mar <- make_graph(dfm = slp1_sub[1,])
slp2_ggp_jan <- make_graph(dfm = slp2_sub[1,])
slp2_ggp_may <- make_graph(dfm = slp2_sub[2,])
slp3_ggp_may <- make_graph(dfm = slp3_sub[1,])
slp3_ggp_oct <- make_graph(dfm = slp3_sub[2,])

ggsave(plot = slp1_ggp_mar, filename = './png/graphs/slope/slp1_ggp_mar.jpg', units = 'in', width = 7, height = 6, dpi = 300)
ggsave(plot = slp2_ggp_jan, filename = './png/graphs/slope/slp2_ggp_jan.jpg', units = 'in', width = 7, height = 6, dpi = 300)
ggsave(plot = slp2_ggp_may, filename = './png/graphs/slope/slp2_ggp_may.jpg', units = 'in', width = 7, height = 6, dpi = 300)
ggsave(plot = slp3_ggp_may, filename = './png/graphs/slope/slp3_ggp_may.jpg', units = 'in', width = 7, height = 6, dpi = 300)
ggsave(plot = slp3_ggp_oct, filename = './png/graphs/slope/slp3_ggp_oct.jpg', units = 'in', width = 7, height = 6, dpi = 300)


