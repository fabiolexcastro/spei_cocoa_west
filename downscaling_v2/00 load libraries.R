


cat('Start!\n')
require(pacman)
pacman::p_load(terra, spatialEco, gganimate, sf, fs, future, lubridate, RColorBrewer, furrr, glue, crayon, qs, SPEI,
               rnaturalearthdata, raster, geodata, fields, tidyverse)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

cat(green('Done!\n'))