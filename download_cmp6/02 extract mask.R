
# Load libraries ----------------------------------------------------------
options(warn = -1, scipen = 999)
require(pacman)
pacman::p_load(terra, tidyverse, rnaturalearthdata, crayon, fs, sf, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fles <- dir_ls('../tif/nasa/baseline', type = 'directory')
mdls <- basename(fles)
limt <- rnaturalearthdata::map::::countries110(scale = 50, returnclass = 'sf')
cntr <- c("Cameroon", "Côte d'Ivoire", "Ghana", "Liberia", "Nigeria", "Sierra Leone", "Togo", 'Benin')
limt <- filter(limt, name %in% cntr)
limt <- vect(limt)

# Function ----------------------------------------------------------------
get.rst <- function(mdl){
  cat(mdl, '\n')
  fls <- grep(mdl, fles, value = T)
  fls <- dir_ls(fls) %>% grep('world', ., value = T) %>% as.character()
  fls <- dir_ls(fls)
  out <- glue('../tif/nasa/baseline/{mdl}/cntr')
  dir_create(out)
  rst <- map(.x = 1:length(fls), .f = function(i){
    cat(basename(fls[i]), '\t')
    fle <- fls[i]
    trr <- terra::rast(fle)
    rtt <- terra::rotate(trr)
    rtt <- terra::crop(rtt, limt)
    terra::writeRaster(x = rtt, filename = glue('{out}/{basename(fls[i])}'))
    cat('Done!\n')
    rm(trr)
  })
  cat('Done!\n')
}

# Extract by mask  --------------------------------------------------------
purrr::map(mdls[2:length(mdls)], get.rst)

