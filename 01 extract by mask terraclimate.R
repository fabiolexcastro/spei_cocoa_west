
# -------------------------------------------------------------------------
# Author: @fabiolexcastro
# January 11th - 2022
# -------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, glue, stringr, sf, tidyverse, fs, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Set working directory
setwd('//catalogue/workspace-cluster9/SPEI_COCOA_WEST_AFRICA')

# Load data ---------------------------------------------------------------
root <- '//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_guatemala/_data/_nc/_world'
fles <- dir_ls(root, regexp = '.nc$')
vars <- c('tmin', 'ppt', 'tmax')
limt <- raster::shapefile('./data/shp/base/countries_target_4.shp')
plot(limt)
dout <- './data/raster/terraclimate/baseline'

# Function to use -----------------------------------------------------------
extract_mask <- function(fle){

    cat('Start ', basename(fle), '\n')
    rst <- raster::stack(fle)
    rst <- raster::crop(rst, limt)
    rst <- raster::mask(rst, limt)
    writeRaster(x = rst, filename = glue('{dout}/{basename(fle)}'), overwrite = TRUE)

}

# Apply the function --------------------------------------------------------
for(i in 1:length(fles)){
    extract_mask(fle = fles[i])
}
