
# Function to downlaod baseline 
cat('To download baseline\n')
library(geodata)

zone <- terra::vect('../gpkg/countries_africa.gpkg')

# Download climate
prec <- geodata::worldclim_global(var = 'prec', res = 2.5, path = '../tmpr')
prec <- terra::crop(prec, zone) %>% terra::mask(., zone)

tmax <- geodata::worldclim_global(var = 'tmax', res = 2.5, path = '../tmpr')
tmax <- terra::crop(tmax, zone) %>% terra::mask(., zone)

tmin <- geodata::worldclim_global(var = 'tmin', res = 2.5, path = '../tmpr')
tmin <- terra::crop(tmin, zone) %>% terra::mask(., zone)

mask_5km <- prec[[1]] * 0 + 1
names(mask_5km) <- 'mask'
mask_5km <- raster::raster(mask_5km)

prec <- stack(prec)
tmax <- stack(tmax)
tmin <- stack(tmin)
 
# Download DEM

srtm <- geodata::elevation_global(res = 2.5, path = '../tmpr')
srtm <- terra::crop(srtm, zone)
srtm <- terra::mask(srtm, zone)
srtm <- raster::raster(srtm)
srtm[which(srtm[] < 0)] <- 0
