
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, terra, sf, fs, tidyverse, glue)

g <- gc(reset = TRUE)
rm(list = ls)
options(sicpen = 999)

# Load data ---------------------------------------------------------------
path <- '../tif/nasa/baseline'
fles <- dir_ls(path) %>% as.character(fles)
mdls <- basename(fles) %>% grep('tas_day', ., value = T) %>% str_split(., pattern = '_') %>% sapply(., `[[`, 3) %>% unique()

dirs <- glue('{path}/{mdls}')
map(dirs, dir_create)

dirs <- glue('{dirs}/world')
map(dirs, dir_create)

# Moving ------------------------------------------------------------------

purrr::map(.x = 1:length(mdls), .f = function(i){
  
  mdl <- mdls[i]
  fls <- grep(mdl, fles, value = T)
  
  purrr::map(.x = 1:length(fls), .f = function(j){
    
    fle <- fls[j]
    out <- glue('../tif/nasa/baseline/{mdl}/world/{basename(fle)}')
    fs::file_move(path = fle, new_path = out)
    
  })
  
  
  
})


file_move(path = )



