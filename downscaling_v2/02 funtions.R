



make_gif <- function(var){
  
  img <- dir_ls('../png/maps/raw_climate', regexp = var) %>% as.character()
  img <- map(img, image_read)
  jnd <- image_join(img)
  anm <- magick::image_animate(jnd, fps = 1)
  
  # To write
  image_write(image = anm, path = glue('../gif/raw_{var}.gif'))
  
}

