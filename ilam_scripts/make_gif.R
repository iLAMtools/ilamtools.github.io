library('magick')

make_gif <- function(out_file_name,
                     pi_sub_folder){
  
  gif_images = vector()
  
  for (f in list.files(paste0("./mvmnt_", pi_sub_folder),
                       pattern="*.jpg",
                       full.names = TRUE)) {
    
    gif_images <- append(gif_images, image_read(f))
    
  }
  
  gif <-image_animate(image_scale(gif_images, "900x800"), 
                      fps = 1, dispose = "previous")
  
  image_write(gif, paste0(out_file_name, "_",pi_sub_folder, ".gif"))
  
}
