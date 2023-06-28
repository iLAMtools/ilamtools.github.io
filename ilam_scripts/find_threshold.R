# 2023/02/17 - Jacob Dayton

# pixel difference at n_thr quantile() for 2% random sampling of consecutive image subtractions
# mean value of quantile pixel differences is returned for use in find_movements()

# I wonder about days with minimal to no movement (near end of trial)...? *** select first half of trial?

find_threshold <- function(files,                                       # list of file names
                           p_sample = 0.05,                # percent of images to sample ***
                           n_thr = 0.999,                      # n_threshold integer (e.g., 0.996 == "0.4%" )
                           n_blr = 3,                          # let user select blur radius? ***
                           x_left = 0,                                  # value for crop on x min
                           x_right = 2592,                              # value for crop on x max
                           y_bot = 0,                                   # value for crop on y min
                           y_top = 1944,                                # value for crop on y max
                           channel = "grayscale", # channel value
                           animal = "white"){               # white on black bg or black on white bg
  
  n <- length(files) # total number of images to be analyzed
  
  iterations <- ceiling(p_sample*n)
  pix_diff <- NA # empty vector to hold results 
  
  for (i in 1:iterations){
    pix_sample <- sample((n-1), 1, replace = T) # randomly sample "prev" image *
    
    if (channel == "R" | channel == "red") {
      prev <- 
        load.image(files[pix_sample]) %>% 
        R() %>% isoblur(sigma = n_blr)
      post <- 
        load.image(files[pix_sample+1]) %>%
        R() %>% isoblur(sigma = n_blr)     
    } else if (channel == "G" | channel == "green") {
      prev <- 
        load.image(files[pix_sample]) %>% 
        G() %>% isoblur(sigma = n_blr)
      post <- 
        load.image(files[pix_sample+1]) %>%
        G() %>% isoblur(sigma = n_blr)      
    } else if (channel == "B" | channel == "blue") {
      prev <- 
        load.image(files[pix_sample]) %>% 
        B() %>% isoblur(sigma = n_blr)
      post <- 
        load.image(files[pix_sample+1]) %>%
        B() %>% isoblur(sigma = n_blr)
    } else {
      prev <- 
        load.image(files[pix_sample]) %>% 
        grayscale() %>% isoblur(sigma = n_blr)
      post <- 
        load.image(files[pix_sample+1]) %>%
        grayscale() %>% isoblur(sigma = n_blr)
    }
    
    if (animal == "black") {
      diff <- (prev - post) %>%
        imsub(x %inr% c(x_left, x_right), 
              y %inr% c(y_bot, y_top)) # crop
    } else if (animal == "white") { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
      diff <- (post - prev) %>%
        imsub(x %inr% c(x_left, x_right), 
              y %inr% c(y_bot, y_top))}
    pix_diff[i] <- quantile(diff, n_thr) # pixel difference value for corresponding n_thr quantile
  }
  print("Completed find_threshold")
  return(pix_diff) 
}
