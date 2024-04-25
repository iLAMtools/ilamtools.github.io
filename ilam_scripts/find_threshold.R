#' Find pixel difference cutoff for thresholding
#'
#' This function is called within find_movements()
#' @export
#' @examples
#' \dontrun{
#'  pix_diff <- find_threshold(files=files[1:(0.5*length(files))],
#'  p_sample=p_sample,
#'  n_blr=3,
#'  n_thr=0.998,
#'  x_left=x_left, x_right=x_right,
#'  y_bot=y_bot, y_top=y_top,
#'  channel=channel, animal=animal)
#'  }

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
        imager::load.image(files[pix_sample]) %>%
        imager::R() %>% imager::isoblur(sigma = n_blr)
      post <-
        imager::load.image(files[pix_sample+1]) %>%
        imager::R() %>% imager::isoblur(sigma = n_blr)
    } else if (channel == "G" | channel == "green") {
      prev <-
        imager::load.image(files[pix_sample]) %>%
        imager::G() %>% imager::isoblur(sigma = n_blr)
      post <-
        imager::load.image(files[pix_sample+1]) %>%
        imager::G() %>% imager::isoblur(sigma = n_blr)
    } else if (channel == "B" | channel == "blue") {
      prev <-
        imager::load.image(files[pix_sample]) %>%
        imager::B() %>% imager::isoblur(sigma = n_blr)
      post <-
        imager::load.image(files[pix_sample+1]) %>%
        imager::B() %>% imager::isoblur(sigma = n_blr)
    } else {
      prev <-
        imager::load.image(files[pix_sample]) %>%
        imager::grayscale() %>% imager::isoblur(sigma = n_blr)
      post <-
        imager::load.image(files[pix_sample+1]) %>%
        imager::grayscale() %>% imager::isoblur(sigma = n_blr)
    }

    if (animal == "black") {
      diff <- (prev - post) %>%
        imager::imsub(x %inr% c(x_left, x_right),
                      y %inr% c(y_bot, y_top)) # crop
      pix_diff[i] <- quantile(diff, n_thr) # pixel difference value for corresponding n_thr quantile
    } else if (animal == "white") { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
      diff <- (post - prev) %>%
        imager::imsub(x %inr% c(x_left, x_right),
                      y %inr% c(y_bot, y_top))}
    pix_diff[i] <- quantile(diff, n_thr) # pixel difference value for corresponding n_thr quantile
  }
  print("Completed find_threshold")
  return(pix_diff)
}
