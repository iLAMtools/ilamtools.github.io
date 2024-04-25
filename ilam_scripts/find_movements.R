#' Find movements
#'
#' This function identifies all "blobs" or movements between consecutive images
#' @param files a vector of file names
#' @param x_left a number for leftmost crop boundary
#' @param x_right a number for rightmost crop boundary
#' @param y_bot a number for bottom crop boundary
#' @param y_top a number for top crop boundary
#'
#' @export
#' @examples
#' \dontrun{
#' # out <- find_movements(files = file_names,
#' # n_max = 75000,
#' # find_thr = T,
#' # type_thr = "absolute",
#' # p_sample = 0.2,
#' # channel = "grayscale",
#' # animal = "white")
#' }
find_movements <- function(files,
                           n_thr = 0.996,
                           n_cln = 5,
                           n_grw = 1.5,
                           n_blr = 3,
                           n_max = 75000,
                           x_left = 0,
                           x_right = 2592,
                           y_bot = 0,
                           y_top = 1944,
                           find_thr = T,
                           type_thr = "absolute",
                           p_sample = 0.02,
                           channel = "grayscale",
                           animal = "white") {

  if (find_thr == T & type_thr == "relative") {
    stop("The find_threshold argument only obtains absolute, not relative, threshold values.
               Please select find_thr='T' AND type_thr='absolute'.
               or choose find_thr='F'AND type_thr='relative'")
  }

  if (!requireNamespace("imager", quietly = TRUE)) {
    stop(
      "Package \"imager\" must be installed and loaded to use find_movements().",
      call. = FALSE
    )
  }


  if (find_thr == T & type_thr == "absolute") {
    print("find_thr==T, type_thr==absolute")
    pix_diff <- find_threshold(files=files[1:(0.5*length(files))], p_sample=p_sample,
                               n_blr=n_blr, n_thr=n_thr,
                               x_left=x_left, x_right=x_right,
                               y_bot=y_bot, y_top=y_top,
                               channel=channel, animal=animal)

    n_sample <- length(pix_diff) # sample size
    n_bootstrap <- 1000 # number of bootstrap samples
    bootstrap <- c() # empty vector to hold results

    for (i in 1:n_bootstrap){
      obs <- sample(1:n_sample, replace = T)
      bootstrap[i] <- mean(pix_diff[obs]) # mean of the bootstrap sample
    }

    pix_boot = quantile(bootstrap, 0.99) # 99% value for bootstrap mean ***
    n_thr = pix_boot
    print(paste("Threshold cut-off for absolute pixel difference:", n_thr))
  }

  if (find_thr == F & is.numeric(type_thr) == TRUE) {
    print("find_thr==F, is.numeric(type_thr) == TRUE")
    print("User supplied threshold")
    n_thr = type_thr
  }

  n <- length(files) # number of images we have to look at is called n

  mvmt <- tibble(s = NA,
                         x = NA,
                         y = NA,
                         d = NA)

  blnk <- tibble(s = 0,
                         x = NA,
                         y = NA)

  time_prev <- files[1]

  print("Finding movements now...")

  if (channel == "R" | channel == "red") {
    prev <- load.image(time_prev) %>% imager::R() %>% imager::isoblur(sigma = n_blr)
    i <- 1
    while (i <= (n-1)) {
      time_post <- files[i+1]
      post <- load.image(time_post) %>% imager::R() %>% imager::isoblur(sigma = n_blr)

      if (animal == "black") { # if animal is black on white background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (prev - post) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      } else { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (post - prev) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      }

      if (find_thr == F & type_thr == "relative") {
        pixs <- diff %>% imager::threshold(stringr::str_c(n_thr*100,"%"))
      } else {
        pixs <- (diff > n_thr)
      }

      { setTimeLimit(60) # stops automatically after 30s (it shouldn't take that long)
        pixs <- pixs %>%
          imager::shrink(n_cln) %>%
          imager::grow(n_grw*n_cln) }

      if (sum(pixs) > 0 & sum(pixs) < n_max) { # white pixels of interest must be >0 and <75000 (too noisy)
        splt <- pixs %>% imager::split_connected() # returns discrete blobs of connected pixels
        if (length(splt) > 0) { # number of discrete movements ("blobs") must be > 0
          blbs <- tibble(s = rep(NA, length(splt)),
                                 x = rep(NA, length(splt)),
                                 y = rep(NA, length(splt)),
                                 d = rep(NA, length(splt)))
          for (j in 1:length(splt)) {
            blbs$s[j] <- sum(splt[[j]])
            blbs$x[j] <- imager::where(splt[[j]]) %>% # table of all pixel x- and y-coordinates
              dplyr::summarize(mean(x)) %>%          # takes mean value of column x (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$y[j] <- imager::where(splt[[j]]) %>%
              dplyr::summarize(mean(y)) %>%          # takes mean value of column y (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$d[j] <- time_post
          }
          mvmt <- rbind(mvmt, blbs)
        } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }
      } else if (sum(pixs) >= n_max) { # image is noisy, we assign 2000000 to remove later
        blbs <- tibble(s = 2000000,
                               x = 0,
                               y = 0,
                               d = time_post)
        mvmt <- rbind(mvmt, blbs)
      } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }

      time_prev <- time_post # new time becomes old time
      prev <- post # new photo becomes old photo
      i <- i + 1   # increment i by 1 (part of while loop)
      suppressWarnings(rm(diff, pixs, splt, blbs, post))
    }}
  else if (channel == "G" | channel == "green") {
    prev <- load.image(time_prev) %>% imager::G() %>% imager::isoblur(sigma = n_blr)
    i <- 1
    while (i <= (n-1)) {
      time_post <- files[i+1]
      post <- load.image(time_post) %>% imager::G() %>% imager::isoblur(sigma = n_blr)

      if (animal == "black") { # if animal is black on white background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (prev - post) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      } else { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (post - prev) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      }

      if (find_thr == F & type_thr == "relative") {
        pixs <- diff %>% imager::threshold(stringr::str_c(n_thr*100,"%"))
      } else {
        pixs <- (diff > n_thr)
      }

      { setTimeLimit(60) # stops automatically after 30s (it shouldn't take that long)
        pixs <- pixs %>%
          imager::shrink(n_cln) %>%
          imager::grow(n_grw*n_cln) }

      if (sum(pixs) > 0 & sum(pixs) < n_max) { # white pixels of interest must be >0 and <75000 (too noisy)
        splt <- pixs %>% imager::split_connected() # returns discrete blobs of connected pixels
        if (length(splt) > 0) { # number of discrete movements ("blobs") must be > 0
          blbs <- tibble(s = rep(NA, length(splt)),
                                 x = rep(NA, length(splt)),
                                 y = rep(NA, length(splt)),
                                 d = rep(NA, length(splt)))
          for (j in 1:length(splt)) {
            blbs$s[j] <- sum(splt[[j]])
            blbs$x[j] <- imager::where(splt[[j]]) %>% # table of all pixel x- and y-coordinates
              dplyr::summarize(mean(x)) %>%          # takes mean value of column x (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$y[j] <- imager::where(splt[[j]]) %>%
              dplyr::summarize(mean(y)) %>%          # takes mean value of column y (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$d[j] <- time_post
          }
          mvmt <- rbind(mvmt, blbs)
        } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }
      } else if (sum(pixs) >= n_max) { # image is noisy, we assign 2000000 to remove later
        blbs <- tibble(s = 2000000,
                               x = 0,
                               y = 0,
                               d = time_post)
        mvmt <- rbind(mvmt, blbs)
      } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }

      time_prev <- time_post # new time becomes old time
      prev <- post # new photo becomes old photo
      i <- i + 1   # increment i by 1 (part of while loop)
      suppressWarnings(rm(diff, pixs, splt, blbs, post))
    }
  }
  else if (channel == "B" | channel == "blue") {
    prev <- load.image(time_prev) %>% imager::B() %>% imager::isoblur(sigma = n_blr)
    i <- 1
    while (i <= (n-1)) {
      time_post <- files[i+1]
      post <- load.image(time_post) %>% imager::B() %>% imager::isoblur(sigma = n_blr)

      if (animal == "black") { # if animal is black on white background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (prev - post) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      } else { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (post - prev) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      }

      if (find_thr == F & type_thr == "relative") {
        pixs <- diff %>% imager::threshold(stringr::str_c(n_thr*100,"%"))
      } else {
        pixs <- (diff > n_thr)
      }

      { setTimeLimit(60) # stops automatically after 30s (it shouldn't take that long)
        pixs <- pixs %>%
          imager::shrink(n_cln) %>%
          imager::grow(n_grw*n_cln) }

      if (sum(pixs) > 0 & sum(pixs) < n_max) { # white pixels of interest must be >0 and <75000 (too noisy)
        splt <- pixs %>% imager::split_connected() # returns discrete blobs of connected pixels
        if (length(splt) > 0) { # number of discrete movements ("blobs") must be > 0
          blbs <- tibble(s = rep(NA, length(splt)),
                                 x = rep(NA, length(splt)),
                                 y = rep(NA, length(splt)),
                                 d = rep(NA, length(splt)))
          for (j in 1:length(splt)) {
            blbs$s[j] <- sum(splt[[j]])
            blbs$x[j] <- imager::where(splt[[j]]) %>% # table of all pixel x- and y-coordinates
              dplyr::summarize(mean(x)) %>%          # takes mean value of column x (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$y[j] <- imager::where(splt[[j]]) %>%
              dplyr::summarize(mean(y)) %>%          # takes mean value of column y (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$d[j] <- time_post
          }
          mvmt <- rbind(mvmt, blbs)
        } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }
      } else if (sum(pixs) >= n_max) { # image is noisy, we assign 2000000 to remove later
        blbs <- tibble(s = 2000000,
                               x = 0,
                               y = 0,
                               d = time_post)
        mvmt <- rbind(mvmt, blbs)
      } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }

      time_prev <- time_post # new time becomes old time
      prev <- post # new photo becomes old photo
      i <- i + 1   # increment i by 1 (part of while loop)
      suppressWarnings(rm(diff, pixs, splt, blbs, post))
    }
  }
  else {
    prev <- load.image(time_prev) %>% imager::grayscale() %>% imager::isoblur(sigma = n_blr)
    i <- 1
    while (i <= (n-1)) {
      time_post <- files[i+1]
      post <- load.image(time_post) %>% imager::grayscale() %>% imager::isoblur(sigma = n_blr)

      if (animal == "black") { # if animal is black on white background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (prev - post) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      } else { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
        diff <- (post - prev) %>%
          imager::imsub(x %inr% c(x_left,x_right),
                        y %inr% c(y_bot,y_top)) # crop
      }

      if (find_thr == F & type_thr == "relative") {
        pixs <- diff %>% imager::threshold(stringr::str_c(n_thr*100,"%"))
      } else {
        pixs <- (diff > n_thr)
      }

      { setTimeLimit(60) # stops automatically after 30s (it shouldn't take that long)
        pixs <- pixs %>%
          imager::shrink(n_cln) %>%
          imager::grow(n_grw*n_cln) }

      if (sum(pixs) > 0 & sum(pixs) < n_max) { # white pixels of interest must be >0 and <75000 (too noisy)
        splt <- pixs %>% imager::split_connected() # returns discrete blobs of connected pixels
        if (length(splt) > 0) { # number of discrete movements ("blobs") must be > 0
          blbs <- tibble(s = rep(NA, length(splt)),
                                 x = rep(NA, length(splt)),
                                 y = rep(NA, length(splt)),
                                 d = rep(NA, length(splt)))
          for (j in 1:length(splt)) {
            blbs$s[j] <- sum(splt[[j]])
            blbs$x[j] <- imager::where(splt[[j]]) %>% # table of all pixel x- and y-coordinates
              dplyr::summarize(mean(x)) %>%          # takes mean value of column x (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$y[j] <- imager::where(splt[[j]]) %>%
              dplyr::summarize(mean(y)) %>%          # takes mean value of column y (coordinate)
              as.numeric() %>%              # extracts numerical value from mini-table
              round(0) #***
            blbs$d[j] <- time_post
          }
          mvmt <- rbind(mvmt, blbs)
        } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }
      } else if (sum(pixs) >= n_max) { # image is noisy, we assign 2000000 to remove later
        blbs <- tibble(s = 2000000,
                               x = 0,
                               y = 0,
                               d = time_post)
        mvmt <- rbind(mvmt, blbs)
      } else { mvmt <- rbind(mvmt, cbind(blnk, d = time_post)) }

      time_prev <- time_post # new time becomes old time
      prev <- post # new photo becomes old photo
      i <- i + 1   # increment i by 1 (part of while loop)
      suppressWarnings(rm(diff, pixs, splt, blbs, post))
    }}

  suppressWarnings(rm(blnk, blbs, diff, time_prev, time_post))
  mvmt$d <- sub("\\.[[:alnum:]]+$", "", basename(as.character(mvmt$d)))
  mvmt = mvmt[-1,]

  return(mvmt)
}
