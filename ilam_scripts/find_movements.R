# identifies all blobs (movements) given a set of images, threshold %, and cleaning value
# returns a dataframe with details (size, x- & y-coordinates) of movement in each frame

# 2023/02/20 - Jacob Dayton 
# now includes method for global thresholding across all image comparisons

find_movements <- function(files,              # list of file names 
                          n_thr = 0.996,      # threshold value (0.992 == "0.8%")
                          n_cln = 5,          # value for cleaning (number of pixels)
                          n_grw = 1.5,        # multiplier for n_cln (shrink vs. grow)
                          n_blr = 3,          # let user select blur radius? ***** check this
                          n_max = 75000,      # upper cut-off for # pixel differences
                          x_left = 0,         # value for crop on x min
                          x_right = 2592,     # value for crop on x max
                          y_bot = 0,          # value for crop on y min
                          y_top = 1944,       # value for crop on y max
                          find_thr = T,                   # make this part optional? ***
                          type_thr = "absolute",
                          p_sample = 0.02,                            # percent of images to sample ***
                          channel = "grayscale",
                          animal = "white") { 
  
  if (find_thr == T & type_thr == "relative") {
    stop("The find_threshold argument only obtains absolute, not relative, threshold values.
               Please select find_thr='T' AND type_thr='absolute'.
               or choose find_thr='F'AND type_thr='relative'")
  }
  
  if (find_thr == T & type_thr == "absolute") {
    print("find_thr==T, type_thr==absolute")
    #return vector containing the n_thr quantile value of pixel differences between a random sample
    #of consecutive image substractions from the first 50% of the experiment (i.e., when movements are high)
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
  
  #to speed up, this should be re-written so that boolean for channel == "*" does not need to be evaluated every time
  if (channel == "R" | channel == "red") {
    prev <- load.image(time_prev) %>% R() %>% isoblur(sigma = n_blr)
  } else if (channel == "G" | channel == "green") { prev <- load.image(time_prev) %>% G() %>% isoblur(sigma = n_blr)
  } else if (channel == "B" | channel == "blue") { prev <- load.image(time_prev) %>% B() %>% isoblur(sigma = n_blr)
  } else { prev <- load.image(time_prev) %>% grayscale() %>% isoblur(sigma = n_blr) } 
  
  i <- 1
  
  while (i <= (n-1)) {
    time_post <- files[i+1]
    if (channel == "R" | channel == "red") { post <- load.image(time_post) %>% R() %>% isoblur(sigma = n_blr)
    } else if (channel == "G" | channel == "green") { post <- load.image(time_post) %>% G() %>% isoblur(sigma = n_blr)
    } else if (channel == "B" | channel == "blue") { post <- load.image(time_post) %>% B() %>% isoblur(sigma = n_blr)
    } else { post <- load.image(time_post) %>% grayscale() %>% isoblur(sigma = n_blr) } 
    
    if (animal == "black") { # if animal is black on white background, white pixels are PRESENT in post and ABSENT in prev
      diff <- (prev - post) %>%
        imsub(x %inr% c(x_left,x_right), 
              y %inr% c(y_bot,y_top)) # crop
    } else { # if animal is white on black background, white pixels are PRESENT in post and ABSENT in prev
      diff <- (post - prev) %>%
        imsub(x %inr% c(x_left,x_right), 
              y %inr% c(y_bot,y_top)) # crop
    } 
    
    if (find_thr == F & type_thr == "relative") {
      pixs <- diff %>% threshold(str_c(n_thr*100,"%"))
    } else {
      pixs <- (diff > n_thr)
    }
    
    { setTimeLimit(60) # stops automatically after 30s (it shouldn't take that long)
      pixs <- pixs %>%
        shrink(n_cln) %>%
        grow(n_grw*n_cln) }
    
    if (sum(pixs) > 0 & sum(pixs) < n_max) { # white pixels of interest must be >0 and <75000 (too noisy)
      splt <- pixs %>% split_connected() # returns discrete blobs of connected pixels
      if (length(splt) > 0) { # number of discrete movements ("blobs") must be > 0
        blbs <- tibble(s = rep(NA, length(splt)), 
                       x = rep(NA, length(splt)), 
                       y = rep(NA, length(splt)),
                       d = rep(NA, length(splt)))
        for (j in 1:length(splt)) {
          blbs$s[j] <- sum(splt[[j]])
          blbs$x[j] <- imager::where(splt[[j]]) %>% # table of all pixel x- and y-coordinates
            summarize(mean(x)) %>%          # takes mean value of column x (coordinate)
            as.numeric() %>%              # extracts numerical value from mini-table
            round(0) #***
          blbs$y[j] <- imager::where(splt[[j]]) %>% 
            summarize(mean(y)) %>%          # takes mean value of column y (coordinate)
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
  suppressWarnings(rm(blnk, blbs, diff, time_prev, time_post)) 
  mvmt$d <- sub("\\.[[:alnum:]]+$", "", basename(as.character(mvmt$d)))
  mvmt = mvmt[-1,]
  # remove rows that contain balls imager::where s==NA
  #mvmt <- mvmt[-which(is.na(mvmt$s)),] 
  # replace "noisy" values in s (2,000,000) with NA
  #mvmt$s[which(mvmt$s==2000000)] <- NA
  
  return(mvmt)
}
