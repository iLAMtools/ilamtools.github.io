#' Draw movements
#'
#' This function will circle identified movements/blobs onto their corresponding image files
#' @param pi_sub_folder name of subfolder containing images
#' @param by_change name of dataframe containing movements (name of csv rows much correspond to image names)
#' @param x_left number for crop (must match crop used in find_movements())
#' @param x_right number for crop (must match crop used in find_movements())
#' @param y_bot number for crop (must match crop used in find_movements())
#' @param y_top number for crop (must match crop used in find_movements())
#' @export
#'
plot_movements <- function(file_names,
                           pi_sub_folder, by_change,
                           x_left=0, x_right=2592,
                           y_bot=0, y_top=1944,
                           n_max=75000){

  pi.pix <- file_names[-(1:3)]

  by_change_pi = by_change

  by_change_pi <- by_change_pi %>% dplyr::filter(pi == pi_sub_folder) %>%
    dplyr::filter(time %in% unique(by_change$time)[-(1:3)]) %>%
    dplyr::mutate(s = replace(s, s==2000000 & s >= n_max, NA))


  dir.create(path=paste0("mvmnt_", pi_sub_folder),
             showWarnings = FALSE)

  for (i in 1:length(pi.pix[1:60])){
    par(mfrow=c(1,1))
    jpeg(file=paste0("mvmnt_", pi_sub_folder, "/time", sprintf("%03d", i), ".jpg"),
         width=x_right-x_left, height=1944) #use png() if high-res desired
    load.image(pi.pix[i+1]) %>% #originally this was i+1
      imager::imsub(x %inr% c(x_left,x_right),
            y %inr% c(y_bot,y_top)) %>%
      plot(xlim = c(0,x_right-x_left), ylim = c(0, y_top-y_bot), axes=FALSE)

    title(unique(by_change_pi$time)[i], adj=0.5, line = -3, cex.main = 4)

    #Draw circle standards for blob circle ~12800, 3200, 800, 200, 50 pxs
    draw.circle(75, #x coord
                75, #y coord
                sqrt(12800/pi),
                border = 'green',
                lwd=3)
    draw.circle(175, #x coord
                43, #y coord
                sqrt(3200/pi),
                border = 'green',
                lwd=3)
    draw.circle(227, #x coord
                27, #y coord
                sqrt(800/pi),
                border = 'green',
                lwd=3)
    draw.circle(255, #x coord
                19, #y coord
                sqrt(200/pi),
                border = 'green',
                lwd=3)
    draw.circle(271, #x coord
                15, #y coord
                sqrt(50/pi),
                border = 'green',
                lwd=3)

    by_change_t = by_change_pi %>% dplyr::filter(time == unique(by_change_pi$time)[i])
    for (j in 1:nrow(by_change_t)) {
      draw.circle(by_change_t[j,]$x %>% as.integer(),
                  by_change_t[j,]$y %>% as.integer(),
                  sqrt(by_change_t[j,]$s/pi),
                  border = 'red',
                  lwd=3)
    }

    dev.off()
  }

}
