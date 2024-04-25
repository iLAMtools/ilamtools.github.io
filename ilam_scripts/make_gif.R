#' Make gif
#'
#' This function will make a gif of all images contained with ./mvmnt_[subfolder]
#' @param out_file_name the name of the output gif
#' @param pi_sub_folder the name of the folder within ./mvmnt_[subfolder]
#' @export
#' @examples
#' # make_gif(out_file_name, pi_sub_folder)
#'
make_gif <- function(out_file_name,
                     pi_sub_folder){

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop(
      "Package \"magick\" must be installed and loaded to use this make_gif() function.",
      call. = FALSE
    )
  }

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
