#' Make DAM file
#'
#' Converts by_change input to DAM format (TriKinetics Drosophila Activity Monitor)
#' @param by_frame a data frame
#' @param variable_name a character indicating whether file should contain "s" (sum of movements) or "n" (total number of movements/blobs)
#' @export
#' @examples
#' # DAM_by_frame = make_dam_file(by_frame_input, variable_name = "s")
#'
make_dam_file <- function(by_frame, #data frame to use
                          variable_name = "s"){

  dam_output <- tibble::tibble(date = by_frame$time,
                               status = 1, #1=valid data, 51=no data received
                               extras = 0,
                               monitor = 42, #monitor number
                               tube_no = 0,
                               data = "Ct", #counts total
                               blank = 0, #unused
                               sensor = ifelse(by_frame$treatment=="L",1,0), #1=lights-on, 0=lights-off
                               version = by_frame$pi,
                               counts = dplyr::pull(by_frame, variable_name)) %>%
    tidyr::pivot_wider(names_from = version,
                       values_from = counts,
                       values_fill = 0) %>%
    tidyr::separate(date, sep = -8, into = c("date","time")) %>%
    tidyr::separate(date, into = c(NA, NA, "day"), extra = "drop") %>%
    tibble::add_column(index = 1:(nrow(by_frame)/length(unique(by_frame$pi))), .before = "day") %>%
    tibble::add_column(month = stringr::str_sub(months(unique(by_frame$time)), 0, 3),
                       year = stringr::str_sub(lubridate::year(unique(by_frame$time)),-2), .before = "time") %>%
    tidyr::unite("date", day:year, sep = " ")

  dam_output <- add_columns_to_42(dam_output)

  return(dam_output)
}
