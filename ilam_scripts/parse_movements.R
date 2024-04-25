#' Parse movements
#'
#' Parse .csv containing the movements output by find_movement()
#' @param file_mvmnts a csv containing dataframe output by find_movement()
#' @param start_photophase a number for the time that photophase started in standard time
#' @param end_photophase a number for the time that photophase ended in standard time
#' @export
#' @examples
#' \dontrun{
#' # by_change <- parse_movements(file_mvmnts = paste0("Onub_fhlbnd_202308",".csv"),
#' # start_photophase = 2,
#' # end_photophase = 14)
#' }

#Function to parse .csv listing the movements identified by find_movement()
#Requires file name, starting hour of photophase, ending hour of photophase

parse_movements <- function(file_mvmnts,
                            start_photophase,
                            end_photophase){
  readr::read_csv(file_mvmnts,
           col_types = cols(s = col_number(),
                            x = col_number(),
                            y = col_number())) %>%
    tidyr::separate(d, c("pi", "date", "est"), sep="\\.") %>%
    subset(is.na(pi) == F) %>%
    dplyr::mutate(sec = "00", year = "2022") %>%
    tidyr::separate(est, sep = -2, into = c("hour", "min")) %>%
    tidyr::separate(date, sep = -2, into = c("month", "day")) %>%
    tidyr::unite("hms", c(hour, min, sec), sep = ":") %>%
    tidyr::unite("ymd", c(year, month, day), sep = ".") %>%
    tidyr::unite("time", c(ymd, hms), sep = " ") %>%
    dplyr::mutate(time = lubridate::ymd_hms(time)) %>%
    dplyr::mutate(treatment =
             ifelse(lubridate::hour(time) >= start_photophase &
                    lubridate::hour(time) < end_photophase,
                    "L", "D"))}

