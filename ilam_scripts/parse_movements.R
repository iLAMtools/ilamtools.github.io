#Function to parse .csv listing the movements identified by find_movement()
#Requires file name, starting hour of photophase, ending hour of photophase

parse_movements <- function(file_mvmnts,
                            start_photophase,
                            end_photophase){
  read_csv(file_mvmnts, 
           col_types = cols(s = col_number(),
                            x = col_number(),
                            y = col_number())) %>%
    separate(d, c("pi", "date", "est"), sep="\\.") %>%
    subset(is.na(pi) == F) %>%
    mutate(sec = "00", year = "2022") %>%
    separate(est, sep = -2, into = c("hour", "min")) %>%
    separate(date, sep = -2, into = c("month", "day")) %>%
    unite("hms", c(hour, min, sec), sep = ":") %>%
    unite("ymd", c(year, month, day), sep = ".") %>%
    unite("time", c(ymd, hms), sep = " ") %>%
    mutate(time = ymd_hms(time)) %>%
    mutate(treatment = 
             ifelse(hour(time) >= start_photophase & 
                    hour(time) < end_photophase,
                    "L", "D"))}

