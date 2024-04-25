#' Filter to number of blobs
#'
#' Converts a "by_change" input and outputs a data frame containing total blobs/movements to be used as input for make_dam_file()
#'
#' @export
filter_nblobs <- function(by_change) { #data frame to use

    compare_df <- dplyr::filter(by_change, s>0 & s<2000000 & !is.na(s)) %>% #use group_by("pi")
    dplyr::summarise(blob_size = round(mean(s, na.rm=TRUE),0),
              count = n(),
              sd = round(sd(s),0),
              lb = round(blob_size-1*sd, 0),
              ub = round(blob_size+2*sd, 0))

  by_change = by_change %>%
    dplyr::cross_join(compare_df) %>% #use left_join(by = pi)
    dplyr::mutate(s = replace(s, s>0 & s <= lb, NA)) %>%
    dplyr::mutate(s = replace(s, s!=2000000 & s >= ub, NA))

  by_frame <-
    by_change %>% dplyr::ungroup() %>%
    dplyr::group_by(pi, ID, time, treatment) %>%
    dplyr::summarize(n = length(s[!is.na(s)]), #number of blobs of size (s) != NA
              s = sum(s, na.rm = TRUE)) %>% #sum of blob sizes, NA removed
    dplyr::mutate(n = ifelse(s == 0, 0, n)) %>% #if sum of blobs=0, then n<-0 (otherwise it'd be 1)
    dplyr::distinct(pi, ID, time, treatment, n, s) #sanity check to remove any duplicates

  by_frame <-
    by_frame %>% dplyr::ungroup() %>%
    dplyr::mutate(s = replace(s, s==2000000, NA),
           n = replace(n, is.na(s), NA))

  by_frame <-
    by_frame %>% dplyr::group_by(pi) %>%
    dplyr::mutate(s = round((na.locf0(s, fromLast = TRUE) + zoo::na.locf0(s, fromLast = FALSE))/2,0),
           n = round((zoo::na.locf0(n, fromLast = TRUE) + zoo::na.locf0(n, fromLast = FALSE))/2,0)) %>% ungroup()

  by_frame <-
    by_frame %>% dplyr::ungroup() %>%
    dplyr::mutate(s = replace(s, is.na(s), 0),
           n = replace(n, is.na(n), 0))

  return(by_frame)
 }
