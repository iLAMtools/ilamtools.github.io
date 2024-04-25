#' Add columns to 42
#'
#' Internal function required by make_dam_file.R. Fills empty monitor value to reach 32 monitor columns required by DAM format (TriKinetics Drosophila Activity Monitor)
#' @param df a data frame
#' @export
add_columns_to_42 <- function(df) {
  current_ncol <- ncol(df)
  while (current_ncol < 42) {
    new_col_name <- paste0("X", current_ncol + 1 - 10)
    df <- df %>% dplyr::mutate(!!new_col_name := 0)
    current_ncol <- current_ncol + 1
    }
  return(df)
  }
