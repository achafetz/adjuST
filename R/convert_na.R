#' Convert NAs to Zeros
#'
#' @param df dataframe
#'
#' @export

convert_na <- function(df){

  df <- dplyr::mutate_if(df, is.numeric, ~ ifelse(is.na(.), 0, .))

  return(df)
}
