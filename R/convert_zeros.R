#' Convert Zeros to NA
#'
#' @param df dataframe
#'
#' @export

convert_zeros <- function(df){

  df <- dplyr::mutate_if(df, is.numeric, ~ ifelse((is.finite(.) & . !=0), ., NA))

  return(df)
}
