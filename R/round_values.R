#' Rounding Strategy
#'
#' @param df dataframe
#'
#' @export

round_values <- function(df){

  df <- dplyr::mutate_if(df, is.numeric,
                         ~ ifelse(Age %in% peds_o01, ceiling(.), round(., 0)))

  return(df)
}



