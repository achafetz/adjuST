#' Rounding Strategy
#'
#' @param df dataframe
#' @param ... indicators to round
#'
#' @export

round_values <- function(df, ...){

  inds <- dplyr::enquos(...)

  df <- dplyr::mutate_at(df, dplyr::vars(!!!inds),
                         ~ ifelse(Age %in% peds_o01, ceiling(.), round(., 0)))

  return(df)
}



