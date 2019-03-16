#' Match original columns of the Site Tool
#'
#' @param df adjusted dataframe
#'
#' @export

match_orig <- function(df){

  df <- dplyr::select(df, -dplyr::matches("^[a-z].*", ignore.case = FALSE))

  return(df)
}
