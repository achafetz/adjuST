#' Aggregate by grouping variables
#'
#' @param df dataframe to create distributions on
#' @param indicator variable to aggregate
#' @param ... grouping variables to sum by (Age & Sex removed from distro)
#'
#' @importFrom magrittr %>%
#' @export

aggr_group <- function(df, indicator, ...){

  indicator_enquo <- dplyr::enquos(indicator)
  group_var <- dplyr::enquos(...)

  df <- df %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarize_at(dplyr::vars(!!!indicator_enquo), sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  return(df)
}
