#' Generate Variable distribution across grouping vars
#'
#' @param df dataframe to create distributions on
#' @param indicator variable to aggregate
#' @param ... grouping variables to sum by (Age & Sex removed from distro)
#'
#' @importFrom magrittr %>%
#' @export

gen_distro <- function(df, indicator, ...){

  indicator_quo <- dplyr::enquo(indicator)
  indicator_enquo <- dplyr::enquos(indicator)
  group_var <- dplyr::enquos(...)


  df <- df %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarize_at(dplyr::vars(!!!indicator_enquo), sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(psnu, Mechanism, ageCoarse) %>%
    dplyr::mutate(distro = !!indicator_quo / sum(!!indicator_quo)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-!!indicator_quo)

  return(df)
}
