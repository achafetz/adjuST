#' Apply Distribution to
#'
#' @param df dataframe to apply distributions on
#' @param df_distro distribution dataframe created with `gen_distro()`
#' @param ... variables to apply the distribution to
#'
#' @importFrom magrittr %>%
#' @export

apply_distro <- function(df, df_distro, ...){

  indicator_quo <- dplyr::enquos(...)

  #suppressMessages(
  df_join <- dplyr::left_join(df, df_distro)
  #)
  df_join <- df_join %>%
    dplyr:: group_by(psnu, Mechanism, sitename, ageCoarse) %>%
    dplyr::mutate_at(dplyr::vars(!!!indicator_quo), ~ distro * sum(., na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-distro)


  return(df_join)
}
