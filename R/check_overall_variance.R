#' Check the variable at the OU level
#'
#' @param df_original original, unadjusted dataset
#' @param df_updated df with updated distribution
#'
#' @export

check_overall_variance <- function(df_original, df_updated) {
  df_o <- df_original %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    tidyr::gather(ind, orig)

  df_u <- df_updated %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    tidyr::gather(ind, new)

  dplyr::full_join(df_o, df_u) %>%
    dplyr::mutate(variance = orig - new,
                  var_pct = scales::percent(1 - (orig/new), .01)) %>%
    knitr::kable(format.args = list(big.mark = ","))


}
