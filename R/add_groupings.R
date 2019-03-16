#' Add PSNU and Age groupsings
#'
#' @param df site tool data frame to apply to
#'
#' @importFrom magrittr %>%
#' @export

add_groupings <- function(df){

  df <- df %>%
    tidyr::separate(Site, c("psnu", "sitename"), sep = " > ", remove = FALSE) %>% #separate out psnu from site for grouping
    dplyr::mutate(psnu = stringr::str_remove(psnu, " \\(.*$")) %>% #clean up psnu name (where undistributed)
    tibble::add_column(ageCoarse = ifelse(df$Age %in% peds, "<15", "15+"), .after = "Age")

  return(df)
}

