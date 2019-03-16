#' Export New Targets to Excel
#'
#' @param df data frame with new targest to export
#' @param wb workbook name, must exist prior to creation, eg `wb <- openxlsx::createWorkbook()`
#' @param tab_name tab to add
#' @param filepath filepath, default = "out/"
#'
#' @export

write_excel <- function(df, wb, tab_name, filepath = "out/"){

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, tab_name)

  openxlsx::writeData(wb, tab_name, tab_name)
  openxlsx::writeData(wb, tab_name, df_tb_stat_clean, startRow = 5)

  filename <- paste0("COP19_TZA_SiteTool_Target_Redistribution_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  openxlsx::saveWorkbook(wb, file.path("out", filename), overwrite = TRUE)

}
