#' Excel Spreadsheet for Public Download of RTP Monitoring Data
#'
#' This function creates a Microsoft Excel spreadsheet for public download from the RTP Monitoring dashboard.
#' Each tab contains a single table of data from the dashboard.
#'
#' @param table_list List of data tables and spreadsheet tab names from the dashboard.
#' @param output_path File path for saving the Excel workbook.
#' @return spreadsheet of RTP Monitoring data
#' 
#' @export
#'
create_public_spreadsheet <- function(table_list, output_path) {
  
  hs <- openxlsx::createStyle(
    fontColour = "black",
    border = "bottom",
    fgFill = "#00a7a0",
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )
  
  table_idx <- 1
  sheet_idx <- 2
  
  wb <- openxlsx::loadWorkbook(system.file("extdata", "metadata.xlsx", package = "psrcrtp"))
  
  for (i in table_list) {
    for (j in names(table_list)) {
      if (names(table_list)[table_idx] == j) {
        
        openxlsx::addWorksheet(wb, sheetName = j)
        openxlsx::writeDataTable(wb, sheet = sheet_idx, x = i, tableStyle = "none", headerStyle = hs, withFilter = FALSE)
        openxlsx::setColWidths(wb, sheet = sheet_idx, cols = 1:length(i), widths = "auto")
        openxlsx::freezePane(wb, sheet = sheet_idx, firstRow = TRUE)
        
      } else {next}
    }
    if (table_idx < length(table_list)) {
      
      table_idx <- table_idx + 1
      sheet_idx <- sheet_idx + 1
      
    } else {break}
  }
  
  openxlsx::saveWorkbook(wb, file = output_path)
  
}
