#' Excel Spreadsheet for Public Download of RTP Monitoring Data
#'
#' This function creates a Microsoft Excel spreadsheet for public download from the RTP Monitoring dashboard.
#' Each tab contains a single table of data from the dashboard.
#'
#' @param table_list List of data tables and spreadsheet tab names from the dashboard.
#' @return spreadsheet of RTP Monitoring data
#' 
#' @export
#'
create_public_spreadsheet <- function(table_list) {
  
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
  
  wb <- openxlsx::loadWorkbook("metadata.xlsx")
  
  for (i in table_list) {
    for (j in names(table_list)) {
      if (names(table_list)[table_idx] == j) {
        
        #if (exists("share", where = i)) {class(i$share) <- c(class(i$share), "percentage")}  # doesn't work?
        
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
  
  #openxlsx::saveWorkbook(wb, file = )
  
}
