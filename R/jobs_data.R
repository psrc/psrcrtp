#' Total Employment Growth Near High Capacity Transit
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' VISION 2050 Station Area and RGC buffers are stored in ElmerGeo.
#' 
#' @param start_year The first year of data in the series. Defaults to 2010.
#' @return tibble of total jobs near High Capacity Transit by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples 
#' 
#' emp_hct <- employment_near_hct()}
#'  
#' @export
#'
employment_near_hct <- function(start_year = 2010) {
  
  db_conn <- DBI::dbConnect(odbc::odbc(),
                            driver = "SQL Server",
                            server = "AWS-PROD-SQL\\Sockeye",
                            database = "Elmer",
                            trusted_connection = "yes")
  
  tbl <- DBI::Id(schema = "employment", table = "hct_station_areas_employment")
  
  print("Pulling total employment estimates from Elmer.")
  
  processed <- DBI::dbReadTable(db_conn, tbl) %>% 
    dplyr::filter(.data$data_year >= start_year) %>% 
    dplyr::transmute(geography = "Region",
                     geography_type = "PSRC Region",
                     metric = "Employment Inside HCT Area",
                     date = lubridate::ymd(paste0(.data$data_year,"-03-01")),
                     estimate = .data$emp_in_hct,
                     share = .data$emp_in_hct / .data$total_emp)
  
  print("All done.")
  return(processed)
  
}
