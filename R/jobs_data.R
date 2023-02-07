#' Total Employment Growth Near High Capacity Transit
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' VISION 2050 Station Area and RGC buffers are stored in ElmerGeo.
#' 
#' @param start_year The first year of data in the series. Defaults to 2010.
#' @return tibble of total jobs near High Capacity Transit and in the region by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples 
#' emp_hct <- employment_near_hct()
#'  
#' @export
#'
employment_near_hct <- function(start_year = 2010) {
  
  db_conn <- DBI::dbConnect(odbc::odbc(),
                            driver = "SQL Server",
                            server = "AWS-PROD-SQL\\Sockeye",
                            database = "Elmer",
                            trusted_connection = "yes")
  
  print("Pulling total employment estimates from Elmer.")
  
  region <- DBI::dbGetQuery(db_conn,
                            "SELECT * FROM employment.hct_station_areas_employment
                             WHERE geo IN ('Inside HCT Area', 'Region')") %>% 
    dplyr::filter(.data$data_year >= start_year) %>% 
    tidyr::pivot_wider(names_from = .data$geo,
                       values_from = .data$total_emp) %>% 
    dplyr::mutate(share = .data$`Inside HCT Area` / .data$`Region`) %>% 
    tidyr::pivot_longer(cols = c(.data$`Inside HCT Area`,
                                 .data$`Region`),
                        names_to = "variable",
                        values_to = "estimate") %>% 
    dplyr::transmute(geography = "Region",
                     geography_type = "PSRC Region",
                     variable = .data$variable,
                     metric = "Total Employment",
                     date = lubridate::ymd(paste0(.data$data_year,"-03-01")),
                     estimate = .data$estimate,
                     share = ifelse(.data$variable == "Region", 1, .data$share))
  
  county <- DBI::dbGetQuery(db_conn,
                            "SELECT * FROM employment.hct_station_areas_employment
                             WHERE geo IN ('33', '35', '53', '61')") %>% 
    dplyr::filter(.data$data_year >= start_year) %>% 
    dplyr::transmute(geography = dplyr::case_when(.data$geo == "33" ~ "King",
                                                  .data$geo == "35" ~ "Kitsap",
                                                  .data$geo == "53" ~ "Pierce",
                                                  .data$geo == "61" ~ "Snohomish"),
                     geography_type = "County",
                     metric = "Total Employment",
                     date = lubridate::ymd(paste0(.data$data_year,"-03-01")),
                     estimate = .data$total_emp)
  
  processed <- dplyr::bind_rows(region, county)
  
  processed <- processed[, c("geography", "geography_type", "variable", "metric", "date", "estimate", "share")]
  
  print("All done.")
  return(processed)
  
}

#' Total Observed and Forecast Employment Growth
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer and the Macroforecast.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @return tibble of total jobs in the region by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples 
#' forecast_jobs <- jobs_forecast_data()
#'  
#' @export
#'
jobs_forecast_data <- function(forecast_base_yr=2018){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  print("Getting Histroic Jobs Data")
  oj <- psrcrtp::employment_near_hct() %>%
    dplyr::filter(.data$variable=="Region") %>%
    dplyr::mutate(grouping="Total Employment", geography_type="PSRC Region", share=1, variable="Total", metric="Observed Employment")
  
  # Annual Observed Employment Change
  oc <- oj %>%
    dplyr::group_by(.data$geography) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), variable="Change") %>% 
    tidyr::drop_na()
  
  o <- dplyr::bind_rows(oj, oc)
  rm(oj,oc)
  
  print("Getting Jobs Forecast Data")
  # Observed Data up to Forecast Base Year
  fo <- o %>%
    dplyr::mutate(metric="Forecast Employment") %>%
    dplyr::filter(lubridate::year(.data$date) <= forecast_base_yr)
  
  # Get Forecast Population Growth from Elmer
  fj <- psrctrends::get_elmer_table("Macroeconomic.employment_facts") %>%
    dplyr::filter(.data$employment_sector_dim_id==8 & .data$data_year >= forecast_base_yr) %>%
    dplyr::rename(estimate="jobs") %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$data_year,"-04-01"))) %>%
    dplyr::mutate(metric="Forecast Employment") %>% 
    dplyr::select("date", "estimate", "metric") %>%
    dplyr::mutate(geography="Region", grouping="Total Employment", geography_type="PSRC Region", share=1, variable="Total") 
  
  # Annual Forecast Employment Change
  fc <- fj %>%
    dplyr::group_by(.data$geography) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), variable="Change") %>% 
    tidyr::drop_na()
  
  fj <- fj %>%
    dplyr::filter(lubridate::year(.data$date) > forecast_base_yr)
  
  f <- dplyr::bind_rows(o, fo, fj, fc)
  rm(o, fo, fj, fc)
  
  print("All Done")
  return(f)
}