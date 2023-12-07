#' Total Employment Growth Near High Capacity Transit
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' VISION 2050 Station Area and RGC buffers are stored in ElmerGeo.
#' 
#' @param start_year The first year of data in the series. Defaults to 2010.
#' @return tibble of total jobs near High Capacity Transit and in the region by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' jobs_hct <- jobs_near_hct()}
#'  
#' @export
#'
jobs_near_hct <- function(start_year = 2010) {
  
  region <- psrcelmer::get_table(schema='employment', tbl_name='hct_station_areas_employment') |>
    dplyr::filter(.data$data_year >= start_year) |>
    dplyr::filter(.data$geo %in% c("Inside HCT Area", "Region")) |>
    tidyr::pivot_wider(names_from = "geo", values_from = "total_emp") |>
    dplyr::rename(`in station area` = "Inside HCT Area") |>
    dplyr::mutate(`not in station area` = .data$Region - .data$`in station area`) |>
    tidyr::pivot_longer(cols = !c("data_year", "Region"), names_to = "variable", values_to = "jobs") |>
    dplyr::select(-"Region") |>
    dplyr::arrange(.data$variable, .data$data_year) |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(job_growth = .data$jobs - dplyr::lag(.data$jobs)) |>
    tidyr::pivot_longer(cols = !c("data_year", "variable"), names_to = "metric", values_to = "estimate") |>
    tidyr::drop_na() |>
    dplyr::mutate(grouping = dplyr::case_when(
      stringr::str_detect(.data$metric, "growth") ~ "Change",
      !(stringr::str_detect(.data$metric, "growth")) ~ "Total"))|>
    dplyr::mutate(metric = "Jobs near HCT") |>
    dplyr::rename(year = "data_year")
  
  totals <- region |>
    dplyr::group_by(.data$year, .data$metric, .data$grouping) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  region <- dplyr::left_join(region, totals, by=c("year", "metric", "grouping")) |>
    dplyr::mutate(share = .data$estimate / .data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(year = as.character(.data$year), geography="Region", geography_type="Region", date=lubridate::mdy(paste0("04-01-", .data$year))) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  return(region)
  
}


#' Total Observed and Forecast Employment Growth
#'
#' This function pulls total employment estimates from PSRC's employment database in Elmer and the Macroforecast.
#' The jobs estimates are already summarized by year in a table in Elmer.
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @param first_year Four digit integer for first year of data to use from Elmer - defaults to 2010
#' @return tibble of total jobs in the region by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' jobs <- jobs_data()}
#'  
#' @export
#'
jobs_data <- function(forecast_base_yr=2018, first_year = 2010){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  print(stringr::str_glue("Getting Historic Jobs Data"))
  o <- psrcrtp::jobs_near_hct(start_year = first_year) |>
    dplyr::group_by(.data$year, .data$date, .data$geography, .data$geography_type, .data$grouping, .data$metric) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable="Observed", metric="Jobs") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  print(stringr::str_glue("Getting Jobs Forecast Data"))
  # Observed Data up to Forecast Base Year
  fo <- o |> dplyr::mutate(variable="Forecast") |> dplyr::filter(.data$year <= forecast_base_yr)
  
  # Get Forecast Population Growth from Elmer
  fj <- psrcelmer::get_table(schema='Macroeconomic', tbl_name='employment_facts') |>
    dplyr::filter(.data$employment_sector_dim_id==8 & .data$data_year >= forecast_base_yr-1) |>
    dplyr::rename(estimate="jobs", year="data_year") |>
    dplyr::mutate(date = lubridate::ymd(paste0(.data$year,"-04-01"))) |>
    dplyr::mutate(geography="Region", geography_type="Region", variable="Forecast", metric="Jobs") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "metric", "estimate") |>
    dplyr::mutate(change = (.data$estimate- dplyr::lag(.data$estimate))) |>
    tidyr::drop_na() |>
    tidyr::pivot_longer(cols = c("estimate", "change"), names_to = "grouping", values_to = "estimate") |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "change", "Change")) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "estimate", "Total")) |>
    dplyr::mutate(year = as.character(.data$year)) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  f <- dplyr::bind_rows(o, fo, fj)
  
  print("All Done")
  return(f)
}