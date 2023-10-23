#' Total Vehicle Registrations by Electrification Level
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is pulled from https://data.wa.gov/ via the Socrata API
#' 
#' @return tibble of total vehicle registrations by electrification level by month and county
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' total_vehicle_registrations <- all_registrations()}
#' 
#' @export
#'
all_ev_registrations <- function () {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  psrc_token <- "tjnJfQzL0SfZJ1cbT0iiCUpO3"
  df <- dplyr::as_tibble(RSocrata::read.socrata("https://data.wa.gov/resource/3d5d-sdqb.json", app_token = psrc_token)) |>
    dplyr::filter(.data$state %in% c("WA") & .data$county %in% c("King", "Kitsap", "Pierce", "Snohomish")) |>
    dplyr::select(-"state", -"vehicle_primary_use", -"percent_electric_vehicles") |>
    dplyr::rename(bev="battery_electric_vehicles_bevs_") |>
    dplyr::rename(phev="plug_in_hybrid_electric_vehicles_phevs_") |>
    dplyr::rename(ev="electric_vehicle_ev_total") |>
    dplyr::rename(non_ev = "non_electric_vehicles") |>
    dplyr::rename(geography = "county") |>
    dplyr::mutate(bev=as.numeric(.data$bev), phev=as.numeric(.data$phev)) |>
    dplyr::mutate(ev=as.numeric(.data$ev), non_ev=as.numeric(.data$non_ev)) |>
    dplyr::mutate(total_vehicles=as.numeric(.data$total_vehicles)) |>
    dplyr::group_by(.data$date, .data$geography) |>
    dplyr::summarise(bev=sum(.data$bev), phev=sum(.data$phev), ev=sum(.data$ev), non_ev=sum(.data$non_ev), total_vehicles=sum(.data$total_vehicles)) |>
    dplyr::as_tibble() 
  
  region <- df |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(bev=sum(.data$bev), phev=sum(.data$phev), ev=sum(.data$ev), non_ev=sum(.data$non_ev), total_vehicles=sum(.data$total_vehicles)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(geography = "Region")
  
  df <- dplyr::bind_rows(df, region)
  
  share <- df |>
    dplyr::mutate(bev_share = .data$bev / .data$total_vehicles) |>
    dplyr::mutate(phev_share = .data$phev / .data$total_vehicles) |>
    dplyr::mutate(ev_share = .data$ev / .data$total_vehicles) |>
    dplyr::mutate(non_ev_share = .data$non_ev / .data$total_vehicles) |>
    dplyr::mutate(total_vehicles_share = 1) |>
    dplyr::select(-"bev", -"phev", -"ev", -"non_ev", -"total_vehicles") |>
    tidyr::pivot_longer(!c("date", "geography"), names_to = "variable", values_to = "share") |>
    dplyr::mutate(variable = stringr::str_remove_all(.data$variable, "_share"))
  
  totals <- df |>
    tidyr::pivot_longer(!c("date", "geography"), names_to = "variable", values_to = "estimate")
  
  final_data <- dplyr::left_join(totals, share, by=c("date", "geography", "variable")) |>
    dplyr::mutate(geography_type = "County", grouping = "All", metric = "total-vehicle-registrations") |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "total_vehicles", "Total Vehicles")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "bev", "Battery Electric")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "phev", "Plug-In Hybrid")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "non_ev", "Internal Combustion")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "ev", "Total Electric Vehicles")) |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate", "share")
  
  return(final_data)
  
}

#' New Vehicle Registrations by Electrification Level
#'
#' This cleans data from the Washington State Registration Data vehicle registrations on open data portal.
#' The data is pre-downloaded due to size from: https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' 
#' @param data_file path to downloaded data files https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' @param title_type Either an "Original Title" or a "Transfer Title" - defaults to "Original Title"
#' @param vehicle_type Either for "New" or "Used" vehicles  -defaults to "New"
#' @return tibble of new vehicle registrations by electrification level by month and county
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' new_vehicle_original_registrations <- new_ev_registrations(data_file="C:/coding/Vehicle_Title_Transactions.csv", 
#'                                                            title_type=c("Original Title"), 
#'                                                            vehicle_type=c("New"))}
#' 
#' @export
#'
new_ev_registrations <- function (data_file, title_type="Original Title", vehicle_type="New") {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  df <- readr::read_csv(data_file, show_col_types = FALSE) |>
    dplyr::filter(.data$`New or Used Vehicle` == vehicle_type & .data$`Transaction Type` == title_type) |>
    dplyr::select(date="Transaction Month and Year", variable="Electrification Level", 
                  geography="County", estimate="Transaction Count") |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "BEV \\(Battery Electric Vehicle\\)", "Battery Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "ICE \\(Internal Combustion Engine\\)", "Internal Combustion Engine")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Mild HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Strong HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "PHEV \\(Plug-in Hybrid Electric Vehicle\\)", "Plug-in Hybrid Electric Vehicle")) |>
    dplyr::group_by(.data$date, .data$variable, .data$geography) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(date = lubridate::mdy(date))
  
  region <- df %>%
    dplyr::group_by(.data$date, .data$variable) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(geography = "Region")
  
  df <- dplyr::bind_rows(df, region)
  
  total <- df |>
    dplyr::group_by(.data$date, .data$geography) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  df <- dplyr::left_join(df, total, by=c("date", "geography")) |>
    dplyr::mutate(share = .data$estimate/.data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(geography_type = "County", grouping = vehicle_type, metric = "new-vehicle-registrations") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate", "share")
  
  return(df)
  
}

#' Electric Vehicle Makers by Vehicle Registration
#'
#' This cleans data from the Washington State Registration Data vehicle registrations on open data portal.
#' The data is pre-downloaded due to size from: https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' 
#' @param data_file path to downloaded data files https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' @return tibble of electric vehicle manufacturers registered in the region by year to date
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' ev_manufacturers <- new_ev_models(data_file="C:/coding/Vehicle_Title_Transactions.csv")}
#' 
#' @export
#'
new_ev_models <- function (data_file="C:/coding/Vehicle_Title_Transactions.csv") {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  df <- readr::read_csv(data_file, show_col_types = FALSE) 
  
  mo <- df |>
    dplyr::select(date="Transaction Month and Year") |>
    dplyr::mutate(date = lubridate::mdy(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::mutate(month = lubridate::month(.data$date)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(max_month = max(.data$month)) |>
    dplyr::as_tibble()
  
  working <- df |>
    dplyr::select(date="Transaction Month and Year", variable="Electrification Level", grouping="Make", estimate="Transaction Count") |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "BEV \\(Battery Electric Vehicle\\)", "Battery Electric Vehicle")) |>
    dplyr::filter(.data$variable == "Battery Electric Vehicle") |>
    dplyr::mutate(date = lubridate::mdy(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::group_by(.data$year, .data$variable, .data$grouping) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  working <- dplyr::left_join(working, mo, by=c("year")) |>
    dplyr::mutate(date = lubridate::mdy(paste0(.data$max_month,"-01-", .data$year))) |>
    dplyr::select(-"year", -"max_month")
  
  total <- working %>%
    dplyr::group_by(.data$date, .data$variable) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  working <- dplyr::left_join(working, total, by=c("date", "variable")) |>
    dplyr::mutate(share = .data$estimate/.data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(geography = "Region", geography_type = "County", metric = "ev-manufacturers") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate", "share")
  
  return(working)
  
}

#' New Vehicle Registrations by Electrification Level and Census Tract
#'
#' This cleans data from the Washington State Registration Data vehicle registrations on open data portal.
#' The data is pre-downloaded due to size from: https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' 
#' @param data_file path to downloaded data files https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' @param title_type Either an "Original Title" or a "Transfer Title" - defaults to "Original Title"
#' @param vehicle_type Either for "New" or "Used" vehicles  -defaults to "New"
#' @return tibble of new vehicle registrations by electrification level by month and county
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' registrations_by_tract <- ev_registrations_tract(data_file="C:/coding/Vehicle_Title_Transactions.csv", 
#'                                                  title_type=c("Original Title"), 
#'                                                  vehicle_type=c("New"))}
#' 
#' @export
#'
ev_registrations_tract <- function (data_file, title_type="Original Title", vehicle_type="New") {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  df <- readr::read_csv(data_file, show_col_types = FALSE) |>
    dplyr::filter(.data$`New or Used Vehicle` == vehicle_type & .data$`Transaction Type` == title_type) |>
    dplyr::select(date="Transaction Month and Year", variable="Electrification Level", 
                  geography="2020 GEOID", estimate="Transaction Count") |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "BEV \\(Battery Electric Vehicle\\)", "Battery Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "ICE \\(Internal Combustion Engine\\)", "Internal Combustion Engine")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Mild HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Strong HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "PHEV \\(Plug-in Hybrid Electric Vehicle\\)", "Plug-in Hybrid Electric Vehicle")) |>
    dplyr::group_by(.data$date, .data$variable, .data$geography) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(date = lubridate::mdy(date))
  
  total <- df |>
    dplyr::group_by(.data$date, .data$geography) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  df <- dplyr::left_join(df, total, by=c("date", "geography")) |>
    dplyr::mutate(share = .data$estimate/.data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(geography_type = "Tract", grouping = vehicle_type, metric = "new-vehicle-registrations") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate", "share")
  
  return(df)
  
}
