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
all_registrations <- function () {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  psrc_token <- "tjnJfQzL0SfZJ1cbT0iiCUpO3"
  
  print(stringr::str_glue("Loading Total vehicle registrations from WA State Open Portal via RScorata"))
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
  
  print(stringr::str_glue("Creating a region summary from county data"))
  region <- df |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(bev=sum(.data$bev), phev=sum(.data$phev), ev=sum(.data$ev), non_ev=sum(.data$non_ev), total_vehicles=sum(.data$total_vehicles)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(geography = "Region")
  
  df <- dplyr::bind_rows(df, region)
  
  print(stringr::str_glue("Calculating shares"))
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
  
  print(stringr::str_glue("Final cleanup"))
  final_data <- dplyr::left_join(totals, share, by=c("date", "geography", "variable")) |>
    dplyr::mutate(geography_type = ifelse(.data$geography == "Region", "Region", "County"), 
                  grouping = "All", 
                  metric = "total-vehicle-registrations") |>
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "total_vehicles", "Total Vehicles")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "bev", "Battery Electric")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "phev", "Plug-In Hybrid")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "non_ev", "Internal Combustion")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "ev", "Total Electric Vehicles")) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  print(stringr::str_glue("All done"))
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
#' new_vehicle_registrations <- new_registrations(title_type=c("Original Title"), 
#'                                                vehicle_type=c("New"))}
#' 
#' @export
#'
new_registrations <- function (data_file="X:/DSA/rtp-dashboard/DOL/Vehicle_Title_Transactions.csv", title_type="Original Title", vehicle_type="New") {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  print(stringr::str_glue("Loading the Vehicle Title file from {data_file} - this is a large file so it can take a minute or two"))
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
  
  print(stringr::str_glue("Creating a region summary from county data"))
  region <- df %>%
    dplyr::group_by(.data$date, .data$variable) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(geography = "Region")
  
  df <- dplyr::bind_rows(df, region)
  
  print(stringr::str_glue("Calculating shares and final cleanup"))
  total <- df |>
    dplyr::group_by(.data$date, .data$geography) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  df <- dplyr::left_join(df, total, by=c("date", "geography")) |>
    dplyr::mutate(share = .data$estimate/.data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::mutate(geography_type = ifelse(.data$geography == "Region", "Region", "County"),
                  grouping = vehicle_type, metric = "vehicle-registrations") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  print(stringr::str_glue("All done"))
  return(df)
  
}

#' Electric Vehicle Makers by Vehicle Registration
#'
#' This cleans data from the Washington State Registration Data vehicle registrations on open data portal.
#' The data is pre-downloaded due to size from: https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' 
#' @param data_file path to downloaded data files https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#' @param engine_type type of vehicle to summarize make for - defaults to Battery Electric Vehicle
#' @return tibble of electric vehicle manufacturers registered in the region by year to date
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' ev_manufacturers <- vehicle_manufacturers(engine_type="Battery Electric Vehicle")}
#' 
#' @export
#'
vehicle_manufacturers <- function (data_file="X:/DSA/rtp-dashboard/DOL/Vehicle_Title_Transactions.csv", engine_type="Battery Electric Vehicle") {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  print(stringr::str_glue("Loading the Vehicle Title file from {data_file} - this is a large file so it can take a minute or two"))
  df <- readr::read_csv(data_file, show_col_types = FALSE) 
  
  print(stringr::str_glue("Determining the latest month of data by year"))
  mo <- df |>
    dplyr::select(date="Transaction Month and Year") |>
    dplyr::mutate(date = lubridate::mdy(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::mutate(month = lubridate::month(.data$date)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(max_month = max(.data$month)) |>
    dplyr::as_tibble()
  
  print(stringr::str_glue("Summarizing Manufacturer data for {engine_type} by Region and Year"))
  working <- df |>
    dplyr::select(date="Transaction Month and Year", variable="Electrification Level", grouping="Make", estimate="Transaction Count") |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "BEV \\(Battery Electric Vehicle\\)", "Battery Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "ICE \\(Internal Combustion Engine\\)", "Internal Combustion Engine")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Mild HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Strong HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "PHEV \\(Plug-in Hybrid Electric Vehicle\\)", "Plug-in Hybrid Electric Vehicle")) |>
    dplyr::filter(.data$variable == engine_type) |>
    dplyr::mutate(date = lubridate::mdy(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::group_by(.data$year, .data$variable, .data$grouping) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  print(stringr::str_glue("Calculating shares and final cleanup for {engine_type} manufacturers"))
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
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::mutate(geography = "Region", geography_type = "Region", metric = "vehicle-manufacturers") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  print(stringr::str_glue("All done"))  
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
#' @return tibble of new vehicle registrations by electrification level by year to date by 2020 census tract
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' new_veh_tract <- vehicle_registrations_tract(title_type=c("Original Title"), 
#'                                              vehicle_type=c("New"))}
#' 
#' @export
#'
vehicle_registrations_tract <- function (data_file="X:/DSA/rtp-dashboard/DOL/Vehicle_Title_Transactions.csv", title_type="Original Title", vehicle_type="New") {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  print(stringr::str_glue("Loading the Vehicle Title file from {data_file} - this is a large file so it can take a minute or two"))
  df <- readr::read_csv(data_file, show_col_types = FALSE) 
  
  print(stringr::str_glue("Determining the latest month of data by year"))
  mo <- df |>
    dplyr::select(date="Transaction Month and Year") |>
    dplyr::mutate(date = lubridate::mdy(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::mutate(month = lubridate::month(.data$date)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(max_month = max(.data$month)) |>
    dplyr::as_tibble()
  
  print(stringr::str_glue("Summarizing {vehicle_type} vehicle registrations by Census Tract"))
  working <- df |>
    dplyr::filter(.data$`New or Used Vehicle` == vehicle_type & .data$`Transaction Type` == title_type) |>
    dplyr::select(date="Transaction Month and Year", variable="Electrification Level", 
                  geography="2020 GEOID", estimate="Transaction Count") |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "BEV \\(Battery Electric Vehicle\\)", "Battery Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "ICE \\(Internal Combustion Engine\\)", "Internal Combustion Engine")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Mild HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Strong HEV \\(Hybrid Electric Vehicle\\)", "Hybrid Electric Vehicle")) |>
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "PHEV \\(Plug-in Hybrid Electric Vehicle\\)", "Plug-in Hybrid Electric Vehicle")) |>
    dplyr::mutate(date = lubridate::mdy(.data$date)) |>
    dplyr::mutate(year = lubridate::year(.data$date)) |>
    dplyr::group_by(.data$year, .data$variable, .data$geography) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    tidyr::drop_na()
  
  print(stringr::str_glue("Calculating shares and final cleanup for {vehicle_type} vehicle registrations by Census Tract"))
  working <- dplyr::left_join(working, mo, by=c("year")) |>
    dplyr::mutate(date = lubridate::mdy(paste0(.data$max_month,"-01-", .data$year))) |>
    dplyr::select(-"year", -"max_month")
  
  total <- working |>
    dplyr::group_by(.data$date, .data$geography) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  working <- dplyr::left_join(working, total, by=c("date", "geography")) |>
    dplyr::mutate(share = .data$estimate/.data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(geography = as.character(.data$geography)) |>
    dplyr::mutate(geography_type = "Tract", grouping = vehicle_type, metric = "vehicle-registrations") |>
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  print(stringr::str_glue("All done")) 
  return(working)
  
}

#' Vehicle Registration data by various geographies
#'
#' This function summarizes vehicle registration data by electrification level 
#' The data is pre-downloaded due to size from: https://data.wa.gov/Transportation/Title-Transactions-by-Month/u4cd-bc3x
#'
#' @param dol_registration_file path to downloaded registration data file
#' @return tibble vehicle registrations by electrification level for county, region and census tracts
#'
#' @importFrom rlang .data
#'
#' @examples 
#' \dontrun{
#' vehicle_data <- process_vehicle_registration_data()}
#' 
#' @export
#'
process_vehicle_registration_data <- function(dol_registration_file="X:/DSA/rtp-dashboard/DOL/Vehicle_Title_Transactions.csv") {
  
  processed <- NULL
  total_vehicle_registrations <- all_registrations()
  new_vehicle_registrations <- new_registrations(data_file=dol_registration_file, vehicle_type="New")
  used_vehicle_registrations <- new_registrations(data_file=dol_registration_file, vehicle_type="Used")
  ev_manufacturers <- vehicle_manufacturers(data_file=dol_registration_file, engine_type="Battery Electric Vehicle")
  hev_manufacturers <- vehicle_manufacturers(data_file=dol_registration_file, engine_type="Hybrid Electric Vehicle")
  phev_manufacturers <- vehicle_manufacturers(data_file=dol_registration_file, engine_type="Plug-in Hybrid Electric Vehicle")
  new_vehicle_tract <- vehicle_registrations_tract(data_file=dol_registration_file, vehicle_type=c("New"))
  used_vehicle_tract <- vehicle_registrations_tract(data_file=dol_registration_file, vehicle_type=c("Used"))
  processed <- dplyr::bind_rows(total_vehicle_registrations, 
                                new_vehicle_registrations, used_vehicle_registrations,
                                ev_manufacturers, hev_manufacturers, phev_manufacturers,
                                new_vehicle_tract, used_vehicle_tract)
  return(processed)
  
}
