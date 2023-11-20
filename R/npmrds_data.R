#' Congested Lane Miles by Time of Day and County
#'
#' This function cleans data from the NPMRDS output files from INRIX.
#' Data is pre-processed through a python script on data downloaded from the RTIS site
#' 
#' @param file_path Absolute file path to pre-processed travel time files from NPMRDS
#' 
#' @return tibble lane-miles of congestion by severity and time of day
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' congested_lanes_miles <- process_npmrds_data()}
#' 
#' @export
#'
process_npmrds_data <- function(file_path = "X:/DSA/rtp-dashboard/NPMRDS/") {
  
  # Lists for aggregation
  am_peak <- c("5am", "6am","7am", "8am")
  midday <- c("9am", "10am", "11am","Noon", "1pm", "2pm")
  pm_peak <- c("3pm", "4pm","5pm", "6pm")
  evening <- c("7pm", "8pm", "9pm", "10pm")
  overnight <- c("11pm", "Midnight", "1am", "2am", "3am", "4am")
  
  processed <- NULL
  for (file in list.files(path = file_path, pattern = ".*.csv")) {
    d <- stringr::str_remove_all(file, "_cars_tmc_95th_percentile_speed_weekdays.csv")
    
    print(stringr::str_glue("Opening travel time data for weekdays in {d}"))
    
    t <- readr::read_csv(paste0(file_path, file), show_col_types = FALSE) |> 
      dplyr::mutate(date=d) |>
      dplyr::mutate(lane_miles = .data$miles * .data$thrulanes_unidir) |>
      tidyr::separate(col=.data$date, into = c("date","year")) |>
      dplyr::mutate(year = as.character(.data$year)) |>
      dplyr::mutate(date = lubridate::mdy(paste0(.data$date,"-01-",.data$year))) |>
      dplyr::filter(!(is.na(.data$f_system)))
    
    ifelse(is.null(processed), processed <- t, processed <- dplyr::bind_rows(processed, t))
    
  }
  
  m <- "county" 
  
  print(stringr::str_glue("Summarizing Travel Time Data by {m}"))
  c <- processed |>
    dplyr::select("Tmc", tidyselect::all_of(m), "lane_miles", tidyselect::contains("ratio"), "date", "year") |>
    tidyr::pivot_longer(cols = tidyselect::contains("ratio")) |>
    dplyr::mutate(name = stringr::str_remove_all(.data$name, "_ratio")) |>
    dplyr::mutate(geography = stringr::str_to_title(.data$county), variable = .data$name) |>
    dplyr::mutate(grouping = dplyr::case_when(
      .data$value < 0.25 ~ "Severe",
      .data$value < 0.50 ~ "Heavy",
      .data$value < 0.75 ~ "Moderate",
      .data$value >= 0.75 ~ "Minimal")) |>
    tidyr::drop_na() |>
    dplyr::group_by(.data$geography, .data$date, .data$year, .data$variable, .data$grouping) |>
    dplyr::summarise(congested = sum(.data$lane_miles)) |>
    dplyr::as_tibble()
  
  t <- processed |>
    dplyr::mutate(geography = stringr::str_to_title(.data$county)) |>
    dplyr::group_by(.data$geography, .data$date, .data$year) |>
    dplyr::summarise(total = sum(.data$lane_miles)) |>
    dplyr::as_tibble()
  
  c <- dplyr::left_join(c,t, by=c("geography", "date", "year")) |>
    dplyr::mutate(variable = dplyr::case_when(
      .data$variable %in% am_peak ~ "AM Peak Period",
      .data$variable %in% midday ~ "Midday",
      .data$variable %in% pm_peak ~ "PM Peak Period",
      .data$variable %in% evening ~ "Evening",
      .data$variable %in% overnight ~ "Overnight")) |>
    dplyr::group_by(.data$geography, .data$date, .data$year, .data$variable, .data$grouping) |>
    dplyr::summarise(estimate = sum(.data$congested), total  =sum(.data$total)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(metric = "Congested Lane-Miles", geography_type="County")
  
  r <- c |>
    dplyr::group_by(.data$date, .data$year, .data$variable, .data$grouping) |>
    dplyr::summarise(estimate = sum(.data$estimate), total  =sum(.data$total)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(metric = "Congested Lane-Miles", geography_type="Region", geography="Region")
  
  c <- dplyr::bind_rows(c, r) |>
    dplyr::mutate(share = .data$estimate / .data$total) |> 
    dplyr::select(-"total") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  return(c)
  
}

#' Am, Midday and PM Speed Ratios for mapping
#'
#' This function cleans data from the NPMRDS output files from INRIX.
#' Data is pre-processed through a python script on data downloaded from the RTIS site
#' 
#' @param file_path Absolute file path to pre-processed travel time files from NPMRDS
#' 
#' @return tibble am, midday and pm speed ratios by TMC with geometry for mapping
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' congestion_map_data <- map_npmrds_data()}
#' 
#' @export
#'
map_npmrds_data <- function(file_path = "X:/DSA/rtp-dashboard/NPMRDS/") {
  
  wgs84 <- 4326
  
  print(stringr::str_glue("Opening travel time shapefile"))
  tmc <- sf::st_read(paste0(file_path,"shapefiles/", as.character(lubridate::year(Sys.Date())), "/Washington.shp")) |>
    dplyr::select("Tmc", roadway="RoadName", geography="County") |>
    dplyr::filter(.data$geography %in% c("KING", "KITSAP","PIERCE", "SNOHOMISH")) |>
    dplyr::mutate(geography = stringr::str_to_title(.data$geography)) |>
    sf::st_transform(wgs84) |>
    tidyr::drop_na()
  
  processed <- NULL
  for (file in list.files(path = file_path, pattern = ".*.csv")) {
    d <- stringr::str_remove_all(file, "_cars_tmc_95th_percentile_speed_weekdays.csv")
    print(stringr::str_glue("Opening travel time data for weekdays in {d}"))
    
    t <- readr::read_csv(paste0(file_path, file), show_col_types = FALSE) |> 
      dplyr::mutate(date=d) |>
      dplyr::mutate(lane_miles = .data$miles * .data$thrulanes_unidir) |>
      tidyr::separate(col=.data$date, into = c("date","year")) |>
      dplyr::mutate(year = as.character(.data$year)) |>
      dplyr::mutate(date = lubridate::mdy(paste0(.data$date,"-01-",.data$year))) |>
      dplyr::filter(!(is.na(.data$f_system))) |>
      dplyr::select("Tmc", "date", "year", am_peak="7am_ratio", midday="Noon_ratio", pm_peak="4pm_ratio") |>
      tidyr::drop_na()
    
    print(stringr::str_glue("Joining speed data to shapefile for {d}"))
    p <- dplyr::left_join(tmc, t, by="Tmc") |> tidyr::drop_na()
    
    ifelse(is.null(processed), processed <- p, processed <- dplyr::bind_rows(processed, p))
    
  }
  
  return(processed)
  
}
