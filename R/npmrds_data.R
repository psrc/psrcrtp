#' Congested Lane Miles by Time of Day and County
#'
#' This function cleans data from the NPMRDS output files from INRIX.
#' Data is pre-processed through a python sccript on data downloaded from the RTIS site
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
