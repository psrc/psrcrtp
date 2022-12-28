#' Traffic Fatalities from the Washington Traffic Safety Commission
#'
#' This function pulls and cleans data from yearly fatal traffic collision data from the Washington Traffic Safety Commission.
#' Data comes from the WTSC's Coded Fatal Crash data files.
#' 
#' @return tibble of fatal collision metrics for the region and counties by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' annual_crash_data <- process_crash_data_annual()
#' 
#' @export
#'
process_crash_data_annual <- function() {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  data_years <- seq(10, 20, by = 1)
  
  ptype_lookup <- data.frame(ptype = c(1, 2, 3,
                                       4, 5, 6, 7,
                                       8, 9, 10, 11,
                                       12, 13, 19, 88, 99),
                             person_mode = c("Motor Vehicle", "Motor Vehicle", "Motor Vehicle Not In-Transport",
                                             "Occupant of Non-Motor Vehicle", "Walking", "Biking", "Biking",
                                             "Other", "Unknown Occupant Type", "Person On/In Building", "Other",
                                             "Other", "Other", "Unknown Non-Motorist Type", "Not Reported", "Unknown"))
  
  funclass_lookup <- data.frame(funclass = c(1, 2, 3, 4,
                                             5, 6, 9, 11,
                                             12, 13, 14, 15,
                                             16, 19, 99),
                                road_class = c("Interstate", "Principal Arterial", "Minor Arterial", "Collector",
                                               "Collector", "Local Road", "Unknown", "Interstate",
                                               "Principal Arterial", "Principal Arterial", "Minor Arterial", "Collector",
                                               "Local Road", "Unknown", "Unknown"))
  
  funcsys_lookup <- data.frame(funcsystem = c(1, 2, 3, 4,
                                              5, 6, 7, 96,
                                              98, 99),
                               road_class = c("Interstate", "Principal Arterial", "Principal Arterial", "Minor Arterial",
                                              "Collector", "Collector", "Local Road", "Not in State Inventory",
                                              "Not Reported", "Unknown"))
  
  # Initial processing of fatal crash data
  processed <- NULL
  for (year in data_years) {
    
    data_file <- stringr::str_glue("X:/DSA/rtp-dashboard/WTSC/Data/person{year}.xlsx")
    
    print(stringr::str_glue("Working on 20{year} data processing and cleanup."))
    
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data_file, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>% 
      dplyr::filter(.data$county %in% c(33, 35, 53, 61)
             & .data$injury == 4) %>% 
      dplyr::select(.data$year,
                    county = .data$co_char,
                    .data$numfatal,
                    .data$race_me,
                    .data$ptype,
                    tidyselect::starts_with("func")
      ) %>% 
      dplyr::mutate(date = lubridate::ymd(paste0(.data$year, "-12", "-01")),
             numfatal = 1,
             race = dplyr::case_when(.data$race_me %in% c("AIAN", "API", "Black", "Hispanic", "Multiracial", "Other") ~ "POC",
                                           TRUE ~ .data$race_me))
    
    # Get person_type code values
    t <- dplyr::left_join(t, ptype_lookup, by = c("ptype" = "ptype"))
    
    # Get road_class code values
    ifelse(t$year < 2015,
           t <- dplyr::left_join(t, funclass_lookup, by = c("funclass" = "funclass")),
           t <- dplyr::left_join(t, funcsys_lookup, by = c("funcsystem" = "funcsystem")))
    
    # Fatalities by county
    fc <- t %>% 
      dplyr::group_by(.data$county, .data$date) %>% 
      dplyr::summarize(estimate = sum(.data$numfatal)) %>% 
      tidyr::as_tibble() %>% 
      dplyr::mutate(geography = .data$county,
                    geography_type = "County",
                    variable = "All Fatalities",
                    metric = "Fatalities")
    
    # Fatalities by region
    fp <- t %>% 
      dplyr::group_by(.data$date) %>% 
      dplyr::summarize(estimate = sum(.data$numfatal)) %>% 
      tidyr::as_tibble() %>% 
      dplyr::mutate(geography = "Region",
                    geography_type = "PSRC Region",
                    variable = "All Fatalities",
                    metric = "Fatalities")
    
    # Fatalities by race
    fr <- t %>% 
      dplyr::group_by(.data$race, .data$date) %>% 
      dplyr::summarize(estimate = sum(.data$numfatal)) %>% 
      tidyr::as_tibble() %>% 
      dplyr::mutate(geography = "Region",
                    geography_type = "PSRC Region",
                    grouping = .data$race,
                    variable = "All Fatalities",
                    metric = "Fatalities")
    
    # Fatalities by person mode
    fpm <- t %>% 
      dplyr::group_by(.data$person_mode, .data$date) %>% 
      dplyr::summarize(estimate = sum(.data$numfatal)) %>% 
      tidyr::as_tibble() %>% 
      dplyr::mutate(geography = "Region",
                    geography_type = "PSRC Region",
                    variable = .data$person_mode,
                    metric = "Fatalities")
    
    # Fatalities by road class
    frc <- t %>% 
      dplyr::group_by(.data$road_class, .data$date) %>% 
      dplyr::summarize(estimate = sum(.data$numfatal)) %>% 
      tidyr::as_tibble() %>% 
      dplyr::mutate(geography = "Region",
                    geography_type = "PSRC Region",
                    variable = .data$road_class,
                    metric = "Fatalities")
    
    # Combine summarized tables
    ifelse(is.null(processed),
           processed <- dplyr::bind_rows(fc, fp, fr, fpm, frc),
           processed <- dplyr::bind_rows(processed, fc, fp, fr, fpm, frc))
    
  }
  
  processed <- processed[, c("geography", "geography_type", "grouping", "variable", "metric", "date", "estimate")]
  
  print("All done.")
  return(processed)
  
}
