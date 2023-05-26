#' Totals and Shares of Commute Travel Mode and Time from PUMS Data
#'
#' This function processes Public Use Microdata Sample (PUMS) estimates from the U.S. Census Bureau.
#' Data is pulled using PSRC's psrccensus package.
#'
#' @param pums_yr List of PUMS data years. Each year should be the latest in non-overlapping five-year series if span is 5.
#' @param pums_span Integer of either 1 or 5 for the 1 or 5 year data series
#' @return tibble of mean, total, and share of travel mode and time to work by race/ethnicity, county, and region
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#'
#' @examples 
#' \dontrun{
#' commute_data <- process_pums_data(pums_yr = c(2015,2020))}
#' 
#' @export
#'
process_pums_data <- function(pums_yr, pums_span=5) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  pums_vars_old <- c("JWTR",   # travel mode to work (prior to 2019)
                     "JWRIP",  # vehicle occupancy
                     "JWMNP",  # travel time to work
                     "PRACE"   # person race/ethnicity (in psrccensus)
  )
  
  pums_vars_new <- c("JWTRNS", # travel mode to work (2019 and later)
                     "JWRIP",  # vehicle occupancy
                     "JWMNP",  # travel time to work
                     "PRACE"   # person race/ethnicity (in psrccensus)
  )
  
  # Process metrics from PUMS data
  processed <- NULL
  for (i in pums_yr) {
    
    print(paste0("Downloading PUMS ", pums_span, "-year data for ",i,". This can take a few minutes."))
    pums <- psrccensus::get_psrc_pums(span = pums_span,
                                      dyear = i,
                                      level = "p",
                                      vars = if(i < 2019) pums_vars_old else pums_vars_new) %>% 
      dplyr::rename(travel_mode = tidyselect::starts_with("JWTR"),
                    vehicle_occ = "JWRIP",
                    travel_time = "JWMNP",
                    race = "PRACE") %>% 
      dplyr::mutate(vehicle_occ = factor(dplyr::case_when(is.na(.data$vehicle_occ) ~ NA_character_,
                                                          .data$vehicle_occ == 1 ~ "Drove alone",
                                                          TRUE ~ "Carpool"))) %>% 
      dplyr::mutate(travel_mode = factor(dplyr::case_when(is.na(.data$travel_mode) ~ NA_character_,
                                                          .data$travel_mode == "Car, truck, or van" & .data$vehicle_occ == "Drove alone" ~ "Drove alone",
                                                          .data$travel_mode == "Car, truck, or van" & .data$vehicle_occ == "Carpool" ~ "Carpool",
                                                          .data$travel_mode == "Bus or trolley bus" ~ "Bus",
                                                          .data$travel_mode == "Ferryboat" ~ "Ferry",
                                                          .data$travel_mode == "Streetcar or trolley car (carro publico in Puerto Rico)" ~ "Light Rail, Streetcar & Monorail",
                                                          .data$travel_mode == "Light rail, streetcar, or trolley" ~ "Light Rail, Streetcar & Monorail",
                                                          .data$travel_mode == "Railroad" ~ "Commuter Rail",
                                                          .data$travel_mode == "Long-distance train or commuter rail" ~ "Commuter Rail",
                                                          .data$travel_mode == "Other method" ~ "Other",
                                                          .data$travel_mode == "Subway or elevated" ~ "Light Rail, Streetcar & Monorail",
                                                          .data$travel_mode == "Subway or elevated rail" ~ "Light Rail, Streetcar & Monorail",
                                                          .data$travel_mode == "Worked at home" ~ "Worked from home",
                                                          TRUE ~ as.character(.data$travel_mode))),
                    simple_mode = factor(dplyr::case_when(is.na(.data$travel_mode) ~ NA_character_,
                                                          .data$travel_mode == "Bus" ~ "Transit",
                                                          .data$travel_mode == "Ferry" ~ "Transit",
                                                          .data$travel_mode == "Light Rail, Streetcar & Monorail" ~ "Transit",
                                                          .data$travel_mode == "Commuter Rail" ~ "Transit",
                                                          .data$travel_mode == "Motorcycle" ~ "Other",
                                                          .data$travel_mode == "Taxicab" ~ "Other",
                                                          TRUE ~ as.character(.data$travel_mode))),
                    travel_time = ifelse(is.na(.data$travel_time), NA_integer_, as.integer(.data$travel_time)),
                    commute_class = factor(dplyr::case_when(is.na(.data$travel_time) ~ NA_character_,
                                                            as.integer(.data$travel_time) < 15 ~ "15 mintues or less",
                                                            as.integer(.data$travel_time) < 30 ~ "15 to 30 mintues",
                                                            as.integer(.data$travel_time) < 60 ~ "30 to 60 mintues",
                                                            as.integer(.data$travel_time) < 90 ~ "60 to 90 mintues",
                                                            as.integer(.data$travel_time) >= 90 ~ "more than 90 minutes")),
                    race_condensed = factor(dplyr::case_when(.data$race %in% c("American Indian or Alaskan Native Alone",
                                                                               "Asian alone",
                                                                               "Black or African American alone",
                                                                               "Hispanic or Latino",
                                                                               "Native Hawaiian and Other Pacific Islander alone",
                                                                               "Some Other Race alone",
                                                                               "Two or More Races")
                                                             ~ "POC",
                                                             .data$race == "White alone" ~ "White")))
    
    print(paste0("Calculating mean travel time to work by county for PUMS ", pums_span, "-year data for ",i))
    mean_time_county <- psrccensus::psrc_pums_mean(pums,
                                                   stat_var = "travel_time",
                                                   group_vars = "COUNTY") %>% 
      dplyr::select(geography = "COUNTY",
                    date = "DATA_YEAR",
                    estimate = dplyr::ends_with("mean"),
                    moe = dplyr::ends_with("moe")) %>% 
      dplyr::mutate(metric = "Mean Commute Time",
                    date = lubridate::ymd(paste0(.data$date, "-12-01")),
                    geography_type = ifelse(.data$geography == "Region", "PSRC Region", "County"))
    
    print(paste0("Calculating mean travel time to work by People of Color for PUMS ", pums_span, "-year data for ",i))
    mean_time_poc <- psrccensus::psrc_pums_mean(pums,
                                                stat_var = "travel_time",
                                                group_vars = "race_condensed") %>% 
      dplyr::filter(.data$race_condensed != "Total") %>% 
      dplyr::select(geography = "COUNTY",
                    grouping = "race_condensed",
                    date = "DATA_YEAR",
                    estimate = dplyr::ends_with("mean"),
                    moe = dplyr::ends_with("moe")) %>% 
      dplyr::mutate(metric = "Mean Commute Time",
                    date = lubridate::ymd(paste0(.data$date, "-12-01")),
                    geography_type = "PSRC Region People of Color")
    
    print(paste0("Calculating mean travel time to work by race for PUMS ", pums_span, "-year data for ",i))
    mean_time_race <- psrccensus::psrc_pums_mean(pums,
                                                 stat_var = "travel_time",
                                                 group_vars = "race") %>% 
      dplyr::filter(.data$race != "Total") %>% 
      dplyr::select(geography = "COUNTY",
                    grouping = "race",
                    date = "DATA_YEAR",
                    estimate = dplyr::ends_with("mean"),
                    moe = dplyr::ends_with("moe")) %>% 
      dplyr::mutate(metric = "Mean Commute Time",
                    date = lubridate::ymd(paste0(.data$date, "-12-01")),
                    geography_type = "PSRC Region Race & Ethnicity")
    
    means <- dplyr::bind_rows(mean_time_county, mean_time_poc, mean_time_race)
    rm(mean_time_county, mean_time_poc, mean_time_race)
    
    print(paste0("Calculating travel time buckets by county for PUMS ", pums_span, "-year data for ",i))
    time_county <- psrccensus::psrc_pums_count(pums,
                                               group_vars = c("COUNTY", "commute_class"),
                                               incl_na = FALSE) %>% 
      dplyr::filter(.data$COUNTY != "Region") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "County",
                       variable = .data$commute_class,
                       metric = "Commute Time Class",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating travel time buckets by Region for PUMS ", pums_span, "-year data for ",i))
    time_region <- psrccensus::psrc_pums_count(pums,
                                               group_vars = "commute_class",
                                               incl_na = FALSE) %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region",
                       variable = .data$commute_class,
                       metric = "Commute Time Class",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating travel time buckets by Region by People of Color for PUMS ", pums_span, "-year data for ",i))
    time_poc <- psrccensus::psrc_pums_count(pums,
                                            group_vars = c("race_condensed", "commute_class"),
                                            incl_na = FALSE) %>% 
      dplyr::filter(.data$race_condensed != "Total") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region People of Color",
                       grouping = .data$race_condensed,
                       variable = .data$commute_class,
                       metric = "Commute Time Class",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe)%>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating travel time buckets by Region by Race & Ethnicity for PUMS ", pums_span, "-year data for ",i))
    time_race <- psrccensus::psrc_pums_count(pums,
                                             group_vars = c("race", "commute_class"),
                                             incl_na = FALSE) %>% 
      dplyr::filter(.data$race != "Total") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region Race & Ethnicity",
                       grouping = .data$race,
                       variable = .data$commute_class,
                       metric = "Commute Time Class",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    times <- dplyr::bind_rows(time_county, time_region, time_poc, time_race)
    rm(time_county, time_region, time_poc, time_race)
    
    print(paste0("Calculating travel modes by county for PUMS ", pums_span, "-year data for ",i))
    mode_county <- psrccensus::psrc_pums_count(pums,
                                               group_vars = c("COUNTY", "travel_mode"),
                                               incl_na = FALSE) %>% 
      dplyr::filter(.data$COUNTY != "Region") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "County",
                       variable = .data$travel_mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating travel modes by Region for PUMS ", pums_span, "-year data for ",i))
    mode_region <- psrccensus::psrc_pums_count(pums,
                                               group_vars = "travel_mode",
                                               incl_na = FALSE) %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region",
                       variable = .data$travel_mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating travel modes by Region and People of Color for PUMS ", pums_span, "-year data for ",i))
    mode_poc <- psrccensus::psrc_pums_count(pums,
                                            group_vars = c("race_condensed", "travel_mode"),
                                            incl_na = FALSE) %>% 
      dplyr::filter(.data$race_condensed != "Total") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region People of Color",
                       grouping = .data$race_condensed,
                       variable = .data$travel_mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating travel modes by Region by Race & Ethnicity for PUMS ", pums_span, "-year data for ",i))
    mode_race <- psrccensus::psrc_pums_count(pums,
                                             group_vars = c("race", "travel_mode"),
                                             incl_na = FALSE) %>% 
      dplyr::filter(.data$race != "Total") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region Race & Ethnicity",
                       grouping = .data$race,
                       variable = .data$travel_mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    modes <- dplyr::bind_rows(mode_county, mode_region, mode_poc, mode_race)
    rm(mode_county, mode_region, mode_poc, mode_race)
    
    print(paste0("Calculating simple travel modes by county for PUMS ", pums_span, "-year data for ",i))
    simple_mode_county <- psrccensus::psrc_pums_count(pums,
                                                      group_vars = c("COUNTY", "simple_mode"),
                                                      incl_na = FALSE) %>% 
      dplyr::filter(.data$COUNTY != "Region") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "County",
                       variable = .data$simple_mode,
                       metric = "Simplified Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating simple travel modes by Region for PUMS ", pums_span, "-year data for ",i))
    simple_mode_region <- psrccensus::psrc_pums_count(pums,
                                                      group_vars = "simple_mode",
                                                      incl_na = FALSE) %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region",
                       variable = .data$simple_mode,
                       metric = "Simplified Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating simple travel modes by Region and People of Color for PUMS ", pums_span, "-year data for ",i))
    simple_mode_poc <- psrccensus::psrc_pums_count(pums,
                                                   group_vars = c("race_condensed", "simple_mode"),
                                                   incl_na = FALSE) %>% 
      dplyr::filter(.data$race_condensed != "Total") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region People of Color",
                       grouping = .data$race_condensed,
                       variable = .data$simple_mode,
                       metric = "Simplified Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    print(paste0("Calculating simple travel modes by Region by Race & Ethnicity for PUMS ", pums_span, "-year data for ",i))
    simple_mode_race <- psrccensus::psrc_pums_count(pums,
                                                    group_vars = c("race", "simple_mode"),
                                                    incl_na = FALSE) %>% 
      dplyr::filter(.data$race != "Total") %>% 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "PSRC Region Race & Ethnicity",
                       grouping = .data$race,
                       variable = .data$simple_mode,
                       metric = "Simplified Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       estimate = .data$count,
                       estimate_moe = .data$count_moe,
                       share = .data$share,
                       share_moe = .data$share_moe) %>%
      dplyr::filter(.data$variable != "Total")
    
    simple_modes <- dplyr::bind_rows(simple_mode_county, simple_mode_region, simple_mode_poc, simple_mode_race)
    rm(simple_mode_county, simple_mode_region, simple_mode_poc, simple_mode_race)
    
    # Combine summarized tables
    ifelse(is.null(processed),
           processed <- dplyr::bind_rows(means, modes, times, simple_modes),
           processed <- dplyr::bind_rows(processed, means, modes, times, simple_modes))
    
    processed$year_span <- pums_span
    
  }
  
  processed <- processed[, c("geography", "geography_type", "grouping", "variable", "metric", "date", "estimate", "estimate_moe", "share", "share_moe", "year_span")]
  print("All done.")
  return(processed)
}
