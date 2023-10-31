#' ACS data for PSRC Counties, Cities, Region and MPOs
#'
#' This function pulls and cleans ACS Data.
#' Pulls data by year for PSRC Counties, Cities, Region and MPOs
#' 
#' @param years Four digit integer (or vector of integers) for year(s) of analysis
#' @param acs_tbl ACS table to be downloaded: defaults to B08301 (Mode to Work)
#' @param acs_variables Name of variable look up to use: defaults to commute-modes
#' @return tibble of mode to work data by census year
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' mode_to_work <- process_acs_data(years=c(2016, 2021))}
#' 
#' @export
#'
process_acs_data <- function(years=c(2021), acs_tbl="B08301", acs_variables="commute-modes") {
  
  # Always use ACS 5yr data since we need it for places, tracts and MPO comparisons as well as 2020 data
  acs_type <- 'acs5'
  
  # Columns to keep from Tidy Census Pull
  cols_to_keep <- c("name", "variable", "estimate", "moe", "census_geography", "year")
  
  # Variables for dashboard
  variables <- readr::read_csv(system.file('extdata', paste0(acs_variables,".csv"), package='psrcrtp'), show_col_types = FALSE)
  
  working_data <- NULL
  for (y in years) {
    print(stringr::str_glue("Working on {y}"))
    
    # County & Region data for PSRC region
    # Download the Data
    county_data <- psrccensus::get_acs_recs(geography = 'county', table.names = acs_tbl, years=y, acs.type = acs_type) 
    # Variables of interest
    county_data <- county_data |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Clean up columns
    county_data <- county_data |> dplyr::select(tidyselect::all_of(cols_to_keep))
    # Add labels
    county_data <- dplyr::left_join(county_data, variables, by=c("variable"))
    # Consolidate rows based on simple labels
    county_data <- county_data |> 
      dplyr::group_by(.data$name, .data$census_geography, .data$year, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label")
    # Get totals
    total <- county_data |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    county_data <- dplyr::left_join(county_data, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    rm(total)
    
    # Cities in the PSRC region
    # Download the Data
    city_data <- psrccensus::get_acs_recs(geography = 'place', table.names = acs_tbl, years=y, acs.type = acs_type) |>
      dplyr::filter(.data$census_geography == "City")
    # Variables of interest
    city_data <- city_data |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Clean up columns
    city_data <- city_data |> dplyr::select(tidyselect::all_of(cols_to_keep))
    # Add labels
    city_data <- dplyr::left_join(city_data, variables, by=c("variable"))
    # Consolidate rows based on simple labels
    city_data <- city_data |> 
      dplyr::group_by(.data$name, .data$census_geography, .data$year, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label")
    # Get totals
    total <- city_data |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    city_data <- dplyr::left_join(city_data, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    rm(total)
    
    # Metro Areas
    mpo <- readr::read_csv(system.file('extdata', 'regional-councils-counties.csv', package='psrcrtp'), show_col_types = FALSE) |> 
      dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) |>
      dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) |>
      dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS))
    
    states <- mpo |> dplyr::select("STATE_FIPS") |> dplyr::distinct() |> dplyr::pull()
    counties <- mpo |> dplyr::select("GEOID") |> dplyr::distinct() |> dplyr::pull()
    
    mpo_data <- NULL
    for (st in states) {
      c <- mpo |> dplyr::filter(.data$STATE_FIPS %in% st) |> dplyr::select("COUNTY_FIPS") |> dplyr::pull()
      d <- tidycensus::get_acs(geography = "county", state=st, county=c, table = acs_tbl, year = y, survey = acs_type)
      ifelse(is.null(mpo_data), mpo_data <- d, mpo_data <- dplyr::bind_rows(mpo_data,d))
      rm(c, d)
    }
    
    # Variables of interest
    mpo_data <- mpo_data |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Add labels
    mpo_data <- dplyr::left_join(mpo_data, variables, by=c("variable"))
    # Add in MPO Information
    mpo_county_data <- dplyr::left_join(mpo, mpo_data, by="GEOID", multiple = "all")
    # Consolidate rows based on simple labels
    mpo_county_data <- mpo_county_data |> 
      dplyr::group_by(.data$MPO_AREA, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label", name = "MPO_AREA") |>
      dplyr::mutate(year=y, census_geography="Metro Areas")
    # Get totals
    total <- mpo_county_data |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    mpo_county_data <- dplyr::left_join(mpo_county_data, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    mpo_data <- mpo_county_data
    rm(total, mpo_county_data)
    
    d <- dplyr::bind_rows(county_data, city_data, mpo_data)
    
    if(is.null(working_data)) {working_data <- d} else {working_data <- dplyr::bind_rows(working_data, d)}
    
  }
  
  # Match column names to rtp-dashboard inputs
  working_data <- working_data |> 
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)), grouping="All", metric=acs_variables) |>
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::select("year", "date", geography="name", geography_type="census_geography", variable="label", "grouping", "metric", "estimate", "share", "moe")
  
  return(working_data)
}

#' Totals and Shares of Commute Travel Mode and Time from PUMS Data
#'
#' This function processes Public Use Microdata Sample (PUMS) estimates from the U.S. Census Bureau.
#' Data is pulled using PSRC's psrccensus package.
#'
#' @param pums_yr List of PUMS data years. Each year should be the latest in non-overlapping five-year series if span is 5.
#' @param pums_span Integer of either 1 or 5 for the 1 or 5 year data series
#' @return tibble of mean, total, and share of travel mode, time and departure time to work by race/ethnicity, county, and region
#'
#' @importFrom rlang .data
#'
#' @examples 
#' \dontrun{
#' commute_data <- process_pums_data(pums_yr = c(2021))}
#' 
#' @export
#'
process_pums_data <- function(pums_yr, pums_span=5) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  pums_vars_old <- c("JWTR",   # travel mode to work (prior to 2019)
                     "JWRIP",  # vehicle occupancy
                     "JWMNP",  # travel time to work
                     "JWDP", # departure time to work
                     "PRACE"   # person race/ethnicity (in psrccensus)
  )
  
  pums_vars_new <- c("JWTRNS", # travel mode to work (2019 and later)
                     "JWRIP",  # vehicle occupancy
                     "JWMNP",  # travel time to work
                     "JWDP", # departure time to work
                     "PRACE"   # person race/ethnicity (in psrccensus)
  )
  
  # Process metrics from PUMS data
  processed <- NULL
  for (i in pums_yr) {
    
    print(stringr::str_glue("Downloading PUMS {pums_span}-year data for {i}. This can take a few minutes."))
    pums <- psrccensus::get_psrc_pums(span = pums_span,
                                      dyear = i,
                                      level = "p",
                                      vars = if(i < 2019) pums_vars_old else pums_vars_new) 
    
    print(stringr::str_glue("Cleaning up attribute names"))
    p <- pums |> 
      dplyr::rename(mode = tidyselect::starts_with("JWTR"), occupancy = "JWRIP", departure = "JWDP", times = "JWMNP", race = "PRACE") |> 
      # Clean up Occupancy for use in determining Drove Alone or Carpool
      dplyr::mutate(occupancy = factor(dplyr::case_when(is.na(.data$occupancy) ~ NA_character_,
                                                        .data$occupancy == 1 ~ "Drove alone",
                                                        TRUE ~ "Carpool"))) |> 
      # Clean up Mode Names
      dplyr::mutate(mode = factor(dplyr::case_when(is.na(.data$mode) ~ NA_character_,
                                                   .data$mode == "Car, truck, or van" & .data$occupancy == "Drove alone" ~ "Drove alone",
                                                   .data$mode == "Car, truck, or van" & .data$occupancy == "Carpool" ~ "Carpool",
                                                   .data$mode == "Bus" ~ "Transit",
                                                   .data$mode == "Bus or trolley bus" ~ "Transit",
                                                   .data$mode == "Ferryboat" ~ "Transit",
                                                   .data$mode == "Streetcar or trolley car (carro publico in Puerto Rico)" ~ "Transit",
                                                   .data$mode == "Light rail, streetcar, or trolley" ~ "Transit",
                                                   .data$mode == "Railroad" ~ "Transit",
                                                   .data$mode == "Long-distance train or commuter rail" ~ "Transit",
                                                   .data$mode == "Other method" ~ "Other",
                                                   .data$mode == "Taxicab" ~ "Other",
                                                   .data$mode == "Motorcycle" ~ "Other",
                                                   .data$mode == "Subway or elevated" ~ "Transit",
                                                   .data$mode == "Subway or elevated rail" ~ "Transit",
                                                   .data$mode == "Worked at home" ~ "Worked from home",
                                                   TRUE ~ as.character(.data$mode)))) |>
      # Clean up Travel Times
      dplyr::mutate(times = ifelse(is.na(.data$times), NA_integer_, as.integer(.data$times))) |>
      # Travel Time Bins
      dplyr::mutate(travel_time_bins = factor(dplyr::case_when(is.na(.data$times) ~ NA_character_,
                                                               as.integer(.data$times) < 15 ~ "Less than 15 minutes",
                                                               as.integer(.data$times) < 30 ~ "15 to 30 minutes",
                                                               as.integer(.data$times) < 45 ~ "30 to 45 minutes",
                                                               as.integer(.data$times) < 60 ~ "45 to 60 minutes",
                                                               as.integer(.data$times) >= 60 ~ "more than 60 minutes"))) |>
      # Departure Time Bins
      dplyr::mutate(departure_time_bins = factor(dplyr::case_when(is.na(.data$departure) ~ NA_character_,
                                                                  # Overnight
                                                                  .data$departure == "12:00 a.m. to 12:29 a.m." ~ "Overnight",
                                                                  .data$departure == "12:30 a.m. to 12:59 a.m." ~ "Overnight",
                                                                  .data$departure == "1:00 a.m. to 1:29 a.m." ~ "Overnight",
                                                                  .data$departure == "1:30 a.m. to 1:59 a.m." ~ "Overnight",
                                                                  .data$departure == "2:00 a.m. to 2:29 a.m." ~ "Overnight",
                                                                  .data$departure == "2:30 a.m. to 2:59 a.m." ~ "Overnight",
                                                                  .data$departure == "3:00 a.m. to 3:09 a.m." ~ "Overnight",
                                                                  .data$departure == "3:10 a.m. to 3:19 a.m." ~ "Overnight",
                                                                  .data$departure == "3:20 a.m. to 3:29 a.m." ~ "Overnight",
                                                                  .data$departure == "3:30 a.m. to 3:39 a.m." ~ "Overnight",
                                                                  .data$departure == "3:40 a.m. to 3:49 a.m." ~ "Overnight",
                                                                  .data$departure == "3:50 a.m. to 3:59 a.m." ~ "Overnight",
                                                                  .data$departure == "4:00 a.m. to 4:09 a.m." ~ "Overnight",
                                                                  .data$departure == "4:10 a.m. to 4:19 a.m." ~ "Overnight",
                                                                  .data$departure == "4:20 a.m. to 4:29 a.m." ~ "Overnight",
                                                                  .data$departure == "4:30 a.m. to 4:39 a.m." ~ "Overnight",
                                                                  .data$departure == "4:40 a.m. to 4:49 a.m." ~ "Overnight",
                                                                  .data$departure == "4:50 a.m. to 4:59 a.m." ~ "Overnight",
                                                                  # Early Morning
                                                                  .data$departure == "5:00 a.m. to 5:04 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:05 a.m. to 5:09 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:10 a.m. to 5:14 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:15 a.m. to 5:19 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:20 a.m. to 5:24 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:25 a.m. to 5:29 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:30 a.m. to 5:34 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:35 a.m. to 5:39 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:40 a.m. to 5:44 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:45 a.m. to 5:49 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:50 a.m. to 5:54 a.m." ~ "Early Morning",
                                                                  .data$departure == "5:55 a.m. to 5:59 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:00 a.m. to 6:04 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:05 a.m. to 6:09 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:10 a.m. to 6:14 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:15 a.m. to 6:19 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:20 a.m. to 6:24 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:25 a.m. to 6:29 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:30 a.m. to 6:34 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:35 a.m. to 6:39 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:40 a.m. to 6:44 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:45 a.m. to 6:49 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:50 a.m. to 6:54 a.m." ~ "Early Morning",
                                                                  .data$departure == "6:55 a.m. to 6:59 a.m." ~ "Early Morning",
                                                                  # AM Peak
                                                                  .data$departure == "7:00 a.m. to 7:04 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:05 a.m. to 7:09 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:10 a.m. to 7:14 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:15 a.m. to 7:19 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:20 a.m. to 7:24 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:25 a.m. to 7:29 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:30 a.m. to 7:34 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:35 a.m. to 7:39 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:40 a.m. to 7:44 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:45 a.m. to 7:49 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:50 a.m. to 7:54 a.m." ~ "AM Peak",
                                                                  .data$departure == "7:55 a.m. to 7:59 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:00 a.m. to 8:04 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:05 a.m. to 8:09 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:10 a.m. to 8:14 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:15 a.m. to 8:19 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:20 a.m. to 8:24 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:25 a.m. to 8:29 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:30 a.m. to 8:34 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:35 a.m. to 8:39 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:40 a.m. to 8:44 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:45 a.m. to 8:49 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:50 a.m. to 8:54 a.m." ~ "AM Peak",
                                                                  .data$departure == "8:55 a.m. to 8:59 a.m." ~ "AM Peak",
                                                                  # Late Morning
                                                                  .data$departure == "9:00 a.m. to 9:04 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:05 a.m. to 9:09 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:10 a.m. to 9:14 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:15 a.m. to 9:19 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:20 a.m. to 9:24 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:25 a.m. to 9:29 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:30 a.m. to 9:34 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:35 a.m. to 9:39 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:40 a.m. to 9:44 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:45 a.m. to 9:49 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:50 a.m. to 9:54 a.m." ~ "Late Morning",
                                                                  .data$departure == "9:55 a.m. to 9:59 a.m." ~ "Late Morning",
                                                                  .data$departure == "10:00 a.m. to 10:09 a.m." ~ "Late Morning",
                                                                  .data$departure == "10:10 a.m. to 10:19 a.m." ~ "Late Morning",
                                                                  .data$departure == "10:20 a.m. to 10:29 a.m." ~ "Late Morning",
                                                                  .data$departure == "10:30 a.m. to 10:39 a.m." ~ "Late Morning",
                                                                  .data$departure == "10:40 a.m. to 10:49 a.m." ~ "Late Morning",
                                                                  .data$departure == "10:50 a.m. to 10:59 a.m." ~ "Late Morning",
                                                                  .data$departure == "11:00 a.m. to 11:09 a.m." ~ "Late Morning",
                                                                  .data$departure == "11:10 a.m. to 11:19 a.m." ~ "Late Morning",
                                                                  .data$departure == "11:20 a.m. to 11:29 a.m." ~ "Late Morning",
                                                                  .data$departure == "11:30 a.m. to 11:39 a.m." ~ "Late Morning",
                                                                  .data$departure == "11:40 a.m. to 11:49 a.m." ~ "Late Morning",
                                                                  .data$departure == "11:50 a.m. to 11:59 a.m." ~ "Late Morning",
                                                                  # Afternoon
                                                                  .data$departure == "12:00 p.m. to 12:09 p.m." ~ "Afternoon",
                                                                  .data$departure == "12:10 p.m. to 12:19 p.m." ~ "Afternoon",
                                                                  .data$departure == "12:20 p.m. to 12:29 p.m." ~ "Afternoon",
                                                                  .data$departure == "12:30 p.m. to 12:39 p.m." ~ "Afternoon",
                                                                  .data$departure == "12:40 p.m. to 12:49 p.m." ~ "Afternoon",
                                                                  .data$departure == "12:50 p.m. to 12:59 p.m." ~ "Afternoon",
                                                                  .data$departure == "1:00 p.m. to 1:09 p.m." ~ "Afternoon",
                                                                  .data$departure == "1:10 p.m. to 1:19 p.m." ~ "Afternoon",
                                                                  .data$departure == "1:20 p.m. to 1:29 p.m." ~ "Afternoon",
                                                                  .data$departure == "1:30 p.m. to 1:39 p.m." ~ "Afternoon",
                                                                  .data$departure == "1:40 p.m. to 1:49 p.m." ~ "Afternoon",
                                                                  .data$departure == "1:50 p.m. to 1:59 p.m." ~ "Afternoon",
                                                                  .data$departure == "2:00 p.m. to 2:09 p.m." ~ "Afternoon",
                                                                  .data$departure == "2:10 p.m. to 2:19 p.m." ~ "Afternoon",
                                                                  .data$departure == "2:20 p.m. to 2:29 p.m." ~ "Afternoon",
                                                                  .data$departure == "2:30 p.m. to 2:39 p.m." ~ "Afternoon",
                                                                  .data$departure == "2:40 p.m. to 2:49 p.m." ~ "Afternoon",
                                                                  .data$departure == "2:50 p.m. to 2:59 p.m." ~ "Afternoon",
                                                                  .data$departure == "3:00 p.m. to 3:09 p.m." ~ "Afternoon",
                                                                  .data$departure == "3:10 p.m. to 3:19 p.m." ~ "Afternoon",
                                                                  .data$departure == "3:20 p.m. to 3:29 p.m." ~ "Afternoon",
                                                                  .data$departure == "3:30 p.m. to 3:39 p.m." ~ "Afternoon",
                                                                  .data$departure == "3:40 p.m. to 3:49 p.m." ~ "Afternoon",
                                                                  .data$departure == "3:50 p.m. to 3:59 p.m." ~ "Afternoon",
                                                                  # Evening
                                                                  .data$departure == "4:00 p.m. to 4:09 p.m." ~ "Evening",
                                                                  .data$departure == "4:10 p.m. to 4:19 p.m." ~ "Evening",
                                                                  .data$departure == "4:20 p.m. to 4:29 p.m." ~ "Evening",
                                                                  .data$departure == "4:30 p.m. to 4:39 p.m." ~ "Evening",
                                                                  .data$departure == "4:40 p.m. to 4:49 p.m." ~ "Evening",
                                                                  .data$departure == "4:50 p.m. to 4:59 p.m." ~ "Evening",
                                                                  .data$departure == "5:00 p.m. to 5:09 p.m." ~ "Evening",
                                                                  .data$departure == "5:10 p.m. to 5:19 p.m." ~ "Evening",
                                                                  .data$departure == "5:20 p.m. to 5:29 p.m." ~ "Evening",
                                                                  .data$departure == "5:30 p.m. to 5:39 p.m." ~ "Evening",
                                                                  .data$departure == "5:40 p.m. to 5:49 p.m." ~ "Evening",
                                                                  .data$departure == "5:50 p.m. to 5:59 p.m." ~ "Evening",
                                                                  .data$departure == "6:00 p.m. to 6:09 p.m." ~ "Evening",
                                                                  .data$departure == "6:10 p.m. to 6:19 p.m." ~ "Evening",
                                                                  .data$departure == "6:20 p.m. to 6:29 p.m." ~ "Evening",
                                                                  .data$departure == "6:30 p.m. to 6:39 p.m." ~ "Evening",
                                                                  .data$departure == "6:40 p.m. to 6:49 p.m." ~ "Evening",
                                                                  .data$departure == "6:50 p.m. to 6:59 p.m." ~ "Evening",
                                                                  .data$departure == "7:00 p.m. to 7:29 p.m." ~ "Evening",
                                                                  .data$departure == "7:30 p.m. to 7:59 p.m." ~ "Evening",
                                                                  .data$departure == "8:00 p.m. to 8:29 p.m." ~ "Evening",
                                                                  .data$departure == "8:30 p.m. to 8:59 p.m." ~ "Evening",
                                                                  .data$departure == "9:00 p.m. to 9:09 p.m." ~ "Evening",
                                                                  .data$departure == "9:10 p.m. to 9:19 p.m." ~ "Evening",
                                                                  .data$departure == "9:20 p.m. to 9:29 p.m." ~ "Evening",
                                                                  .data$departure == "9:30 p.m. to 9:39 p.m." ~ "Evening",
                                                                  .data$departure == "9:40 p.m. to 9:49 p.m." ~ "Evening",
                                                                  .data$departure == "9:50 p.m. to 9:59 p.m." ~ "Evening",
                                                                  .data$departure == "10:00 p.m. to 10:09 p.m." ~ "Evening",
                                                                  .data$departure == "10:10 p.m. to 10:19 p.m." ~ "Evening",
                                                                  .data$departure == "10:20 p.m. to 10:29 p.m." ~ "Evening",
                                                                  .data$departure == "10:30 p.m. to 10:39 p.m." ~ "Evening",
                                                                  .data$departure == "10:40 p.m. to 10:49 p.m." ~ "Evening",
                                                                  .data$departure == "10:50 p.m. to 10:59 p.m." ~ "Evening",
                                                                  .data$departure == "11:00 p.m. to 11:29 p.m." ~ "Evening",
                                                                  .data$departure == "11:30 p.m. to 11:59 p.m." ~ "Evening")))
    
    print(stringr::str_glue("Calculating mean travel time to work by county for PUMS {pums_span}-year data for {i}"))
    mean_time_county <- psrccensus::psrc_pums_mean(p, stat_var = "times", group_vars = "COUNTY") |>
      dplyr::select(geography = "COUNTY", date = "DATA_YEAR", estimate = dplyr::ends_with("mean"), moe = dplyr::ends_with("moe")) |>
      dplyr::mutate(metric = "Mean Commute Time",
                    date = lubridate::ymd(paste0(.data$date, "-12-01")),
                    year = as.character(lubridate::year(.data$date)),
                    geography_type = ifelse(.data$geography == "Region", "Region", "County"),
                    grouping = "Workers over 16yrs of age",
                    variable = "travel time to work") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "moe")
    
    print(stringr::str_glue("Calculating mean travel time to work by Race for PUMS {pums_span}-year data for {i}"))
    mean_time_race <- psrccensus::psrc_pums_mean(p, stat_var = "times", group_vars = "race") |> 
      dplyr::filter(.data$race != "Total") %>% 
      dplyr::select(geography = "COUNTY", grouping = "race", date = "DATA_YEAR", estimate = dplyr::ends_with("mean"), moe = dplyr::ends_with("moe")) |>
      dplyr::mutate(metric = "Mean Commute Time",
                    date = lubridate::ymd(paste0(.data$date, "-12-01")),
                    year = as.character(lubridate::year(.data$date)),
                    geography_type = "Race",
                    variable = "travel time to work") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "moe")
    
    print(stringr::str_glue("Calculating travel time buckets by county for PUMS {pums_span}-year data for {i}"))
    time_bins_county <- psrccensus::psrc_pums_count(p, group_vars = c("COUNTY", "travel_time_bins"), incl_na = FALSE) |>
      dplyr::filter(.data$COUNTY != "Region") |> 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "County",
                       variable = .data$travel_time_bins,
                       metric = "Travel Time Bins",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = "All") |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating travel time buckets by Region for PUMS {pums_span}-year data for {i}"))
    time_bins_region <- psrccensus::psrc_pums_count(p, group_vars = c("travel_time_bins"), incl_na = FALSE) |>
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "Region",
                       variable = .data$travel_time_bins,
                       metric = "Travel Time Bins",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = "All") |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating travel time buckets by Race for PUMS {pums_span}-year data for {i}"))
    time_bins_race <- psrccensus::psrc_pums_count(p, group_vars = c("race", "travel_time_bins"), incl_na = FALSE) |>
      dplyr::filter(.data$race != "Total") |>
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "Race",
                       variable = .data$travel_time_bins,
                       metric = "Travel Time Bins",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = .data$race,) |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating travel modes by county for PUMS {pums_span}-year data for {i}"))
    mode_county <- psrccensus::psrc_pums_count(p, group_vars = c("COUNTY", "mode"),incl_na = FALSE) |>
      dplyr::filter(.data$COUNTY != "Region") |> 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "County",
                       variable = .data$mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = "All") |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating travel modes by Region for PUMS {pums_span}-year data for {i}"))
    mode_region <- psrccensus::psrc_pums_count(p, group_vars = c("mode"),incl_na = FALSE) |>
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "Region",
                       variable = .data$mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = "All") |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating travel modes by Race for PUMS {pums_span}-year data for {i}"))
    mode_race <- psrccensus::psrc_pums_count(p, group_vars = c("race", "mode"),incl_na = FALSE) |>
      dplyr::filter(.data$race != "Total") |>
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "Race",
                       variable = .data$mode,
                       metric = "Commute Mode",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = .data$race) |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating departure time buckets by county for PUMS {pums_span}-year data for {i}"))
    depart_time_bins_county <- psrccensus::psrc_pums_count(p, group_vars = c("COUNTY", "departure_time_bins"), incl_na = FALSE) |>
      dplyr::filter(.data$COUNTY != "Region") |> 
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "County",
                       variable = .data$departure_time_bins,
                       metric = "Departure Time Bins",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = "All") |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating departure time buckets by Region for PUMS {pums_span}-year data for {i}"))
    depart_time_bins_region <- psrccensus::psrc_pums_count(p, group_vars = c("departure_time_bins"), incl_na = FALSE) |>
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "Region",
                       variable = .data$departure_time_bins,
                       metric = "Departure Time Bins",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = "All") |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating departure time buckets by Race for PUMS {pums_span}-year data for {i}"))
    depart_time_bins_race <- psrccensus::psrc_pums_count(p, group_vars = c("race", "departure_time_bins"), incl_na = FALSE) |>
      dplyr::filter(.data$race != "Total") |>
      dplyr::transmute(geography = .data$COUNTY,
                       geography_type = "Race",
                       variable = .data$departure_time_bins,
                       metric = "Departure Time Bins",
                       date = lubridate::ymd(paste0(.data$DATA_YEAR, "-12-01")),
                       year = as.character(lubridate::year(.data$date)),
                       estimate = .data$count,
                       moe = .data$count_moe,
                       share = .data$share,
                       grouping = .data$race,) |>
      dplyr::filter(.data$variable != "Total") |>
      dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share", "moe")
    
    # Combine summarized tables
    ifelse(is.null(processed),
           processed <- dplyr::bind_rows(mean_time_county, mean_time_race,
                                         mode_county, mode_region, mode_race,
                                         time_bins_county, time_bins_region, time_bins_race,
                                         depart_time_bins_county, depart_time_bins_region, depart_time_bins_race),
           processed <- dplyr::bind_rows(processed, 
                                         mean_time_county, mean_time_race,
                                         mode_county, mode_region, mode_race,
                                         time_bins_county, time_bins_region, time_bins_race,
                                         depart_time_bins_county, depart_time_bins_region, depart_time_bins_race))
  }
  
  print("All done.")
  return(processed)
}

#' Commute Travel Mode and Time Data from ACS & PUMS Data
#'
#' This function processes Public Use Microdata Sample (PUMS) ACS etimates from the U.S. Census Bureau.
#' Data is pulled using PSRC's psrccensus package.
#'
#' @param data_years List of data years. Each year should be the latest in non-overlapping five-year series.
#' @return tibble ACS and PUMS travel mode, time and departure time to work by race/ethnicity, county, and region
#'
#' @importFrom rlang .data
#'
#' @examples 
#' \dontrun{
#' commute_data <- process_commute_data(data_years = c(2021))}
#' 
#' @export
#'
process_commute_data <- function(data_years = c(2021)) {
  
  print("Working on Mode to Work")
  mw <- process_acs_data(years=data_years, acs_tbl="B08301", acs_variables="commute-modes")
  print("Working on Travel time to Work")
  tw <- process_acs_data(years=data_years, acs_tbl="B08303", acs_variables="commute-times")
  print("Working on Departure time to work")
  dw <- process_acs_data(years=data_years, acs_tbl="B08011", acs_variables="departure-time")
  print("Working on Time, Mode and Departure data from PUMS")
  pd <- process_pums_data(pums_yr=data_years)
  
  processed <- dplyr::bind_rows(mw, tw, dw, pd)
  
  return(processed)
  
}
