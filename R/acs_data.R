#' PSRC Counties and Region ACS Work Mode Share
#'
#' This function pulls and cleans ACS Mode Share Data from Census Table B08006.
#' Pulls data by year for King, Kitsap, Pierce and Snohomish counties and summarized by PSRC Region
#' 
#' @param years Four digit integer (or vector of integers) for year(s) of analysis
#' @return tibble of mode to work data by county and region by census year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' modes_psrc_counties <- acs_mode_to_work_county(years <- 2021)
#' 
#' @export
#'
acs_mode_to_work_county <- function(years) {
  
  commute_trips <- c("B08006_001")
  da_trips <- c("B08006_003")
  carpool_trips <- c("B08006_004")
  transit_trips <- c("B08006_008")
  bike_trips <- c("B08006_014")
  walk_trips <- c("B08006_015")
  other_trips <- c("B08006_016")
  wfh_trips <- c("B08006_017")
  
  all_trips <- c(commute_trips, da_trips, carpool_trips, transit_trips, bike_trips, walk_trips, other_trips, wfh_trips)
  non_da_trips <- c(carpool_trips,transit_trips,bike_trips,walk_trips,wfh_trips)
  
  mode_data = NULL
  for (y in years) {
    print(paste0("Working on ",y))
    
    if(y==2020) {acs_type='acs5'} else {acs_type='acs1'}
    
    modes <- psrccensus::get_acs_recs(geography = 'county',table.names = c('B08006'), years=y, acs.type = acs_type) %>% 
      dplyr::filter(.data$variable %in% all_trips)
    
    totals <- modes %>% 
      dplyr::filter(.data$variable %in% commute_trips) %>% 
      dplyr::select("name", "year", "estimate") %>% 
      dplyr::rename(total="estimate")
    
    modes <- dplyr::left_join(modes, totals, by=c("name","year")) %>% 
      dplyr::mutate(share=.data$estimate/.data$total) %>%
      dplyr::select("name", "estimate", "share", "moe", "label", "concept", "census_geography", "year") %>%
      dplyr::mutate(label = stringr::str_replace_all(.data$label, "!!", " ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Estimate Total: ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Estimate Total ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Car, truck, or van: ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Car, truck, or van ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Estimate ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, ":")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, " \\(excluding taxicab\\)")) %>%
      dplyr::mutate(label = stringr::str_replace_all(.data$label, "Worked at home", "Worked from home")) %>%
      dplyr::rename(geography="name", variable="label", metric="concept", geography_type="census_geography") %>%
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)), grouping="PSRC Region", share_moe=0, metric="Mode to Work") %>%
      dplyr::select(-"year")
    
    if(is.null(mode_data)) {mode_data <- modes} else {mode_data <- dplyr::bind_rows(mode_data, modes)}
    
  }
  
  return(mode_data)
}

#' PSRC Cities ACS Work Mode Share
#'
#' This function pulls and cleans ACS Mode Share Data from Census Table B08006.
#' Pulls data by year for cities in the PSRC Region
#' 
#' @param years Four digit integer (or vector of integers) for year(s) of analysis
#' @return tibble of mode to work data by county and region by census year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' modes_psrc_cities <- acs_mode_to_work_place(years <- 2020)
#' 
#' @export
#'
acs_mode_to_work_place <- function(years) {
  
  commute_trips <- c("B08006_001")
  da_trips <- c("B08006_003")
  carpool_trips <- c("B08006_004")
  transit_trips <- c("B08006_008")
  bike_trips <- c("B08006_014")
  walk_trips <- c("B08006_015")
  other_trips <- c("B08006_016")
  wfh_trips <- c("B08006_017")
  
  all_trips <- c(commute_trips, da_trips, carpool_trips, transit_trips, bike_trips, walk_trips, other_trips, wfh_trips)
  non_da_trips <- c(carpool_trips,transit_trips,bike_trips,walk_trips,wfh_trips)
  
  mode_data = NULL
  for (y in years) {
    print(paste0("Working on ",y))
    
    modes <- psrccensus::get_acs_recs(geography = 'place',table.names = c('B08006'), years=y, acs.type = 'acs5') %>% 
      dplyr::filter(.data$variable %in% all_trips) %>%
      dplyr::filter(.data$census_geography == "City")
    
    totals <- modes %>% 
      dplyr::filter(.data$variable %in% commute_trips) %>% 
      dplyr::select("name", "year", "estimate") %>% 
      dplyr::rename(total="estimate")
    
    modes <- dplyr::left_join(modes, totals, by=c("name","year")) %>% 
      dplyr::mutate(share=.data$estimate/.data$total) %>%
      dplyr::select("name", "estimate", "share", "moe", "label", "concept", "census_geography", "year") %>%
      dplyr::mutate(label = stringr::str_replace_all(.data$label, "!!", " ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Estimate Total: ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Estimate Total ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Car, truck, or van: ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Car, truck, or van ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, "Estimate ")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, ":")) %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label, " \\(excluding taxicab\\)")) %>%
      dplyr::mutate(label = stringr::str_replace_all(.data$label, "Worked at home", "Worked from home")) %>%
      dplyr::rename(geography="name", variable="label", metric="concept", geography_type="census_geography") %>%
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)), grouping="PSRC Cities", share_moe=0, metric="Mode to Work") %>%
      dplyr::select(-"year")
    
    if(is.null(mode_data)) {mode_data <- modes} else {mode_data <- dplyr::bind_rows(mode_data, modes)}
    
  }
  
  return(mode_data)
}

#' Metro Regions ACS Work Mode Share
#'
#' This function pulls and cleans ACS Mode Share Data from Census Table B08006.
#' Pulls data by year for Metro Regions across the Nation
#' 
#' @param years Four digit integer (or vector of integers) for year(s) of analysis
#' @return tibble of mode to work data by county and region by census year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' modes_metros <- acs_mode_to_work_metros(years <- 2019)
#' 
#' @export
#'
acs_mode_to_work_metros <- function(years) {
  
  options(dplyr.summarise.inform = FALSE)
  
  # General Mode Data
  commute_trips <- c("B08006_001")
  da_trips <- c("B08006_003")
  carpool_trips <- c("B08006_004")
  transit_trips <- c("B08006_008")
  bike_trips <- c("B08006_014")
  walk_trips <- c("B08006_015")
  other_trips <- c("B08006_016")
  wfh_trips <- c("B08006_017")
  
  all_trips <- c(commute_trips, da_trips, carpool_trips, transit_trips, bike_trips, walk_trips, other_trips, wfh_trips)
  non_da_trips <- c(carpool_trips,transit_trips,bike_trips,walk_trips,wfh_trips)
  
  # Figure of which counties are in each MPO
  mpo_file <- system.file('extdata', 'regional-councils-counties.csv', package='psrcrtp')
  
  mpo <- readr::read_csv(mpo_file, show_col_types = FALSE) %>% 
    dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
    dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
    dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS))
  
  states <- mpo %>% 
    dplyr::select("STATE_FIPS") %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  
  counties <- mpo %>% 
    dplyr::select("GEOID") %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  
  # Get Mode Share Data from ACS using TidyCensus
  mpo_county_data <- NULL
  for(yr in years) {
    print(paste0("Working on mode share data for ",yr))
    
    for (st in states) {
      
      c <- mpo %>% 
        dplyr::filter(.data$STATE_FIPS %in% st) %>% 
        dplyr::select("COUNTY_FIPS") %>% 
        dplyr::pull()
      
      mode <- tidycensus::get_acs(geography = "county", state=st, county=c, table = c('B08006'), year = yr, survey = "acs5") %>% 
        dplyr::select(-"moe") %>% 
        dplyr::mutate(year=yr) %>%
        dplyr::select(-"NAME") %>%
        dplyr::filter(.data$variable %in% all_trips) %>%
        dplyr::mutate(variable = dplyr::case_when(
          .data$variable == "B08006_001" ~ "Total",
          .data$variable == "B08006_003" ~ "Drove alone",
          .data$variable == "B08006_004" ~ "Carpooled",
          .data$variable == "B08006_008" ~ "Public transportation",
          .data$variable == "B08006_014" ~ "Bicycle",
          .data$variable == "B08006_015" ~ "Walked",
          .data$variable == "B08006_016" ~ "Taxicab, motorcycle, or other means",
          .data$variable == "B08006_017" ~ "Worked from home"))
      
      ifelse(is.null(mpo_county_data), mpo_county_data <- mode, mpo_county_data <- dplyr::bind_rows(mpo_county_data,mode))
      
      rm(c, mode)
    }
  }
  
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID", multiple = "all")
  
  metros <- mpo_county_data %>%
    dplyr::select("MPO_AREA", "variable", "estimate", "year") %>%
    dplyr::rename(geography="MPO_AREA") %>%
    dplyr::group_by(.data$geography, .data$variable, .data$year) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(metric="Mode to Work", geography_type="Metro Regions", share=0, moe=0, share_moe=0, grouping="Metro Regions")
  
  totals <- metros %>% 
    dplyr::filter(.data$variable == "Total") %>% 
    dplyr::select("geography", "year", "estimate") %>% 
    dplyr::rename(total="estimate")
  
  metros <- dplyr::left_join(metros, totals, by=c("geography","year")) %>% 
    dplyr::mutate(share=.data$estimate/.data$total) %>%
    dplyr::select(-"total") %>%
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) %>%
    dplyr::select(-"year")
  
  return(metros)
  
}
