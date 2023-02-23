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
#' modes_psrc <- acs_mode_to_work_county(years <- 2021)
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
      dplyr::rename(geography="name", variable="label", metric="concept", geography_type="census_geography") %>%
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)), grouping="PSRC Region", share_moe=0, metric="Mode to Work") %>%
      dplyr::select(-"year")
    
    if(is.null(mode_data)) {mode_data <- modes} else {mode_data <- dplyr::bind_rows(mode_data, modes)}
    
  }
  
  return(mode_data)
}
