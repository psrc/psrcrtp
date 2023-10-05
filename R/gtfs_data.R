#' GTFS Route Data
#'
#' This function pulls and cleans GTFS Route data from the PSRC server for use in further transit analysis.
#' 
#' @param year Year of gtfs data as a four digit integer - available from 2015 to current year
#' @param service_change Service Change period - either Fall or Spring 
#' @return tibble of gtfs route level data by service period year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' routes_2015 <- transit_routes(year=2015, service_change = "Spring")}
#' 
#' @export
#'
transit_routes <- function(year, service_change) {
  
  if (tolower(service_change)=="spring") {data_month = "04"} else (data_month = "10")
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- paste0("X:DSA/shiny-uploads/gtfs/",tolower(service_change),"_",as.character(year),".zip")
  
  st_express <- c("510","511","512","513","522","532","535","540","541","542","544","545",
                  "550","554","555","556","560","566","567","574","577", "578", "580","586", 
                  "590","592","594","595","596")
  
  brt_routes <- c("701","702", "Swift", "Swift Blue", "Swift Green",
                  "A Line", "B Line", "C Line", "D Line", "E Line", "F Line")
  
  # Open Regional GTFS File
  print(paste0("Opening the ",service_change, " ", year, " GTFS archive to get routes."))
  gtfs <- tidytransit::read_gtfs(path=gtfs_file, files = c("routes"))
  
  # Clean Up Routes for ST Express, BRT and other Modes
  print(paste0("Cleaning the ",service_change, " ", year, " GTFS routes to capture Agency names, transit type and ST Express routes."))
  routes <- tidyr::as_tibble(gtfs$routes) %>% 
    dplyr::mutate(route_short_name=as.character(.data$route_short_name)) %>%
    dplyr::mutate(route_short_name=stringr::str_replace_all(.data$route_short_name, "\\.0","")) %>%
    # Cleanup Agency Names for ST Express Routes
    dplyr::mutate(agency_name=dplyr::case_when(
      route_short_name %in% st_express ~ "Sound Transit",
      !(route_short_name %in% st_express) ~ agency_id)) %>%
    # Set Agency Names
    dplyr::mutate(agency_name=dplyr::case_when(
      agency_name %in% c("29") ~ "Community Transit",
      agency_name %in% c("3") ~ "Pierce Transit",
      agency_name %in% c("97") ~ "Everett Transit",
      agency_name %in% c("EOS","23") ~ "City of Seattle",
      agency_name %in% c("KCM","1") ~ "King County Metro",
      agency_name %in% c("KMD") ~ "King County Marine Division",
      agency_name %in% c("kt") ~ "Kitsap Transit",
      agency_name %in% c("Sound Transit","","40","ST") ~ "Sound Transit",
      agency_name %in% c("WSF") ~ "Washington State Ferries")) %>%
    # Get Route Description and Long Names Consistent
    dplyr::mutate(route_long_name=dplyr::case_when(
      route_long_name == "" ~ route_desc,
      route_long_name != "" ~ route_long_name)) %>%
    # Flag BRT Routes
    dplyr::mutate(transit_type=dplyr::case_when(
      route_short_name %in% brt_routes ~ "BRT",
      !(route_short_name %in% brt_routes) ~ "0")) %>%
    dplyr::mutate(transit_type=dplyr::case_when(
      transit_type == "BRT" ~ "BRT",
      route_type == 0 ~ "Light Rail or Streetcar",
      route_type == 2 ~ "Commuter Rail",
      route_type == 3 ~ "Bus",
      route_type == 4 ~ "Ferry")) %>%
    # Remove Extra Attributes
    dplyr::select("route_id", "route_short_name", "route_long_name", "agency_name", "transit_type") %>%
    # Add Date to results
    dplyr::mutate(date=lubridate::mdy(paste0(data_month,"-01-",year)))
  
  print("All done.")
  
  return(routes)
}

#' GTFS Stops by Mode
#'
#' This function pulls and cleans GTFS Stop data from the PSRC server for use in further transit analysis.
#' 
#' @param year Year of gtfs data as a four digit integer - available from 2015 to current year
#' @param service_change Service Change period - either Fall or Spring 
#' @return tibble of gtfs stop level data by service period year and mode
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' stops_2021 <- transit_stops_by_mode(year=2021, service_change = "Spring")}
#' 
#' @export
#'
transit_stops_by_mode <- function(year, service_change) {
  
  if (tolower(service_change)=="spring") {data_month = "04"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- paste0("X:DSA/shiny-uploads/gtfs/",tolower(service_change),"_",as.character(year),".zip")
  
  # Get Route Details to get modes on the routes
  routes <- psrcrtp::transit_routes(year, service_change) %>%
    dplyr::select("route_id", "agency_name", "transit_type")
  
  # Open Regional GTFS File for stop times
  print(paste0("Opening the ",service_change, " ", year, " GTFS archive to get Stop Times to get unique stops by route."))
  gtfs <- tidytransit::read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times"))
  
  # Clean Up Stop Times to get routes and mode by stops served
  print(paste0("Cleaning the ",service_change, " ", year, " GTFS Stop Times to capture stps by Agency and Mode."))
  
  stoptimes <- tidyr::as_tibble(gtfs$stop_times) %>%
    dplyr::select("trip_id", "stop_id")
  
  # Need to use trips to get route id for use in stop times
  trips <- tidyr::as_tibble(gtfs$trips) %>%
    dplyr::select("trip_id", "route_id")
  
  trips <- dplyr::left_join(trips, routes, by=c("route_id"))
  
  # Get Coordinates from Stops
  stops <- tidyr::as_tibble(gtfs$stops) %>%
    dplyr::select("stop_id", "stop_name", "stop_lat", "stop_lon")
  
  # Get Mode and agency from trips to stops
  print(paste0("Getting unqiue stop list by modes for the ",service_change, " ", year, " GTFS archive."))
  final_stops <- dplyr::left_join(stoptimes, trips, by=c("trip_id")) %>%
    dplyr::select(-"trip_id", -"route_id") %>%
    dplyr::distinct()
  
  final_stops <- dplyr::left_join(final_stops, stops, by=c("stop_id")) %>%
    dplyr::mutate(date=lubridate::mdy(paste0(data_month,"-01-",year)))
  
  print("All Done.")
  
  return(final_stops)
}

#' Census Blocks served by Transit
#'
#' This function pulls GTFS data by Mode and intersect with Blocks to determine which blocks are served by transit.
#' 
#' @param year Year as a four digit integer - available from 2015 to current year
#' @param service_change Service Change period - either Fall or Spring - defaults to Spring
#' @param buffer Buffer Distance express in miles - defaults to 0.5
#' @param modes Either HCT or All for transit serving the blocks - defaults to HCT
#' @return character vector of census block id's served by transit modes
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' hct_blocks <- blocks_served_by_transit(year=2022)}
#' 
#' @export
#'

blocks_served_by_transit <- function(year, service_change="Spring", buffer=0.5, modes="HCT") {
  
  wgs84 <- 4326
  spn <- 2285 
  
  if (modes=="HCT"){
    
    transit_modes <- c("BRT", "Commuter Rail", "Ferry", "Light Rail or Streetcar")
    
  } else {
    
    transit_modes <- c("BRT", "Bus", "Commuter Rail", "Ferry", "Light Rail or Streetcar")
    
  }
  
  print(paste0("Processing ", modes, " stops data for ", service_change, " of ", year))
  stops <- psrcrtp::transit_stops_by_mode(year=year, service_change = service_change) %>%
    dplyr::filter(.data$transit_type %in% transit_modes) %>%
    dplyr::select("stop_id", "stop_lat", "stop_lon") %>%
    dplyr::distinct()
  
  
  print(paste0("Buffering stops by ", buffer, " miles and dissolving."))
  stops_layer <- sf::st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% 
    sf::st_transform(spn) %>%
    sf::st_buffer(dist = buffer*5280) %>%
    sf::st_union() %>% 
    sf::st_sf() %>%
    dplyr::mutate(transit_type = "HCT")
  
  if (year >=2020) { 
    
    census_year <- 2020
    print(paste0("Downloading ", census_year, " Census Blocks - this can take a minute getting from Elmergeo"))
    block_layer <- psrcelmer::st_read_elmergeo(layer_name = "block2020", project_to_wgs84 = FALSE)
    
    block_layer <- block_layer %>%
      dplyr::select("geoid20") %>%
      sf::st_transform(spn) %>%
      dplyr::rename(geoid="geoid20")
    
  } else {
    
    census_year <- 2010
    print(paste0("Downloading ", census_year, " Census Blocks - this can take a minute getting from Elmergeo"))
    block_layer <- psrcelmer::st_read_elmergeo(layer_name = "block2010", project_to_wgs84 = FALSE)
    
    block_layer <- block_layer %>%
      dplyr::select("geoid10") %>%
      sf::st_transform(spn) %>%
      dplyr::rename(geoid="geoid10")
    
  }
  
  print(paste0("Intersecting Blocks and Transit Stops and outputting list of Blocks served by ", modes, " stops."))
  blocks <- sf::st_intersection(stops_layer, block_layer) %>% 
    sf::st_drop_geometry() %>%
    dplyr::select("geoid") %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  return(blocks)
  
}

#' Equity Population served by Transit
#'
#' This function pulls GTFS data by Mode, intersects with Blocks and calculates how many people are served by transit.
#' 
#' @param year Year as a four digit integer - available from 2015 to current year
#' @param service_change Service Change period - either Fall or Spring - defaults to Spring
#' @param modes Either HCT or All for transit serving the blocks - defaults to HCT
#' @param latest_census_year Four digit integer for latest ACS 5 yr data - defaults to 2021
#' @param buffer Buffer Distance express in miles - defaults to 0.50
#' @return tibble of population and share of people serve by transit by equity focus population
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' pop_half_mile_hct <- equity_population_near_transit(year=2022)
#' pop_qtr_mile_hct <- equity_population_near_transit(year=2022, buffer=0.25)}
#' 
#' @export
#'
equity_population_near_transit <- function(year, service_change="Spring", modes="HCT", latest_census_year=2021, buffer=0.50) {
  
  print("Getting Equity Population by Census Block")
  pop_by_block_efa <- psrcrtp::population_by_efa(year=year, latest_census_year = latest_census_year)
  
  print("Getting list of blocks near transit")
  transit_blocks <- psrcrtp::blocks_served_by_transit(year=year, service_change=service_change, buffer=buffer, modes=modes)
  
  print(paste0("Calculating Population within ", buffer, " miles of ", modes, " transit."))
  blocks_near_transit <-pop_by_block_efa %>%
    dplyr::filter(.data$geography %in% transit_blocks) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble()
  
  print("Calculating Total Population for the entire Region by Equity Focus Areafor share calculations.")
  all_blocks <-pop_by_block_efa %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::rename(total="estimate")
  
  print("Creating final output")
  pop <- dplyr::left_join(blocks_near_transit, all_blocks, by=c("variable")) %>%
    dplyr::mutate(share=.data$estimate/.data$total) %>%
    dplyr::mutate(geography="Region", geography_type="PSRC Region") %>%
    dplyr::mutate(metric=paste0("Population near ",modes," transit")) %>%
    dplyr::mutate(grouping=paste0(buffer, " mile buffer distance")) %>%
    dplyr::mutate(date=lubridate::mdy(paste0("04-01-",year))) %>%
    dplyr::select(-"total")
  
  return(pop)
}
