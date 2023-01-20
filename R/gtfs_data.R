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
#' routes_2015 <- transit_routes(year=2015, service_change = "Spring")
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
#' stops_2021 <- transit_stops_by_mode(year=2021, service_change = "Spring")
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
