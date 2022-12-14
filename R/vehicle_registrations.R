#' New Vehicle Registrations by Electrification Level
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is pulled from https://data.wa.gov/ via the Socrata API
#' 
#' @param mo List of months as integers - defaults 1,4,7 and 10 
#' @param yrs List of years as 4 digit integers from 2020 onward - defaults to 2020-2022
#' @param counties List of County Names - defaults to PSRC counties
#' @return tibble of new vehicle registrations by electrification level by month and county
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' new_veh <- new_vehicle_registrations(mo=c(4), yrs=c(2022))
#' 
#' @export
#'
new_vehicle_registrations <- function(mo=c(1,4,7,10), 
                                      yrs=c(2020,2021,2022), 
                                      counties=c("King","Kitsap","Pierce","Snohomish")) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  new_registrations=NULL
  for (y in yrs) {
    
    for (m in mo) {
      rtp_token <- "tjnJfQzL0SfZJ1cbT0iiCUpO3"
      url <- paste0("https://data.wa.gov/resource/brw6-jymh.json?transaction_type=Original%20Registration&start_of_month=",y,"-",m,"-01T00:00:00.000")
      print(paste0("Downloading Data from ",m,"-",y,". Be patient, there is a lot of data being downloaded so it is a bit slow."))
      tbl <- dplyr::as_tibble(RSocrata::read.socrata(url, app_token = rtp_token))
      
      # Get Data by County
      c <-  tbl %>%
        dplyr::mutate(start_of_month = lubridate::ymd(.data$start_of_month)) %>%
        dplyr::select(.data$start_of_month, .data$electrification_level, .data$county) %>%
        dplyr::filter(.data$county %in% counties) %>%
        dplyr::mutate(estimate=1) %>%
        dplyr::rename(date=.data$start_of_month, geography=.data$county, variable=.data$electrification_level) %>%
        dplyr::mutate(variable=stringr::str_replace_all(.data$variable, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "HEV \\(Hybrid Electric Vehicle\\)")) %>%
        dplyr::mutate(variable=stringr::str_replace_all(.data$variable, "Mild HEV \\(Hybrid Electric Vehicle\\)", "HEV \\(Hybrid Electric Vehicle\\)")) %>%
        dplyr::mutate(variable=stringr::str_replace_all(.data$variable, "Strong HEV \\(Hybrid Electric Vehicle\\)", "HEV \\(Hybrid Electric Vehicle\\)")) %>%
        dplyr::group_by(.data$date, .data$geography, .data$variable) %>%
        dplyr::summarise(estimate=sum(.data$estimate)) %>%
        tidyr::as_tibble() %>%
        dplyr::mutate(geography_type="County")
      
      # Get Regional Total
      r <- c %>%
        dplyr::select(-.data$geography, -.data$geography_type) %>%
        dplyr::group_by(.data$date, .data$variable) %>%
        dplyr::summarise(estimate=sum(.data$estimate)) %>%
        tidyr::as_tibble() %>%
        dplyr::mutate(geography="Region") %>%
        dplyr::mutate(geography_type="PSRC Region")
      
      # Combine and Get Totals for Shares of Registrations
      c <- dplyr::bind_rows(c,r)
      
      t <- c %>%
        dplyr::select(-.data$variable) %>%
        dplyr::group_by(.data$date, .data$geography, .data$geography_type) %>%
        dplyr::summarise(total=sum(.data$estimate)) %>%
        tidyr::as_tibble()
      
      c <- dplyr::left_join(c,t,by=c("date","geography", "geography_type")) %>%
        dplyr::mutate(share = .data$estimate / .data$total) %>%
        dplyr::select(-.data$total) %>%
        dplyr::mutate(metric="New Vehicle Registrations", grouping="All")
      
      # Get Data by Zipcode
      z <- tbl %>%
        dplyr::mutate(start_of_month = lubridate::ymd(.data$start_of_month)) %>%
        dplyr::select(.data$start_of_month, .data$electrification_level, .data$county, .data$zip_code) %>%
        dplyr::filter(.data$county %in% counties) %>%
        dplyr::select(-.data$county) %>%
        dplyr::mutate(estimate=1) %>%
        dplyr::rename(date=.data$start_of_month, geography=.data$zip_code, variable=.data$electrification_level) %>%
        dplyr::mutate(variable=stringr::str_replace_all(.data$variable, "HEV \\(Hybrid Electric Vehicle\\) - Level Unknown", "HEV \\(Hybrid Electric Vehicle\\)")) %>%
        dplyr::mutate(variable=stringr::str_replace_all(.data$variable, "Mild HEV \\(Hybrid Electric Vehicle\\)", "HEV \\(Hybrid Electric Vehicle\\)")) %>%
        dplyr::mutate(variable=stringr::str_replace_all(.data$variable, "Strong HEV \\(Hybrid Electric Vehicle\\)", "HEV \\(Hybrid Electric Vehicle\\)")) %>%
        dplyr::group_by(.data$date, .data$geography, .data$variable) %>%
        dplyr::summarise(estimate=sum(.data$estimate)) %>%
        tidyr::as_tibble() %>%
        dplyr::mutate(geography_type="Zipcode")
      
      # Get Totals for Shares of Registrations
      t <- z %>%
        dplyr::select(-.data$variable) %>%
        dplyr::group_by(.data$date, .data$geography, .data$geography_type) %>%
        dplyr::summarise(total=sum(.data$estimate)) %>%
        tidyr::as_tibble()
      
      z <- dplyr::left_join(z,t,by=c("date","geography", "geography_type")) %>%
        dplyr::mutate(share = .data$estimate / .data$total) %>%
        dplyr::select(-.data$total) %>%
        dplyr::mutate(metric="New Vehicle Registrations", grouping="All")
      
      ifelse(is.null(new_registrations), new_registrations <- dplyr::bind_rows(c,z), new_registrations <- dplyr::bind_rows(list(new_registrations,c,z)))
      
    } # end of months loop
  } # end of years loop
  
  return(new_registrations)
}

#' All Vehicle Registrations by Electrification Level
#'
#' This function pulls and cleans data from the Washington State Registration Data.
#' Data is pulled from https://data.wa.gov/ via the Socrata API
#' 
#' @param st List of states using two digit abbreviation - defaults WA 
#' @param counties List of County Names - defaults to PSRC counties
#' @return tibble of all vehicle registrations by electrification level by month and county
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' all_veh <- total_vehicle_registrations()
#' 
#' @export
#'
total_vehicle_registrations <- function(st=c("WA"), counties=c("King","Kitsap","Pierce","Snohomish")) {
  
  # Download ZEV Licensing Data and Filter to Counties
  d <- dplyr::as_tibble(RSocrata::read.socrata("https://data.wa.gov/resource/3d5d-sdqb.json", 
                                               app_token = "plpj1IICGOhTVjmebF1kls9Cf")) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    dplyr::filter(.data$state %in% st & .data$county %in% counties) %>%
    tidyr::pivot_longer(cols=c("battery_electric_vehicles_bevs_", 
                               "plug_in_hybrid_electric_vehicles_phevs_",
                               "electric_vehicle_ev_total",
                               "non_electric_vehicles",
                               "total_vehicles"),
                        names_to="variable", values_to="estimate") %>%
    dplyr::filter(.data$variable != "electric_vehicle_ev_total") %>%
    dplyr::select(-.data$state, -.data$vehicle_primary_use,-.data$percent_electric_vehicles) %>%
    dplyr::mutate(estimate = as.integer(.data$estimate)) %>%
    dplyr::group_by(.data$date, .data$county, .data$variable) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::rename(geography=.data$county) %>%
    dplyr::mutate(geography_type="County")
  
  # Add Region Total
  r <- d %>%
    dplyr::select(-.data$geography, -.data$geography_type) %>%
    dplyr::group_by(.data$date, .data$variable) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(geography="Region") %>%
    dplyr::mutate(geography_type="PSRC Region")
  
  d <- dplyr::bind_rows(d, r) %>%
    dplyr::mutate(metric="Total Vehicle Registrations") %>%
    dplyr::mutate(grouping="All")
  
  # Figure out Shares of Each Vehicle Type
  t <- d %>% 
    dplyr::filter(.data$variable == "total_vehicles") %>%
    dplyr::select(.data$date, .data$geography, .data$geography_type, .data$estimate) %>%
    dplyr::rename(total = .data$estimate)
  
  d <- dplyr::left_join(d, t, by=c("date", "geography", "geography_type")) %>%
    dplyr::mutate(share = .data$estimate / .data$total) %>%
    dplyr::select(-.data$total) %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "battery_electric_vehicles_bevs_", "BEV \\(Battery Electric Vehicle\\)")) %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "non_electric_vehicles", "ICE \\(Internal Combustion Engine\\)")) %>%
    dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "plug_in_hybrid_electric_vehicles_phevs_", "PHEV \\(Plug-in Hybrid Electric Vehicle\\)")) %>%
    dplyr::filter(.data$variable != "total_vehicles")
  
  return(d)
}
