#' Annual Transit Metrics from the National Transit Database
#'
#' This function pulls and cleans data from the Monthly NTD Raw Data Release.
#' Data is pulled from https://www.transit.dot.gov/ntd/data-product/monthly-module-raw-data-release
#' 
#' @param ntd_file Sometimes FTA renames the file so you can set the name here - defaults to NULL 
#' @return tibble of transit related metrics for the region and metro areas by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' annual_ntd_data <- process_ntd_data_annual()}
#' 
#' @export
#'
process_ntd_data_annual <- function(ntd_file=NULL) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Passenger Trips, Revenue-Miles and Revenue-Hours tabs
  ntd_tabs <- c("UPT", "VRM", "VRH")
  
  # Figure out which Transit Agencies serve which MPO's
  print("Figuring out which Transit agencies are in which Metro Area.")
  agency_file <- system.file('extdata', 'transit-agency.csv', package='psrcrtp')
  
  agencies <- readr::read_csv(agency_file, show_col_types = FALSE) %>%
    dplyr::mutate(NTDID = stringr::str_pad(string=.data$NTDID, width=5, pad="0", side=c("left")))
  
  ntd_ids <- agencies %>% dplyr::select(.data$NTDID) %>% dplyr::distinct() %>% dplyr::pull()
  
  # NTD Modes to Mode Descriptions
  ntd_modes <- c("AG" = "Rail",
                 "DR-DO" = "Demand Response",
                 "DR-PT" = "Demand Response",
                 "DR-TN" = "Demand Response",
                 "DR-TX" = "Demand Response",
                 "CB" = "Bus",
                 "CC" = "Rail",
                 "CR" = "Commuter Rail",
                 "FB" = "Ferry",
                 "HR" = "Rail",
                 "LR" = "Rail",
                 "MB" = "Bus",
                 "MG" = "Rail",
                 "MO" = "Rail",
                 "RB" = "Bus",
                 "SR" = "Rail",
                 "TB" = "Bus",
                 "TR" = "Rail",
                 "VP" = "Vanpool",
                 "YR" = "Rail")
  
  ntd_modes <- tibble::enframe(ntd_modes) %>% dplyr::rename(Modes=.data$name, mode_name=.data$value)
  
  # Figure out Date to know where to download the file from
  today <- Sys.Date()
  c_yr <- lubridate::year(today)
  c_dy <- lubridate::day(today)
  
  if(c_dy < 7) {
    c_mo <- formatC(as.integer(lubridate::month(today))-1, width=2, flag="0")
    d_mo <- month.name[[as.integer(lubridate::month(today)) - 3]]
    
  } else {
    
    c_mo <- formatC(as.integer(lubridate::month(today)), width=2, flag="0")
    d_mo <- month.name[[as.integer(lubridate::month(today)) - 2]]
    
  }
  
  if (is.null(ntd_file)) {
    data_url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c_yr,"-",c_mo,"/",d_mo,"%20",c_yr,"%20Raw%20Database.xlsx")
  } else {
    data_url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c_yr,"-",c_mo,"/",ntd_file)
  }
  
  print("Downloading NTD Data from FTA Website.")
  utils::download.file(data_url, "working.xlsx", quiet = TRUE, mode = "wb")
  data_file <- paste0(getwd(),"/working.xlsx")
  
  # Initial processing of NTD data
  processed <- NULL
  for (areas in ntd_tabs) {
    print(paste0("Working on ", areas, " data processing and cleanup."))
    
    # Download file and filter data to only include operators for RTP analysis
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data_file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>%
      dplyr::mutate(`5.digit.NTD.ID` = stringr::str_pad(string=.data$`5.digit.NTD.ID`, width=5, pad="0", side=c("left"))) %>%
      dplyr::filter(.data$`5.digit.NTD.ID` %in% ntd_ids) %>% 
      dplyr::mutate(Modes = dplyr::case_when(.data$Modes == "DR" & .data$TOS == "DO" ~ "DR-DO",
                                             .data$Modes == "DR" & .data$TOS == "PT" ~ "DR-PT",
                                             .data$Modes == "DR" & .data$TOS == "TN" ~ "DR-TN",
                                             .data$Modes == "DR" & .data$TOS == "TX" ~ "DR-TX",
                                             TRUE ~ .data$Modes)) %>% 
      dplyr::select(-.data$`4.digit.NTD.ID`, -.data$Active, -.data$Reporter.Type, -.data$UZA, -.data$UZA.Name, -.data$TOS) %>% 
      tidyr::pivot_longer(cols = 4:dplyr::last_col(), names_to = "date", values_to = "estimate", values_drop_na = TRUE)
    
    # Add Detailed Mode Names & Aggregate  
    t <- dplyr::left_join(t, ntd_modes, by=c("Modes")) %>% 
      dplyr::rename(variable=.data$mode_name) %>% 
      dplyr::select(-.data$Modes) %>%
      dplyr::group_by(.data$`5.digit.NTD.ID`, .data$Agency, .data$date, .data$variable) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble()
    
    # Add Metro Area Name
    n <- agencies %>% dplyr::select(.data$NTDID, .data$MPO_AREA, .data$AGENCY_NAME)
    t <- dplyr::left_join(t, n, by=c("5.digit.NTD.ID"="NTDID")) %>%
      dplyr::select(-.data$`5.digit.NTD.ID`, -.data$Agency) %>%
      dplyr::rename(grouping=.data$MPO_AREA, geography=.data$AGENCY_NAME) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(metric=areas) %>%
      dplyr::mutate(metric = dplyr::case_when(.data$metric == "UPT" ~ "Annual Transit Boardings",
                                              .data$metric == "VRM" ~ "Annual Transit Revenue-Miles",
                                              .data$metric == "VRH" ~ "Annual Transit Revenue-Hours")) %>% 
      dplyr::mutate(date = lubridate::parse_date_time(.data$date, 'my'))
    
    rm(n)
    
    # Full year so trim out any data that isn't full year
    max_yr <- t %>% dplyr::select(.data$date) %>% dplyr::distinct() %>% dplyr::pull() %>% max() %>% lubridate::year()
    max_mo <- t %>% dplyr::select(.data$date) %>% dplyr::distinct() %>% dplyr::pull() %>% max() %>% lubridate::month()
    
    if (max_mo <12) {
      yr <- max_yr-1
    } else {
      yr <- max_yr
    }
    
    # Trim Data so it only includes full year data and combine
    t <- t %>% 
      dplyr::filter(lubridate::year(.data$date)<=yr) %>%
      dplyr::mutate(year = lubridate::year(.data$date)) %>%
      dplyr::group_by(.data$year, .data$variable, .data$grouping, .data$geography, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(date = lubridate::ymd(paste0(.data$year,"-12-01"))) %>%
      dplyr::select(-.data$year)
    
    # Metro Areas only need to compare at the total level
    ma <-  t %>%
      dplyr::group_by(.data$date, .data$grouping, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography=.data$grouping, geography_type="Metro Regions", variable="All Transit Modes")
    
    # PSRC Region by Mode
    rm <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::group_by(.data$date, .data$variable, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography="Region", geography_type="PSRC Region", grouping="PSRC Region")
    
    # PSRC Region Total
    rt <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::group_by(.data$date, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography="Region", geography_type="PSRC Region", grouping="PSRC Region", variable="All Transit Modes")
    
    # PSRC Region by Operator and Mode
    rom <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::mutate(geography_type="PSRC Region", grouping="PSRC Region")
    
    # PSRC Region by Operator
    ro <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::group_by(.data$date, .data$geography, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography_type="PSRC Region", grouping="PSRC Region", variable="All Transit Modes")
    
    ifelse(is.null(processed), processed <- dplyr::bind_rows(list(ro,rom,rt,rm,ma)), processed <- dplyr::bind_rows(list(processed,ro,rom,rt,rm,ma)))
  }
  
  # Pivot NTD data wide and create new metric: boardings per revenue-hour
  processed_wide <- processed %>% 
    tidyr::pivot_wider(names_from = .data$metric,
                       values_from = .data$estimate) %>% 
    dplyr::mutate(`Annual Boardings per Hour` = ifelse(.data$`Annual Transit Revenue-Hours` > 0,
                                                round(.data$`Annual Transit Boardings` / .data$`Annual Transit Revenue-Hours`, 2), NA))
  
  # Pivot NTD data back to long and create region-wide estimates per metric
  processed <- processed_wide %>% 
    tidyr::pivot_longer(cols = c(.data$`Annual Transit Boardings`,
                                 .data$`Annual Transit Revenue-Miles`,
                                 .data$`Annual Transit Revenue-Hours`,
                                 .data$`Annual Boardings per Hour`),
                        names_to = "metric",
                        values_to = "estimate")
  
  file.remove(data_file)
  print("All done.")
  
  return(processed)
  
}

#' Year to Date Transit Metrics from the National Transit Database
#'
#' This function pulls and cleans data from the Monthly NTD Raw Data Release.
#' Data is pulled from https://www.transit.dot.gov/ntd/data-product/monthly-module-raw-data-release
#' 
#' @param ntd_file Sometimes FTA renames the file so you can set the name here - defaults to NULL 
#' @return tibble of year to date transit related metrics for the region and metro areas
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' ytd_ntd_data <- process_ntd_data_ytd()}
#' 
#' @export
#'
process_ntd_data_ytd <- function(ntd_file=NULL) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Passenger Trips, Revenue-Miles and Revenue-Hours tabs
  ntd_tabs <- c("UPT", "VRM", "VRH")
  
  # Figure out which Transit Agencies serve which MPO's
  print("Figuring out which Transit agencies are in which Metro Area.")
  agency_file <- system.file('extdata', 'transit-agency.csv', package='psrcrtp')
  
  agencies <- readr::read_csv(agency_file, show_col_types = FALSE) %>%
    dplyr::mutate(NTDID = stringr::str_pad(string=.data$NTDID, width=5, pad="0", side=c("left")))
  
  ntd_ids <- agencies %>% dplyr::select(.data$NTDID) %>% dplyr::distinct() %>% dplyr::pull()
  
  # NTD Modes to Mode Descriptions
  ntd_modes <- c("AG" = "Rail",
                 "DR-DO" = "Demand Response",
                 "DR-PT" = "Demand Response",
                 "DR-TN" = "Demand Response",
                 "DR-TX" = "Demand Response",
                 "CB" = "Bus",
                 "CC" = "Rail",
                 "CR" = "Commuter Rail",
                 "FB" = "Ferry",
                 "HR" = "Rail",
                 "LR" = "Rail",
                 "MB" = "Bus",
                 "MG" = "Rail",
                 "MO" = "Rail",
                 "RB" = "Bus",
                 "SR" = "Rail",
                 "TB" = "Bus",
                 "TR" = "Rail",
                 "VP" = "Vanpool",
                 "YR" = "Rail")
  
  ntd_modes <- tibble::enframe(ntd_modes) %>% dplyr::rename(Modes=.data$name, mode_name=.data$value)
  
  # Figure out Date to know where to download the file from
  today <- Sys.Date()
  c_yr <- lubridate::year(today)
  c_dy <- lubridate::day(today)
  
  if(c_dy < 7) {
    c_mo <- formatC(as.integer(lubridate::month(today))-1, width=2, flag="0")
    d_mo <- month.name[[as.integer(lubridate::month(today)) - 3]]
    
  } else {
    
    c_mo <- formatC(as.integer(lubridate::month(today)), width=2, flag="0")
    d_mo <- month.name[[as.integer(lubridate::month(today)) - 2]]
    
  }
  
  if (is.null(ntd_file)) {
    data_url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c_yr,"-",c_mo,"/",d_mo,"%20",c_yr,"%20Raw%20Database.xlsx")
  } else {
    data_url <- paste0("https://www.transit.dot.gov/sites/fta.dot.gov/files/",c_yr,"-",c_mo,"/",ntd_file)
  }
  
  print("Downloading NTD Data from FTA Website.")
  utils::download.file(data_url, "working.xlsx", quiet = TRUE, mode = "wb")
  data_file <- paste0(getwd(),"/working.xlsx")
  
  # Initial processing of NTD data
  processed <- NULL
  for (areas in ntd_tabs) {
    print(paste0("Working on ", areas, " data processing and cleanup."))
    
    # Download file and filter data to only include operators for RTP analysis
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data_file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>%
      dplyr::mutate(`5.digit.NTD.ID` = stringr::str_pad(string=.data$`5.digit.NTD.ID`, width=5, pad="0", side=c("left"))) %>%
      dplyr::filter(.data$`5.digit.NTD.ID` %in% ntd_ids) %>% 
      dplyr::mutate(Modes = dplyr::case_when(.data$Modes == "DR" & .data$TOS == "DO" ~ "DR-DO",
                                             .data$Modes == "DR" & .data$TOS == "PT" ~ "DR-PT",
                                             .data$Modes == "DR" & .data$TOS == "TN" ~ "DR-TN",
                                             .data$Modes == "DR" & .data$TOS == "TX" ~ "DR-TX",
                                             TRUE ~ .data$Modes)) %>% 
      dplyr::select(-.data$`4.digit.NTD.ID`, -.data$Active, -.data$Reporter.Type, -.data$UZA, -.data$UZA.Name, -.data$TOS) %>% 
      tidyr::pivot_longer(cols = 4:dplyr::last_col(), names_to = "date", values_to = "estimate", values_drop_na = TRUE)
    
    # Add Detailed Mode Names & Aggregate  
    t <- dplyr::left_join(t, ntd_modes, by=c("Modes")) %>% 
      dplyr::rename(variable=.data$mode_name) %>% 
      dplyr::select(-.data$Modes) %>%
      dplyr::group_by(.data$`5.digit.NTD.ID`, .data$Agency, .data$date, .data$variable) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble()
    
    # Add Metro Area Name
    n <- agencies %>% dplyr::select(.data$NTDID, .data$MPO_AREA, .data$AGENCY_NAME)
    t <- dplyr::left_join(t, n, by=c("5.digit.NTD.ID"="NTDID")) %>%
      dplyr::select(-.data$`5.digit.NTD.ID`, -.data$Agency) %>%
      dplyr::rename(grouping=.data$MPO_AREA, geography=.data$AGENCY_NAME) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(metric=areas) %>%
      dplyr::mutate(metric = dplyr::case_when(.data$metric == "UPT" ~ "YTD Transit Boardings",
                                              .data$metric == "VRM" ~ "YTD Transit Revenue-Miles",
                                              .data$metric == "VRH" ~ "YTD Transit Revenue-Hours")) %>% 
      dplyr::mutate(date = lubridate::parse_date_time(.data$date, 'my'))
    
    rm(n)
    
    # Ensure that all data is consistent - find the maximum month for YTD calculations
    max_mo <- t %>% dplyr::select(.data$date) %>% dplyr::distinct() %>% dplyr::pull() %>% max() %>% lubridate::month()
    
    # Trim Data so it only includes ytd for maximum month and combine
    t <- t %>% 
      dplyr::filter(lubridate::month(.data$date)<=max_mo) %>%
      dplyr::mutate(year = lubridate::year(.data$date)) %>%
      dplyr::group_by(.data$year, .data$variable, .data$grouping, .data$geography, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(date = lubridate::ymd(paste0(.data$year,"-",max_mo,"-01"))) %>%
      dplyr::select(-.data$year)
    
    # Metro Areas only need to compare at the total level
    ma <-  t %>%
      dplyr::group_by(.data$date, .data$grouping, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography=.data$grouping, geography_type="Metro Regions", variable="All Transit Modes")
    
    # PSRC Region by Mode
    rm <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::group_by(.data$date, .data$variable, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography="Region", geography_type="PSRC Region", grouping="PSRC Region")
    
    # PSRC Region Total
    rt <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::group_by(.data$date, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography="Region", geography_type="PSRC Region", grouping="PSRC Region", variable="All Transit Modes")
    
    # PSRC Region by Operator and Mode
    rom <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::mutate(geography_type="PSRC Region", grouping="PSRC Region")
    
    # PSRC Region by Operator
    ro <-  t %>%
      dplyr::filter(.data$grouping=="Seattle") %>%
      dplyr::group_by(.data$date, .data$geography, .data$metric) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(geography_type="PSRC Region", grouping="PSRC Region", variable="All Transit Modes")
    
    
    ifelse(is.null(processed), processed <- dplyr::bind_rows(list(ro,rom,rt,rm,ma)), processed <- dplyr::bind_rows(list(processed,ro,rom,rt,rm,ma)))
  }
  
  # Pivot NTD data wide and create new metric: boardings per revenue-hour
  processed_wide <- processed %>% 
    tidyr::pivot_wider(names_from = .data$metric,
                       values_from = .data$estimate) %>% 
    dplyr::mutate(`YTD Boardings per Hour` = ifelse(.data$`YTD Transit Revenue-Hours` > 0,
                                                    round(.data$`YTD Transit Boardings` / .data$`YTD Transit Revenue-Hours`, 2), NA))
  
  # Pivot NTD data back to long and create region-wide estimates per metric
  processed <- processed_wide %>% 
    tidyr::pivot_longer(cols = c(.data$`YTD Transit Boardings`,
                                 .data$`YTD Transit Revenue-Miles`,
                                 .data$`YTD Transit Revenue-Hours`,
                                 .data$`YTD Boardings per Hour`),
                        names_to = "metric",
                        values_to = "estimate")
  
  file.remove(data_file)
  print("All done.")
  
  return(processed)
  
}
