#' Transit Metrics from the National Transit Database
#'
#' This function cleans data from the Monthly NTD Raw Data Release.
#' Data is downloaded from https://www.transit.dot.gov/ntd/data-product/monthly-module-raw-data-release
#' 
#' @return tibble of transit related metrics for the region and metro areas
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' transit_data <- process_ntd_data()}
#' 
#' @export
#'
process_ntd_data <- function() {
  
  # Location of the most recently downloaded NTD file
  file_dir <- "X:/DSA/rtp-dashboard/NTD/"
  setwd(file_dir)
  
  # Choose NTD file
  data_file <- file.choose()
  
  agency_file <- system.file('extdata', 'transit-agency.csv', package='psrcrtp')
  #agency_file <- paste0("C:/coding/psrcrtp/inst/extdata/transit-agency.csv")
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Passenger Trips, Revenue-Miles and Revenue-Hours tabs
  ntd_tabs <- c("UPT", "VRM", "VRH")
  
  # Figure out which Transit Agencies serve which MPO's
  print("Figuring out which Transit agencies are in which Metro Area.")
  agencies <- readr::read_csv(agency_file, show_col_types = FALSE) |>
    dplyr::mutate(NTDID = stringr::str_pad(string=.data$NTDID, width=5, pad="0", side=c("left"))) |>
    dplyr::mutate(UACE = stringr::str_pad(string=.data$UACE, width=5, pad="0", side=c("left")))
  
  ntd_ids <- agencies |> dplyr::select("NTDID") |> dplyr::distinct() |> dplyr::pull()
  
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
  
  ntd_modes <- tibble::enframe(ntd_modes) |> dplyr::rename(Mode="name", mode_name="value")
  
  # Initial processing of NTD data
  processed <- NULL
  for (areas in ntd_tabs) {
    print(paste0("Working on ", areas, " data processing and cleanup."))
    
    # Open file and filter data to only include operators for RTP analysis
    t <- dplyr::as_tibble(openxlsx::read.xlsx(data_file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) |>
      dplyr::mutate(NTD.ID = stringr::str_pad(string=.data$NTD.ID, width=5, pad="0", side=c("left"))) |>
      dplyr::filter(.data$NTD.ID %in% ntd_ids) |> 
      dplyr::mutate(Mode = dplyr::case_when(.data$Mode == "DR" & .data$TOS == "DO" ~ "DR-DO",
                                            .data$Mode == "DR" & .data$TOS == "PT" ~ "DR-PT",
                                            .data$Mode == "DR" & .data$TOS == "TN" ~ "DR-TN",
                                            .data$Mode == "DR" & .data$TOS == "TX" ~ "DR-TX",
                                            TRUE ~ .data$Mode)) |> 
      dplyr::select(-"Legacy.NTD.ID", -"Status", -"Reporter.Type", -"UACE.CD", -"UZA.Name", -"TOS", -"3.Mode") |> 
      tidyr::pivot_longer(cols = 4:dplyr::last_col(), names_to = "date", values_to = "estimate", values_drop_na = TRUE) |> 
      dplyr::mutate(date = lubridate::my(.data$date))
    
    # Add Detailed Mode Names & Aggregate  
    t <- dplyr::left_join(t, ntd_modes, by=c("Mode")) |> 
      dplyr::rename(variable="mode_name") |> 
      dplyr::select(-"Mode") |>
      dplyr::group_by(.data$NTD.ID, .data$Agency, .data$date, .data$variable) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble()
    
    # Add Metro Area Name
    n <- agencies |> dplyr::select("NTDID", "MPO_AREA", "AGENCY_NAME")
    t <- dplyr::left_join(t, n, by=c("NTD.ID"="NTDID"), relationship = "many-to-many") |>
      dplyr::select(-"NTD.ID", -"Agency") |>
      dplyr::rename(grouping="MPO_AREA", geography="AGENCY_NAME") |>
      tidyr::as_tibble() |>
      dplyr::mutate(metric=areas) |>
      dplyr::mutate(metric = dplyr::case_when(.data$metric == "UPT" ~ "Boardings",
                                              .data$metric == "VRM" ~ "Revenue-Miles",
                                              .data$metric == "VRH" ~ "Revenue-Hours"))
    
    rm(n)
    
    #########################################################################################################
    ### Full Year Data
    #########################################################################################################
    
    max_yr <- t |> dplyr::select("date") |> dplyr::distinct() |> dplyr::pull() |> max() |> lubridate::year()
    max_mo <- t |> dplyr::select("date") |> dplyr::distinct() |> dplyr::pull() |> max() |> lubridate::month()
    
    if (max_mo <12) {
      yr <- max_yr-1
    } else {
      yr <- max_yr
    }
    
    # Trim Data so it only includes full year data and combine
    full_yr <- t |>
      dplyr::filter(lubridate::year(.data$date)<=yr) |>
      dplyr::mutate(year = lubridate::year(.data$date)) |>
      dplyr::group_by(.data$year, .data$variable, .data$grouping, .data$geography, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(date = lubridate::ymd(paste0(.data$year,"-12-01"))) |>
      dplyr::select(-"year")
    
    # Metro Areas only need to compare at the total level
    metro_total <-  full_yr |>
      dplyr::group_by(.data$date, .data$grouping, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography=.data$grouping, geography_type="Metro Areas", variable="All Transit Modes", grouping="Annual")
    
    # PSRC Region by Mode
    region_modes <-  full_yr |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::group_by(.data$date, .data$variable, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography="Region", geography_type="Region", grouping="Annual")
    
    # PSRC Region Total
    region_total <-  full_yr |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::group_by(.data$date, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography="Region", geography_type="Region", grouping="Annual", variable="All Transit Modes")
    
    # PSRC Region by Operator and Mode
    region_operator_modes <-  full_yr |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::mutate(geography_type="Transit Operator", grouping="Annual")
    
    # PSRC Region by Operator
    region_operator <-  full_yr |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::group_by(.data$date, .data$geography, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography_type="Transit Operator", grouping="Annual", variable="All Transit Modes")
    
    ifelse(is.null(processed), 
           processed <- dplyr::bind_rows(list(region_operator,region_operator_modes,
                                              region_total,region_modes,
                                              metro_total)), 
           processed <- dplyr::bind_rows(list(processed,
                                              region_operator,region_operator_modes,
                                              region_total,region_modes,
                                              metro_total)))
    
    #########################################################################################################
    ### Year to Date
    #########################################################################################################
    
    # Ensure that all data is consistent - find the maximum month for YTD calculations
    max_mo <- t |> dplyr::select("date") |> dplyr::distinct() |> dplyr::pull() |> max() |> lubridate::month()
    
    # Trim Data so it only includes ytd for maximum month and combine
    ytd <- t |>
      dplyr::filter(lubridate::month(.data$date)<=max_mo) |>
      dplyr::mutate(year = lubridate::year(.data$date)) |>
      dplyr::group_by(.data$year, .data$variable, .data$grouping, .data$geography, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(date = lubridate::ymd(paste0(.data$year,"-",max_mo,"-01"))) |>
      dplyr::select(-"year")
    
    # Metro Areas only need to compare at the total level
    metro_total <-  ytd |>
      dplyr::group_by(.data$date, .data$grouping, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography=.data$grouping, geography_type="Metro Areas", variable="All Transit Modes", grouping="YTD")
    
    # PSRC Region by Mode
    region_modes <-  ytd |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::group_by(.data$date, .data$variable, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography="Region", geography_type="Region", grouping="YTD")
    
    # PSRC Region Total
    region_total <-  ytd |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::group_by(.data$date, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography="Region", geography_type="Region", grouping="YTD", variable="All Transit Modes")
    
    # PSRC Region by Operator and Mode
    region_operator_modes <-  ytd |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::mutate(geography_type="Transit Operator", grouping="YTD")
    
    # PSRC Region by Operator
    region_operator <-  ytd |>
      dplyr::filter(.data$grouping=="Seattle") |>
      dplyr::group_by(.data$date, .data$geography, .data$metric) |>
      dplyr::summarise(estimate=sum(.data$estimate)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(geography_type="Transit Operator", grouping="YTD", variable="All Transit Modes")
    
    processed <- dplyr::bind_rows(list(processed,
                                       region_operator,region_operator_modes,
                                       region_total,region_modes,
                                       metro_total))
    
  }
  
  rm(region_operator,region_operator_modes,region_total,region_modes,metro_total, ytd, t, full_yr)
  
  # Pivot NTD data wide and create new metric: boardings per revenue-hour
  processed_wide <- processed |> 
    tidyr::pivot_wider(names_from = "metric",
                       values_from = "estimate") |> 
    dplyr::mutate(`Boardings-per-Hour` = ifelse(.data$`Revenue-Hours` > 0,
                                                round(.data$`Boardings` / .data$`Revenue-Hours`, 2), NA))
  
  # Pivot NTD data back to long and create region-wide estimates per metric
  processed <- processed_wide |> 
    tidyr::pivot_longer(cols = c("Boardings",
                                 "Revenue-Miles",
                                 "Revenue-Hours",
                                 "Boardings-per-Hour"),
                        names_to = "metric",
                        values_to = "estimate")
  
  processed <- processed |> 
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate") |>
    tidyr::drop_na() |>
    dplyr::filter(.data$estimate >0)
  
  print("All done.")
  
  return(processed)
  
}
