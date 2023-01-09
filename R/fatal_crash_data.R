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

#' National Traffic Fatalities from FARS
#'
#' This function pulls and cleans data from yearly fatal traffic collision data from the FARS Data.
#' 
#' @param safety_yrs List of four digit integers for years of analysis- defaults to 2010 to 2020 
#' 
#' @return tibble of fatal collision metrics for the region and mpos by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' annual_fars_data <- process_fars_data()}
#' 
#' @export
#'
process_fars_data<- function(safety_yrs=c(seq(2010,2020,by=1))) {
  
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
  
  # Get Population Data from ACS using TidyCensus
  mpo_county_data <- NULL
  for(yr in safety_yrs) {
    print(paste0("Working of population data for ",yr))
    
    for (st in states) {
      
      c <- mpo %>% 
        dplyr::filter(.data$STATE_FIPS %in% st) %>% 
        dplyr::select("COUNTY_FIPS") %>% 
        dplyr::pull()
      
      pop <- tidycensus::get_acs(geography = "county", state=st, county=c, variables = c("B03002_001"), year = yr, survey = "acs5") %>% 
        dplyr::select(-"moe") %>% 
        dplyr::mutate(data_year=yr, variable="Population") %>%
        dplyr::select(-"NAME")
      
      ifelse(is.null(mpo_county_data), mpo_county_data <- pop, mpo_county_data <- dplyr::bind_rows(mpo_county_data,pop))
      
      rm(c, pop)
    }
  }
  
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID")
  
  psrc <- mpo_county_data %>% 
    dplyr::filter(.data$MPO_FIPS=="PSRC") %>%
    dplyr::select("COUNTY_NAME", "variable", "estimate", "data_year") %>%
    dplyr::rename(geography="COUNTY_NAME") %>%
    dplyr::mutate(geography=paste0(.data$geography, " County")) %>%
    dplyr::mutate(metric="Fatality Rate", geography_type="PSRC Region", share=0, moe=0, share_moe=0, grouping="All")
  
  metros <- mpo_county_data %>%
    dplyr::select("MPO_AREA", "variable", "estimate", "data_year") %>%
    dplyr::rename(geography="MPO_AREA") %>%
    dplyr::group_by(.data$geography, .data$variable, .data$data_year) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(metric="Fatality Rate", geography_type="Metro Regions", share=0, moe=0, share_moe=0, grouping="All")
  
  fars_data <- dplyr::bind_rows(psrc,metros)
  rm(mpo_county_data, psrc, metros)
  
  # Fatality Data
  collision_data <- NULL
  min_yr <- min(safety_yrs)
  max_yr <- max(safety_yrs)
  
  fars_yrs <- c(seq(min_yr-4,max_yr,by=1))
  
  for (y in fars_yrs) {
    
    # Open Current Years FARS Accident Data
    
    all_files <- as.character(utils::unzip(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), list = TRUE)$Name)
    
    print(paste0("Working of FARS Fatalities data for ",y))
    f <- readr::read_csv(unz(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), all_files[1]), show_col_types = FALSE) %>%
      dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY, width=3, side=c("left"), pad="0")) %>%
      dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE, width=2, side=c("left"), pad="0")) %>%
      dplyr::mutate(GEOID = paste0(.data$STATE_FIPS, .data$COUNTY_FIPS)) %>%
      dplyr::filter(.data$GEOID %in% counties) %>%
      dplyr::select("GEOID", "FATALS") %>%
      dplyr::group_by(.data$GEOID) %>%
      dplyr::summarise(estimate=sum(.data$FATALS)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(data_year=y, variable="Fatalities")
    
    print(paste0("Working of FARS Fatal Collisions for ",y))
    c <- readr::read_csv(unz(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), all_files[1]), show_col_types = FALSE) %>%
      dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY, width=3, side=c("left"), pad="0")) %>%
      dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE, width=2, side=c("left"), pad="0")) %>%
      dplyr::mutate(GEOID = paste0(.data$STATE_FIPS, .data$COUNTY_FIPS)) %>%
      dplyr::filter(.data$GEOID %in% counties) %>%
      dplyr::select("GEOID") %>%
      dplyr::mutate(FATAL_COLLISIONS = 1) %>%
      dplyr::group_by(.data$GEOID) %>%
      dplyr::summarise(estimate =sum(.data$FATAL_COLLISIONS)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(data_year=y, variable="Fatal Collisions")
    
    c <- dplyr::bind_rows(c,f)
    
    ifelse(is.null(collision_data), collision_data <- c, collision_data <- dplyr::bind_rows(collision_data,c))
    
    rm(c,f)
    
  }
  
  # Annual Fatality Data
  mpo_county_data <- dplyr::left_join(mpo, collision_data, by="GEOID")
  
  psrc <- mpo_county_data %>% 
    dplyr::filter(.data$MPO_FIPS=="PSRC") %>%
    dplyr::select("COUNTY_NAME", "variable", "estimate", "data_year") %>%
    dplyr::rename(geography="COUNTY_NAME") %>%
    dplyr::mutate(geography=paste0(.data$geography, " County")) %>%
    dplyr::mutate(metric="1yr Fatality Rate", geography_type="PSRC Region", share=0, moe=0, share_moe=0, grouping="All")
  
  metros <- mpo_county_data %>%
    dplyr::select("MPO_AREA", "variable", "estimate", "data_year") %>%
    dplyr::rename(geography="MPO_AREA") %>%
    dplyr::group_by(.data$geography, .data$variable, .data$data_year) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(metric="1yr Fatality Rate", geography_type="Metro Regions", share=0, moe=0, share_moe=0, grouping="All")
  
  annual_data <- dplyr::bind_rows(psrc,metros)
  rm(psrc, metros, mpo_county_data)
  
  five_yr_data <- NULL
  for (y in safety_yrs) {
    
    c <- annual_data %>% 
      dplyr::filter(.data$data_year >= y-4 & .data$data_year <=y) %>%
      dplyr::select("geography", "variable", "estimate", "geography_type") %>%
      dplyr::group_by(.data$geography, .data$variable, .data$geography_type) %>%
      dplyr::summarise(estimate=sum(.data$estimate)) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate(metric="5yr Fatality Rate", share=0, moe=0, share_moe=0, grouping="All", data_year=y)
    
    ifelse(is.null(five_yr_data), five_yr_data <- c, five_yr_data <- dplyr::bind_rows(five_yr_data,c))
    
    rm(c)
    
  }
  
  # Trim Final Annual Data to Match the 5yr Average Time Period and Calculate Rate per Capita
  annual_data <- annual_data %>% 
    dplyr::filter(.data$data_year %in% safety_yrs)
  
  # Calculate Per Capita Rates
  p <- fars_data %>% 
    dplyr::select("geography", "data_year", "estimate") %>% 
    dplyr::rename(population="estimate")
  
  # Annual Rates
  ac <- annual_data %>% 
    dplyr::filter(.data$variable=="Fatal Collisions") %>% 
    dplyr::rename(collisions="estimate")
  
  ac <- dplyr::left_join(ac, p, by=c("geography","data_year"))
  ac <- ac %>% 
    dplyr::mutate(estimate=(.data$collisions/.data$population)*100000, variable="Fatal Collisions per 100,000 People") %>% 
    dplyr::select(-"population", -"collisions")
  
  af <- annual_data %>% 
    dplyr::filter(.data$variable=="Fatalities") %>% 
    dplyr::rename(collisions="estimate")
  
  af <- dplyr::left_join(af, p, by=c("geography","data_year"))
  af <- af %>% 
    dplyr::mutate(estimate=(.data$collisions/.data$population)*100000, variable="Fatalities per 100,000 People") %>% 
    dplyr::select(-"population", -"collisions")
  
  # 5yr Rates
  fc <- five_yr_data %>% 
    dplyr::filter(.data$variable=="Fatal Collisions") %>% 
    dplyr::rename(collisions="estimate")
  
  fc <- dplyr::left_join(fc, p, by=c("geography","data_year"))
  
  fc <- fc %>% 
    dplyr::mutate(estimate=((.data$collisions/5)/.data$population)*100000, variable="Fatal Collisions per 100,000 People") %>% 
    dplyr::select(-"population", -"collisions")
  
  ff <- five_yr_data %>% 
    dplyr::filter(.data$variable=="Fatalities") %>% 
    dplyr::rename(collisions="estimate")
  
  ff <- dplyr::left_join(ff, p, by=c("geography","data_year"))
  
  ff <- ff %>% 
    dplyr::mutate(estimate=((.data$collisions/5)/.data$population)*100000, variable="Fatalities per 100,000 People") %>% 
    dplyr::select(-"population", -"collisions")
  
  fars_data <- dplyr::bind_rows(fars_data, annual_data, five_yr_data, ac, af, fc, ff) %>%
    dplyr::mutate(date=lubridate::ymd(paste0(.data$data_year,"-12-01"))) %>%
    dplyr::select(-"data_year")
  
  rm(annual_data, five_yr_data, ac, af, fc, ff, p, collision_data)
  
  return(fars_data)
}
