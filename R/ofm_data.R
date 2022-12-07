#' Population Trends from the Office of Financial Management
#'
#' This function pulls and cleans data from the Intercensal Population data from OFM.
#' Forecast data is from the latest PSRC Macroeconomic forecast as stored in Elmer
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @return tibble of population trends for the region and regional geographies by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' 
#' pop_data<- population_data()
#' 
#' @export
#'
population_data <- function(forecast_base_yr=2018){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Get OFM Population Data by Jurisdiction
  print("Downloading population data from the Office of Financial Management and calculating annual population change")
  tp <- psrctrends::get_ofm_intercensal_population() %>%
    dplyr::rename(geography="Jurisdiction", estimate="Estimate", variable="Variable", geography_type="regional_geography") %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$Year,"-04-01"))) %>%
    dplyr::select(-"Year") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(variable="Total")
  
  # Annual Population Change
  pc <- tp %>%
    dplyr::group_by(.data$geography) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), variable="Change")
  
  p <- dplyr::bind_rows(tp, pc) %>% tidyr::drop_na() 
  rm(tp,pc)
  
  # Groupings by Region for Observed Data
  print("Summarizing population by Region")
  r <- p %>% 
    dplyr::select(-"Filter") %>%
    dplyr::filter(.data$geography == "Region") %>%
    dplyr::mutate(metric="Observed Population", share=1, geography_type="Region", grouping="Population")
  
  # Region Totals for subsequent share calculations
  t <- r %>% 
    dplyr::select("date","variable", "estimate") %>%
    dplyr::rename(total="estimate")
  
  # Groupings by County for Observed Data
  print("Summarizing population by County")
  c <- p %>% 
    dplyr::select(-"Filter") %>%
    dplyr::filter(.data$geography %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County")) %>%
    dplyr::mutate(metric="Observed Population", geography_type="County", grouping="Population")
  
  c <- dplyr::left_join(c, t, by=c("date", "variable")) %>%
    dplyr::mutate(share = .data$estimate / .data$total) %>%
    dplyr::select(-"total")
  
  # Groupings by Regional Geography for Observed Data
  print("Summarizing population by Regional Geography")
  rgeo <- p %>%
    dplyr::filter(.data$Filter %in% c(2,4)) %>%
    dplyr::filter(.data$geography != "Unincorporated Region") %>%
    dplyr::group_by(.data$geography_type, .data$date, .data$variable) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::rename(geography="geography_type") %>%
    dplyr::mutate(metric="Observed Population", geography_type="Regional Geography", grouping="Population")
  
  rgeo <- dplyr::left_join(rgeo, t, by=c("date", "variable")) %>%
    dplyr::mutate(share = .data$estimate / .data$total) %>%
    dplyr::select(-"total")
  
  print("Getting Population Forecast Data")
  # Observed Data up to Forecast Base Year
  fo <- r %>%
    dplyr::mutate(metric="Forecast Population") %>%
    dplyr::filter(lubridate::year(.data$date) <= forecast_base_yr)
  
  # Get Forecast Population Growth from Elmer
  fp <- psrctrends::get_elmer_table("Macroeconomic.pop_facts") %>%
    dplyr::filter(.data$pop_group_dim_id==7 & .data$data_year >= forecast_base_yr) %>%
    dplyr::rename(estimate="population") %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$data_year,"-04-01"))) %>%
    dplyr::mutate(metric="Forecast Population") %>% 
    dplyr::select("date", "estimate", "metric") %>%
    dplyr::mutate(geography="Region", grouping="Population", geography_type="Region", share=1, variable="Total") 
  
  # Annual Forecast Population Change
  fc <- fp %>%
    dplyr::group_by(.data$geography) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), variable="Change") %>% 
    tidyr::drop_na()
  
  fp <- fp %>%
    dplyr::filter(lubridate::year(.data$date) > forecast_base_yr)
  
  f <- dplyr::bind_rows(fo, fp, fc)
  rm(fo, fp,fc)
  
  print("Combining Observed and Foresat Data into one tibble")
  population <- dplyr::bind_rows(list(r,c,rgeo,f))
  
  print("All Done")
  return(population)
}