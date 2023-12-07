#' Population and Housing Trends from the Office of Financial Management
#'
#' This function pulls and cleans data from the Intercensal Population data from OFM.
#' Forecast data is from the latest PSRC Macroeconomic forecast as stored in Elmer
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @return tibble of population and housing trends for the region and regional geographies by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' pop_data<- population_data()}
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

#' Population and Housing Unit Growth Near High Capacity Transit
#'
#' This function pulls and cleans data from the SAEP Block data from OFM from Elmer that was parcelized.
#' 
#' @return tibble of population and housing growth near High Capacity Transit by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' pop_hsg_hct <- pop_hsg_near_hct()}
#' 
#'  
#' @export
#'
pop_hsg_near_hct <- function() {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Load Parcel Data from Elmer
  print(stringr::str_glue("Getting Parcel data from Elmer - this can take 5 or more minutes so be patient"))
  parcel_facts <- psrcelmer::get_table(schema='ofm', tbl_name='parcelized_saep_facts')
  parcel_dims <- psrcelmer::get_table(schema = 'small_areas', tbl_name = 'parcel_dim')
  
  # Clean up parcel geometry and join parcel facts and dimensions
  print(stringr::str_glue("Cleaning up parcels"))
  parcel_geo <- parcel_dims |> dplyr::select("parcel_dim_id", "parcel_id", parcel_year="base_year", hct="hct_station_area_vision_2050")
  
  # Summarize Population and Housing Units by HCT Area
  parcels <- dplyr::left_join(parcel_facts, parcel_geo, by=c("parcel_dim_id")) |> 
    dplyr::select(year="estimate_year", "total_pop", "housing_units", variable="hct", "ofm_vintage")
  
  max_ofm_vintage <- max(parcels$ofm_vintage)
  
  parcels <- parcels |> dplyr::filter(.data$year < 2020 | (.data$year >= 2020 & .data$ofm_vintage==max_ofm_vintage)) |>
    dplyr::group_by(.data$year, .data$variable) |>
    dplyr::summarise(population = as.integer(sum(.data$total_pop)), housing_units = as.integer(sum(.data$housing_units))) |>
    dplyr::as_tibble() |>
    dplyr::arrange(.data$variable, .data$year) |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(population_growth = .data$population - dplyr::lag(.data$population), housing_growth = .data$housing_units - dplyr::lag(.data$housing_units)) |>
    tidyr::pivot_longer(cols = !c("year", "variable"), names_to = "metric", values_to = "estimate") |>
    tidyr::drop_na() |>
    dplyr::mutate(grouping = dplyr::case_when(
      stringr::str_detect(.data$metric, "growth") ~ "Change",
      !(stringr::str_detect(.data$metric, "growth")) ~ "Total")) |>
    dplyr::mutate(metric = dplyr::case_when(
      stringr::str_detect(.data$metric, "population") ~ "Population near HCT",
      stringr::str_detect(.data$metric, "housing") ~ "Housing Units near HCT"))
  
  print(stringr::str_glue("Adding totals and calculating shares for HCT growth"))
  totals <- parcels |>
    dplyr::group_by(.data$year, .data$metric, .data$grouping) |>
    dplyr::summarise(total = sum(.data$estimate)) |>
    dplyr::as_tibble()
  
  parcels <- dplyr::left_join(parcels, totals, by=c("year", "metric", "grouping")) |>
    dplyr::mutate(share = .data$estimate / .data$total) |>
    dplyr::select(-"total") |>
    dplyr::mutate(year = as.character(.data$year), geography="Region", geography_type="Region", date=lubridate::mdy(paste0("04-01-", .data$year))) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate", "share")
  
  return(parcels)
  
}

#' Housing Trends from the Office of Financial Management
#'
#' This function pulls and cleans data from the Intercensal Population data from OFM.
#' Forecast data is from the latest VISION 2050 modleing as stored on the file server
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @return tibble of housing trends for the region and regional geographies by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' housing_units <- housing_units_data()}
#' 
#' @export
#'

housing_units_data <- function(forecast_base_yr=2018){

  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)

  # Get OFM Housing Data by Jurisdiction
  print("Downloading housing data from the Office of Financial Management and calculating annual population change")
  t <- psrctrends::get_ofm_postcensal_housing() %>%
    dplyr::rename(geography="Jurisdiction", estimate="Estimate", variable="Variable", geography_type="regional_geography") %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$Year,"-04-01"))) %>%
    dplyr::select(-"Year") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(grouping="Total") %>%
    dplyr::filter(.data$variable != "Mobile Home")

  # Calculate Annual Change
  c <- t %>%
    dplyr::group_by(.data$geography, .data$variable) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), grouping="Change")

  # Combine Totals and Change
  h <- dplyr::bind_rows(t, c) %>% tidyr::drop_na() 
  rm(t,c)

  # Groupings by Region for Observed Data
  print("Summarizing housing units by Region")
  r <- h %>% 
    dplyr::select(-"Filter") %>%
    dplyr::filter(.data$geography == "Region") %>%
    dplyr::mutate(metric="Observed", share=1, geography_type="Region")

  # Region Totals for subsequent share calculations
  t <- r %>% 
    dplyr::select("date","variable", "grouping", "estimate") %>%
    dplyr::rename(total="estimate")

  # Groupings by County for Observed Data
  print("Summarizing Housing Units by County")
  c <- h %>% 
    dplyr::select(-"Filter") %>%
    dplyr::filter(.data$geography %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County")) %>%
    dplyr::mutate(metric="Observed", geography_type="County")

  c <- dplyr::left_join(c, t, by=c("date", "variable", "grouping")) %>%
    dplyr::mutate(share = .data$estimate / .data$total) %>%
    dplyr::select(-"total")

  # Groupings by Regional Geography for Observed Data
  print("Summarizing housing units by Regional Geography")
  rgeo <- h %>%
    dplyr::filter(.data$Filter %in% c(2,4)) %>%
    dplyr::filter(.data$geography != "Unincorporated Region") %>%
    dplyr::group_by(.data$geography_type, .data$date, .data$variable, .data$grouping) %>%
    dplyr::summarise(estimate=sum(.data$estimate)) %>%
    tidyr::as_tibble() %>%
    dplyr::rename(geography="geography_type") %>%
    dplyr::mutate(metric="Observed", geography_type="Regional Geography")

  rgeo <- dplyr::left_join(rgeo, t, by=c("date", "variable", "grouping")) %>%
    dplyr::mutate(share = .data$estimate / .data$total) %>%
    dplyr::select(-"total")

  # Getting Forecasted Housing Units
  print("Getting housing unit forecast data")
  f <- readr::read_csv("X://DSA//shiny-uploads//data//regional-housing.csv", show_col_types = FALSE) %>%
    dplyr::select(-"Observed") %>%
    dplyr::rename(estimate="Forecast") %>%
    dplyr::filter(.data$Year >= forecast_base_yr) %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$Year,"-04-01"))) %>%
    dplyr::select(-"Year") %>%
    dplyr::mutate(metric="Forecast", geography_type="Region", geography="Region", variable="Total Housing Units", grouping="Total", share=1)

  print("Combining Data into one tibble")
  housing <- dplyr::bind_rows(list(r,c,rgeo,f))

  print("All Done")
  return(housing)

}
