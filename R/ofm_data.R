#' Get OFM Intercensal Population Data by Jurisdiction
#'
#' This function pulls Intercensal Population Data by Jurisdiction.
#' 
#' @return tibble of population by jurisdiction
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' ofm_pop_data <- get_ofm_pop_data()}
#' 
#' @export
#'
get_ofm_pop_data <- function(){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Jurisdictions with Regional Geography
  jurisdictions <- psrcelmer::get_table(schema='Political', tbl_name='jurisdiction_dims')
  
  jurisdictions <- jurisdictions |>
    dplyr::mutate(juris_name = gsub("Seatac","SeaTac",.data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",.data$juris_name)) |>
    dplyr::select("juris_name", "regional_geography", "airport_affected") |>
    dplyr::distinct() |>
    dplyr::mutate(regional_geography=gsub("HCT","High Capacity Transit Community",.data$regional_geography)) |>
    dplyr::mutate(regional_geography=gsub("Metro","Metropolitan Cities",.data$regional_geography)) |>
    dplyr::mutate(regional_geography=gsub("Core","Core Cities",.data$regional_geography)) |>
    dplyr::mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",.data$regional_geography)) |>
    dplyr::select(-"airport_affected") |>
    dplyr::mutate(juris_name = gsub("Uninc. King", "King County", .data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", .data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", .data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", .data$juris_name))
  
  # Intercensal estimates
  ofm.url.1990 <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_intercensal_estimates_1990-2000.xlsx"
  ofm.url.2000 <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_intercensal_estimates_2000-2010.xlsx"
  ofm.url.2010 <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_intercensal_estimates_2010_2020.xlsx"
  ofm.url.latest <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/ofm_april1_population_final.xlsx"
  
  # 1990 to 2000
  utils::download.file(ofm.url.1990, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.90 <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population ")) |>
    dplyr::filter(.data$County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") |>
    dplyr::select(-(dplyr::contains("Place.Code")), -"Jurisdiction") |>
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Total.Population.*", ""), City.Name = stringr::str_replace(.data$City.Name, " \\(part\\)", "")) |>
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) |>
    dplyr::rename(County="County.Name", Jurisdiction="City.Name") |>
    dplyr::filter(.data$Year != 2000) |>
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      .data$Filter == 1 ~ paste0(.data$County," County"),
      .data$Filter == 2 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      .data$Filter == 3 ~ paste0(.data$Jurisdiction," ", .data$County," County"),
      .data$Filter == 4 ~ .data$Jurisdiction)) |>
    dplyr::mutate(Jurisdiction = stringr::str_replace(.data$Jurisdiction, "Beaux Arts", "Beaux Arts Village")) |>
    dplyr::mutate(Jurisdiction = stringr::str_replace(.data$Jurisdiction, "Du Pont", "DuPont")) |>
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # 2000 to 2010
  utils::download.file(ofm.url.2000, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.00 <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population")) |>
    dplyr::filter(.data$County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") |>
    dplyr::select(-(dplyr::contains("Place.Code")), -"Jurisdiction") |>
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Total.Population.*", ""), Year = stringr::str_replace(.data$Year, ".Intercensal.*", ""), City.Name = stringr::str_replace(.data$City.Name, " \\(part\\)", "")) |>
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) |>
    dplyr::rename(County="County.Name", Jurisdiction="City.Name") |>
    dplyr::filter(.data$Year != 2010) |>
    dplyr::select(-"Line") |>
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      Filter == 1 ~ .data$Jurisdiction,
      Filter == 2 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 3 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 4 ~ .data$Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # 2010 to 2020
  utils::download.file(ofm.url.2010, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.10 <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population")) |>
    dplyr::filter(.data$County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") |>
    dplyr::select(-(dplyr::contains("FIPS.Code")), -"Jurisdiction") |>
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Total.Population.*", ""), Year = stringr::str_replace(.data$Year, ".Intercensal.*", ""), City.Name = stringr::str_replace(.data$City.Name, " \\(part\\)", "")) |>
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) |>
    dplyr::rename(County="County.Name", Jurisdiction="City.Name") |>
    dplyr::select(-"Line") |>
    dplyr::filter(.data$Year != 2020)|>
    dplyr::mutate(Jurisdiction = dplyr::case_when(
      Filter == 1 ~ .data$Jurisdiction,
      Filter == 2 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 3 ~ paste0(.data$Jurisdiction," ", .data$County, " County"),
      Filter == 4 ~ .data$Jurisdiction)) |>
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # 2020 to present
  utils::download.file(ofm.url.latest, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.pop.latest <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 5, colNames = TRUE, sheet = "Population")) |>
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) |>
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Year", values_to="Estimate") |>
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Population.*", ""), Jurisdiction = stringr::str_replace(.data$Jurisdiction, " \\(part\\)", "")) |>
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) |>
    dplyr::select(-"Line") |>
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both")))
  
  # Combine into one tibble and aggregate places that span multiple counties into one
  ofm.pop <- dplyr::bind_rows(list(ofm.pop.90,ofm.pop.00,ofm.pop.10,ofm.pop.latest)) |>
    dplyr::select(-"County") |>
    dplyr::group_by(.data$Filter, .data$Jurisdiction, .data$Year) |>
    dplyr::summarize(Estimate = sum(.data$Estimate)) |>
    dplyr::as_tibble()
  
  # Create a Regional Summary by Filter Type and then Join to Full OFM tibble
  region.pop <- ofm.pop |>
    dplyr::filter(.data$Filter <= 3) |>
    dplyr::select("Filter", "Year", "Estimate") |>
    dplyr::group_by(.data$Filter,.data$Year) |>
    dplyr::summarize_all(sum) |>
    dplyr::mutate(Jurisdiction = "Region") |>
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 2, "Unincorporated Region", .data$Jurisdiction)) |>
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 3, "Incorporated Region", .data$Jurisdiction))
  
  # Add the regional results to the OFM full tibble
  ofm.pop <- dplyr::bind_rows(ofm.pop,region.pop) |> dplyr::mutate(Variable="Total Population")
  
  # Add Regional Geography to Data
  ofm.pop <- dplyr::left_join(ofm.pop, jurisdictions, by=c("Jurisdiction"="juris_name")) |>
    dplyr::mutate(regional_geography = dplyr::case_when(
      Filter==1 ~ "County",
      Filter==2 ~ "Unincorporated",
      Filter==3 ~ "Incorporated",
      Filter==4 ~ .data$regional_geography
    ))
  
  file.remove(ofm.pop.file)
  
  return(ofm.pop)
  
}

#' Get OFM Housing Data by Jurisdiction
#'
#' This function pulls Housing Data by Jurisdiction.
#' 
#' @return tibble of population by jurisdiction
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' ofm_housing_data <- get_ofm_housing_data()}
#' 
#' @export
#'
get_ofm_housing_data <-function () {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Jurisdictions with Regional Geography
  jurisdictions <- psrcelmer::get_table(schema='Political', tbl_name='jurisdiction_dims')
  
  jurisdictions <- jurisdictions |>
    dplyr::mutate(juris_name = gsub("Seatac","SeaTac",.data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",.data$juris_name)) |>
    dplyr::select("juris_name", "regional_geography", "airport_affected") |>
    dplyr::distinct() |>
    dplyr::mutate(regional_geography=gsub("HCT","High Capacity Transit Community",.data$regional_geography)) |>
    dplyr::mutate(regional_geography=gsub("Metro","Metropolitan Cities",.data$regional_geography)) |>
    dplyr::mutate(regional_geography=gsub("Core","Core Cities",.data$regional_geography)) |>
    dplyr::mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",.data$regional_geography)) |>
    dplyr::select(-"airport_affected") |>
    dplyr::mutate(juris_name = gsub("Uninc. King", "King County", .data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", .data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", .data$juris_name)) |>
    dplyr::mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", .data$juris_name))
  
  # Housing
  ofm.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx"
  
  # 1990 to Present
  utils::download.file(ofm.url, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  
  ofm.type <- dplyr::as_tibble(openxlsx::read.xlsx(ofm.pop.file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Housing Units")) |>
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) |>
    dplyr::select(-(dplyr::contains("1980"))) |>
    tidyr::pivot_longer(cols=dplyr::contains("Housing"), names_to="Year", values_to="Estimate") |>
    dplyr::mutate(Variable= dplyr::case_when(
      stringr::str_detect(.data$Year, "Total") ~ "Total Housing Units",
      stringr::str_detect(.data$Year, "One") ~ "Single-Family",
      stringr::str_detect(.data$Year, "Two") ~ "Multi-Family",
      stringr::str_detect(.data$Year, "Mobile") ~ "Mobile Home")) |>
    dplyr::mutate(Year = stringr::str_replace(.data$Year, ".Census.*", ""), Year = stringr::str_replace(.data$Year, ".Postcensal.*", ""), Jurisdiction = stringr::str_replace(.data$Jurisdiction, " \\(part\\)", "")) |>
    dplyr::mutate(dplyr::across(c('Filter','Year','Estimate'), as.numeric)) |>
    dplyr::select(-"Line") |>
    dplyr::mutate(Jurisdiction = stringr::str_trim(.data$Jurisdiction, side = c("both"))) |>
    dplyr::select(-"County") |>
    dplyr::group_by(.data$Filter, .data$Jurisdiction, .data$Year, .data$Variable) |>
    dplyr::summarize(Estimate = sum(.data$Estimate)) |>
    dplyr::as_tibble()
  
  # Create a Regional Summary by Filter Type and then Join to Full OFM tibble
  region <- ofm.type |>
    dplyr::filter(.data$Filter <= 3) |>
    dplyr::select("Filter" , "Year", "Estimate", "Variable") |>
    dplyr::group_by(.data$Filter,.data$Year, .data$Variable) |>
    dplyr::summarize_all(sum) |>
    dplyr::mutate(Jurisdiction = "Region") |>
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 2, "Unincorporated Region", .data$Jurisdiction)) |>
    dplyr::mutate(Jurisdiction = ifelse(.data$Filter == 3, "Incorporated Region", .data$Jurisdiction))
  
  # Add the regional results to the OFM full tibble
  ofm.type <- dplyr::bind_rows(ofm.type,region)
  
  ofm.type <- dplyr::left_join(ofm.type, jurisdictions, by=c("Jurisdiction"="juris_name")) |>
    dplyr::mutate(regional_geography = dplyr::case_when(
      Filter==1 ~ "County",
      Filter==2 ~ "Unincorporated",
      Filter==3 ~ "Incorporated",
      Filter==4 ~ .data$regional_geography
    ))
  
  file.remove(ofm.pop.file)
  
  return(ofm.type)
}

#' Population  Trends from the Office of Financial Management
#'
#' This function pulls and cleans data from the Intercensal Population data from OFM.
#' Forecast data is from the latest PSRC Macroeconomic forecast as stored in Elmer
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @param start_year Four digit integer for first year of data to use from Elmer - defaults to 2010
#' @return tibble of population and housing trends for the region and regional geographies by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' pop <- regional_population_data()}
#' 
#' @export
#'
regional_population_data <- function(forecast_base_yr=2018, start_year=2010){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Get OFM Population Data by Jurisdiction
  print(stringr::str_glue("Downloading population data from the Office of Financial Management and calculating annual population change"))
  tp <- psrcrtp::get_ofm_pop_data() |>
    dplyr::rename(geography="Jurisdiction", estimate="Estimate", variable="Variable", geography_type="regional_geography") |>
    dplyr::mutate(date = lubridate::ymd(paste0(.data$Year,"-04-01"))) |>
    dplyr::rename(year="Year") |>
    tidyr::drop_na() |>
    dplyr::mutate(variable="Total") |>
    dplyr::group_by(.data$geography) |>
    dplyr::mutate(change = (.data$estimate - dplyr::lag(.data$estimate))) |>
    tidyr::pivot_longer(cols = c("estimate", "change"), names_to = "grouping", values_to = "estimate") |>
    tidyr::drop_na() |>
    dplyr::filter(.data$Filter == 1 & .data$year >= start_year) |>
    dplyr::mutate(variable = "Observed", metric="Population") |>
    dplyr::mutate(geography_type = dplyr::case_when(
      .data$geography == "Region" ~ "Region",
      .data$geography != "Region" ~ "County")) |>
    dplyr::filter(.data$geography_type == "Region") |>
    dplyr::mutate(year = as.character(.data$year)) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "change", "Change")) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "estimate", "Total")) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  # Get Forecast Population Growth from Elmer
  print(stringr::str_glue("Getting Population Forecast Data"))
  fp <-  psrcelmer::get_table(schema='Macroeconomic', tbl_name='pop_facts') |>
    dplyr::filter(.data$pop_group_dim_id==7 & .data$data_year >= forecast_base_yr-1) |>
    dplyr::rename(estimate="population") |>
    dplyr::mutate(date = lubridate::ymd(paste0(.data$data_year,"-04-01"))) |>
    dplyr::mutate(variable="Forecast") |>
    dplyr::select("date", "estimate", "variable") |>
    dplyr::mutate(geography="Region", geography_type="Region", metric="Population") |>
    dplyr::group_by(.data$geography) |>
    dplyr::mutate(change = (.data$estimate - dplyr::lag(.data$estimate))) |> 
    tidyr::drop_na() |>
    tidyr::pivot_longer(cols = c("estimate", "change"), names_to = "grouping", values_to = "estimate") |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "change", "Change")) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "estimate", "Total")) |>
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  f <- dplyr::bind_rows(tp, fp)
  
  print(stringr::str_glue("All Done"))
  return(f)
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
#' This function pulls and cleans data from the Intercensal Housing data from OFM.
#' Forecast data is from the latest VISION 2050 modeling as stored on the file server
#' 
#' @param forecast_base_yr Four digit integer for Base Year in Forecast Data - defaults to 2018 
#' @param start_year Four digit integer for first year of data to use from Elmer - defaults to 2010
#' @return tibble of housing trends for the region and regional geographies by calendar year
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' hsg <- regional_housing_data()}
#' 
#' @export
#'
regional_housing_data <- function(forecast_base_yr=2018, start_year=2010){
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Get OFM Housing Data by Jurisdiction
  print(stringr::str_glue("Downloading housing data from the Office of Financial Management and calculating annual population change"))
  t <- psrcrtp::get_ofm_housing_data() %>%
    dplyr::filter(.data$Jurisdiction == "Region" & .data$Variable == "Total Housing Units") |>
    dplyr::rename(geography="Jurisdiction", estimate="Estimate", variable="Variable", geography_type="regional_geography") |>
    dplyr::mutate(date = lubridate::ymd(paste0(.data$Year,"-04-01")), geography_type="Region") |>
    dplyr::rename(year="Year") |>
    dplyr::mutate(year = as.character(.data$year)) |>
    tidyr::drop_na() |>
    dplyr::select(-"Filter") |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(change = (.data$estimate - dplyr::lag(.data$estimate))) |>
    tidyr::pivot_longer(cols = c("estimate", "change"), names_to = "grouping", values_to = "estimate") |>
    tidyr::drop_na() |>
    dplyr::filter(.data$year >= start_year) |>
    dplyr::mutate(metric="Housing Units", variable="Observed") |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "change", "Change")) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "estimate", "Total")) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate")
  
  # Getting Forecast Housing Units
  print(stringr::str_glue("Getting housing unit forecast data"))
  f <- readr::read_csv("X://DSA//rtp-dashboard//PSRC//regional-housing.csv", show_col_types = FALSE) |>
    dplyr::select(-"Observed") |>
    dplyr::rename(estimate="Forecast") |>
    dplyr::filter(.data$Year >= forecast_base_yr-1) |>
    dplyr::mutate(date = lubridate::ymd(paste0(.data$Year,"-04-01"))) |>
    dplyr::mutate(year = as.character(.data$Year)) |>
    dplyr::mutate(metric="Housing Units", geography_type="Region", geography="Region", variable="Forecast") |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "metric", "estimate") |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(change = (.data$estimate - dplyr::lag(.data$estimate))) |>
    tidyr::pivot_longer(cols = c("estimate", "change"), names_to = "grouping", values_to = "estimate") |>
    tidyr::drop_na()|>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "change", "Change")) |>
    dplyr::mutate(grouping = stringr::str_replace_all(.data$grouping, "estimate", "Total")) |>
    dplyr::select("year", "date", "geography", "geography_type", "variable", "grouping", "metric", "estimate") |>
    dplyr::filter(.data$year >= forecast_base_yr)
  
  housing <- dplyr::bind_rows(t,f)
  
  print(stringr::str_glue("All Done"))
  return(housing)
  
}
