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

#' Population and Housing Unit Growth Near High Capacity Transit
#'
#' This function pulls and cleans data from the SAEP Block data from OFM for 2020 onward.
#' 2010 to 2020 SAEP Block Data is stored in Elmer
#' VISION 2050 Station Area and RGC buffers are stored in ElmerGeo
#' 
#' @return tibble of population and housing growth near High Capacity Transit by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' pop_hsg_hct <- population_near_hct()}
#' 
#'  
#' @export
#'
population_near_hct <- function() {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # Get Census Block Level Population Estimates from OFM: 2020 onward uses 2020 Census
  print("Downloading OFM Block Level Population Estimates from OFM Website for 2020 onward.")
  url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/smallarea/data/xlsx/saep_block20.zip"
  utils::download.file(url, "working.zip", quiet = TRUE, mode = "wb")
  data_file <- paste0(getwd(),"/working.zip")
  
  ofm_post_20 <- readr::read_csv(data_file, show_col_types = FALSE) %>%
    dplyr::filter(.data$COUNTYNAME %in% c("King", "Kitsap", "Pierce", "Snohomish")) %>%
    dplyr::select("GEOID20", "POP2020", "POP2021", "POP2022", "HU2020", "HU2021", "HU2022") %>%
    dplyr::mutate(geoid20 = as.character(.data$GEOID20))
  
  # Buffer 2020 blocks with VISION 2050 Station Areas from ElmerGeo
  db_conn <- DBI::dbConnect(odbc::odbc(),
                            driver = "SQL Server",
                            server = "AWS-PROD-SQL\\Sockeye",
                            database = "ElmerGeo",
                            trusted_connection = "yes")
  
  print("Loading HCT Station Buffer from ElmerGeo")
  buffers <- sf::st_read(dsn=db_conn, 
                         query="SELECT acres, Shape.STAsBinary() as Shape from dbo.vision_hct_station_areas_rgc_dissolved") %>%
    dplyr::mutate(hct_buffer=1)
  
  print("Loading 2020 Census Blocks from ElmerGeo")
  blocks <- sf::st_read(dsn=db_conn, 
                        query="SELECT geoid20, Shape.STAsBinary() as Shape from dbo.block2020_nowater")
  
  print("Buffering Census Blocks with HCT Station areas for 2020 Census")
  buffered_blocks <- sf::st_intersection(blocks, buffers)
  hct_blocks <- buffered_blocks %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-"acres")
  
  ofm_post_20 <- dplyr::left_join(ofm_post_20, hct_blocks, by=c("geoid20"))
  
  # Final Clean up of Growth by Year around Stations
  print("Calculating annual population growth around high-capacity station areas for 2020 onward")
  
  # Total Population by HCT Area and Year
  tp <- ofm_post_20 %>%
    dplyr::select(-"GEOID20", -"geoid20") %>%
    tidyr::pivot_longer(cols = !.data$hct_buffer, names_to = "date", values_to = "estimate") %>%
    dplyr::group_by(.data$hct_buffer, .data$date) %>%
    dplyr::summarise(estimate=as.integer(sum(.data$estimate))) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(grouping = dplyr::case_when(.data$hct_buffer == 1 ~ "Inside HCT Area", TRUE ~ "Outside HCT Area")) %>%
    dplyr::mutate(metric = dplyr::case_when(
      stringr::str_detect(.data$date,"POP") ~ "Population",
      stringr::str_detect(.data$date,"HU") ~ "Housing Units")) %>%
    dplyr::mutate(geography="Region", geography_type="Region") %>%
    dplyr::mutate(date = stringr::str_remove_all(.data$date,"POP")) %>%
    dplyr::mutate(date = stringr::str_remove_all(.data$date,"HU")) %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$date,"-04-01"))) %>%
    dplyr::mutate(variable="Total") %>%
    dplyr::select(-"hct_buffer")
  
  # Annual Population Change
  pc <- tp %>%
    dplyr::group_by(.data$grouping, .data$metric) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), variable="Change")
  
  pop_post_20 <- dplyr::bind_rows(tp, pc) %>% tidyr::drop_na() 
  rm(tp,pc)
  
  # Get Census Block Level Population Estimates from OFM: 2010 to 2020 via Elmer
  print("Getting Block Population from 2010 to 2020 via Elmer")
  block_pop <- psrctrends::get_elmer_table("ofm.estimate_facts") %>% 
    dplyr::filter(.data$publication_dim_id == 3)
  
  block_dim <- psrctrends::get_elmer_table("census.geography_dim") %>% 
    dplyr::filter(.data$geography_type %in% c("Block")) %>% 
    dplyr::select("geography_dim_id", "block_geoid")
  
  ofm_pre_2020 <- dplyr::left_join(block_pop, block_dim, by=c("geography_dim_id")) %>%
    dplyr::mutate(population = .data$household_population + .data$group_quarters_population) %>%
    dplyr::select("block_geoid", "housing_units", "population", "estimate_year") %>%
    dplyr::rename(geoid10="block_geoid")
  
  # Buffer 2010 blocks with VISION 2050 Station Areas from ElmerGeo
  print("Loading 2010 Census Blocks from ElmerGeo")
  blocks <- sf::st_read(dsn=db_conn, 
                        query="SELECT geoid10, Shape.STAsBinary() as Shape from dbo.block2010_nowater")
  
  print("Buffering Census Blocks with HCT Station areas for 2010 Census")
  buffered_blocks <- sf::st_intersection(blocks, buffers)
  
  hct_blocks <- buffered_blocks %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-"acres")
  
  ofm_pre_2020 <- dplyr::left_join(ofm_pre_2020, hct_blocks, by=c("geoid10"))
  
  # Final Clean up of Growth by Year around Stations
  print("Calculating annual population growth around high-capacity station areas from 2010 to 2020")
  
  # Total Population by HCT Area and Year
  tp <- ofm_pre_2020 %>%
    dplyr::select(-"geoid10") %>%
    dplyr::rename(date="estimate_year") %>%
    tidyr::pivot_longer(cols = c("population", "housing_units"), names_to = "metric", values_to = "estimate") %>%
    dplyr::mutate(metric = stringr::str_replace_all(.data$metric, "population", "Population")) %>%
    dplyr::mutate(metric = stringr::str_replace_all(.data$metric, "housing_units", "Housing Units")) %>%
    dplyr::group_by(.data$hct_buffer, .data$date, .data$metric) %>%
    dplyr::summarise(estimate=as.integer(sum(.data$estimate))) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(grouping = dplyr::case_when(.data$hct_buffer == 1 ~ "Inside HCT Area", TRUE ~ "Outside HCT Area")) %>%
    dplyr::mutate(geography="Region", geography_type="Region") %>%
    dplyr::mutate(date = lubridate::ymd(paste0(.data$date,"-04-01"))) %>%
    dplyr::mutate(variable="Total") %>%
    dplyr::select(-"hct_buffer")
  
  # Annual Population Change
  pc <- tp %>%
    dplyr::group_by(.data$grouping, .data$metric) %>%
    dplyr::mutate(estimate = (.data$estimate-dplyr::lag(.data$estimate)), variable="Change")
  
  tp <-tp %>%
    dplyr::filter(lubridate::year(.data$date) != 2020)
  
  pop_pre_20 <- dplyr::bind_rows(tp, pc) %>% 
    tidyr::drop_na() 
  
  rm(tp,pc)
  
  # Combine Pre and Post 2020 and Calculate Shares
  print("Combining Pre and Post 2020 data and generating shares")
  population <- dplyr::bind_rows(pop_pre_20, pop_post_20)
  
  # Calculate Totals
  t <- population %>%
    dplyr::select(-"grouping", -"geography", -"geography_type") %>%
    dplyr::group_by(.data$date, .data$metric, .data$variable) %>%
    dplyr::summarise(total = sum(.data$estimate)) %>%
    tidyr::as_tibble()
  
  # Add Shares
  population <- dplyr::left_join(population, t, by=c("date", "metric", "variable")) %>%
    dplyr::mutate(share = .data$estimate/.data$total) %>%
    dplyr::select(-"total") %>%
    dplyr::mutate(geography = .data$grouping) %>%
    dplyr::mutate(grouping="Growth Near High Capacity Transit")
  
  file.remove(data_file)
  
  return(population)
  
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
#' 
#' housing_units <- housing_units_data()
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

#' Block Level Estimates of equity Focus Populations
#'
#' This function pulls and cleans data from the block level Population data from OFM.
#' Uses Census Tracts or Block Groups to assign shares of the 6 equity focus areas to blocks
#' 
#' @param year Four digit integer for current year of analysis
#' @param latest_census_year Four digit integer for latest ACS 5 yr data - defaults to 2021
#' @return tibble of population by equity focus area by census block by calendar year
#' 
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' pop_by_block_efa <- population_by_efa(year=2022)}
#' 
#' @export
#'

population_by_efa <- function(year, latest_census_year=2021) {
  
  disability_variables <- c("B18101_004", "B18101_007", "B18101_010", "B18101_013", "B18101_016", "B18101_019",
                            "B18101_023", "B18101_026", "B18101_029", "B18101_032", "B18101_035", "B18101_038")
  
  youth_variables <- c("B01001_004", "B01001_005", "B01001_006",
                       "B01001_028", "B01001_029", "B01001_030")
  
  older_variables <- c("B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                       "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")
  
  options(dplyr.summarise.inform = FALSE)
  
  if (year > latest_census_year) {census_year<-latest_census_year} else (census_year <- year)
  
  # Get Population Data by Block from OFM
  if (year >= 2020) {
    
    pop_var <- paste0("POP",year)
    
    # Get Census Block Level Population Estimates from OFM website: 2020 onward uses 2020 Census
    print("Downloading OFM Block Level Population Estimates from OFM Website for 2020 onward.")
    url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/smallarea/data/xlsx/saep_block20.zip"
    utils::download.file(url, "working.zip", quiet = TRUE, mode = "wb")
    data_file <- paste0(getwd(),"/working.zip")
    
    ofm <- readr::read_csv(data_file, show_col_types = FALSE) %>%
      dplyr::filter(.data$COUNTYNAME %in% c("King", "Kitsap", "Pierce", "Snohomish")) %>%
      dplyr::select("GEOID20", dplyr::all_of(pop_var)) %>%
      dplyr::mutate(population = as.integer(.data[[pop_var]])) %>%
      dplyr::mutate(geoid = as.character(.data$GEOID20)) %>%
      dplyr::mutate(date=lubridate::mdy(paste0("04-01-",year))) %>%
      dplyr::select("geoid", "population", "date") %>%
      dplyr::mutate(blockgroup_id = stringr::str_sub(.data$geoid, 1, 12)) %>%
      dplyr::mutate(tract_id = stringr::str_sub(.data$geoid, 1, 11))
    
    file.remove(data_file)
    
  } else {
    
    # Get Census Block Level Population Estimates from OFM: 2010 to 2020 via Elmer
    print("Getting Block Population from 2010 to 2020 via Elmer")
    block_pop <- psrctrends::get_elmer_table("ofm.estimate_facts") %>% 
      dplyr::filter(.data$publication_dim_id == 3)
    
    block_dim <- psrctrends::get_elmer_table("census.geography_dim") %>% 
      dplyr::filter(.data$geography_type %in% c("Block")) %>% 
      dplyr::select("geography_dim_id", "block_geoid")
    
    ofm <- dplyr::left_join(block_pop, block_dim, by=c("geography_dim_id")) %>%
      dplyr::filter(.data$estimate_year == year) %>%
      dplyr::mutate(population = as.integer(.data$household_population + .data$group_quarters_population)) %>%
      dplyr::select("block_geoid", "population", "estimate_year") %>%
      dplyr::mutate(geoid = as.character(.data$block_geoid)) %>%
      dplyr::mutate(date=lubridate::mdy(paste0("04-01-",year))) %>%
      dplyr::select("geoid", "population", "date") %>%
      dplyr::mutate(blockgroup_id = stringr::str_sub(.data$geoid, 1, 12)) %>%
      dplyr::mutate(tract_id = stringr::str_sub(.data$geoid, 1, 11))
    
  }
  
  population <- ofm %>%
    dplyr::select("geoid","population","date") %>%
    dplyr::rename(estimate="population", geography="geoid") %>%
    dplyr::mutate(variable="Total Population", geography_type="Census Block", metric="Population", grouping="Equity Focus Area", share=1)
  
  #####################################################################################################
  # Get Population by Race & Ethnicity for Census Block Groups
  #####################################################################################################
  print(paste0("Downloading ", census_year," Population Data by Census BlockGroup by Race & Ethnicity from Census."))
  pop <- psrccensus::get_acs_recs(geography = 'block group',table.names = c('B03002'), years=c(census_year))
  
  tot <- pop %>% 
    dplyr::filter(.data$variable == "B03002_001") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(total="estimate")
  
  white <- pop %>% 
    dplyr::filter(.data$variable == "B03002_003") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(white="estimate")
  
  poc <- dplyr::left_join(tot,white, by=c("GEOID")) %>%
    dplyr::mutate(poc=.data$total-.data$white) %>%
    dplyr::mutate(share=.data$poc/.data$total) %>%
    dplyr::select("GEOID","share") %>%
    dplyr::mutate(share = tidyr::replace_na(.data$share,0)) %>%
    dplyr::rename(blockgroup_id="GEOID")
  
  # Join Population and Census Data
  temp <- dplyr::left_join(ofm, poc, by=c("blockgroup_id")) %>%
    dplyr::mutate(estimate=round(.data$population*.data$share,0)) %>%
    dplyr::mutate(variable="People of Color", geography_type="Census Block", metric="Population", grouping="Equity Focus Area") %>%
    dplyr::select(-"population", -"blockgroup_id", -"tract_id") %>%
    dplyr::rename(geography="geoid")
  
  population <- dplyr::bind_rows(population, temp)
  
  rm(pop, tot, white, poc, temp)
  
  #####################################################################################################
  # Get Population by Disability Status for Census Tracts
  #####################################################################################################
  print(paste0("Downloading ", census_year," Population Data by Census Tract by Disability Status from Census."))
  pop <- psrccensus::get_acs_recs(geography = 'tract',table.names = c('B18101'), years=c(census_year))
  
  tot <- pop %>% 
    dplyr::filter(.data$variable == "B18101_001") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(total="estimate")
  
  disabled <- pop %>% 
    dplyr::filter(.data$variable %in% disability_variables) %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarise(disabled=sum(.data$estimate)) %>%
    tidyr::as_tibble()
  
  dis <- dplyr::left_join(tot,disabled, by=c("GEOID")) %>%
    dplyr::mutate(share=.data$disabled/.data$total) %>%
    dplyr::select("GEOID","share") %>%
    dplyr::mutate(share = tidyr::replace_na(.data$share,0)) %>%
    dplyr::rename(tract_id="GEOID")
  
  # Join Population and Census Data
  temp <- dplyr::left_join(ofm, dis, by=c("tract_id")) %>%
    dplyr::mutate(estimate=round(.data$population*.data$share,0)) %>%
    dplyr::mutate(variable="People with a Disability", geography_type="Census Block", metric="Population", grouping="Equity Focus Area") %>%
    dplyr::select(-"population", -"blockgroup_id", -"tract_id") %>%
    dplyr::rename(geography="geoid")
  
  population <- dplyr::bind_rows(population, temp)
  
  rm(pop, tot, disabled, dis, temp)
  
  #####################################################################################################
  # Get Population by Poverty Status for Census Tract
  #####################################################################################################
  print(paste0("Downloading ", census_year," Population Data by Tract by Poverty Status from Census."))
  pop <- psrccensus::get_acs_recs(geography = 'tract',table.names = c('C17002'), years=c(census_year))
  
  tot <- pop %>% 
    dplyr::filter(.data$variable == "C17002_001") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(total="estimate")
  
  above <- pop %>% 
    dplyr::filter(.data$variable == "C17002_008") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(above="estimate")
  
  pov <- dplyr::left_join(tot,above, by=c("GEOID")) %>%
    dplyr::mutate(below=.data$total-.data$above) %>%
    dplyr::mutate(share=.data$below/.data$total) %>%
    dplyr::select("GEOID","share") %>%
    dplyr::mutate(share = tidyr::replace_na(.data$share,0)) %>%
    dplyr::rename(tract_id="GEOID")
  
  # Join Population and Census Data
  temp <- dplyr::left_join(ofm, pov, by=c("tract_id")) %>%
    dplyr::mutate(estimate=round(.data$population*.data$share,0)) %>%
    dplyr::mutate(variable="People with Lower Incomes", geography_type="Census Block", metric="Population", grouping="Equity Focus Area") %>%
    dplyr::select(-"population", -"blockgroup_id", -"tract_id") %>%
    dplyr::rename(geography="geoid")
  
  population <- dplyr::bind_rows(population, temp)
  
  rm(pop, tot, above, pov, temp)
  
  #####################################################################################################
  # Get Population by Age for Census Block Groups
  #####################################################################################################
  print(paste0("Downloading ", census_year," Population Data by Census BlockGroup by Age from Census."))
  pop <- psrccensus::get_acs_recs(geography = 'block group',table.names = c('B01001'), years=c(census_year))
  
  tot <- pop %>% 
    dplyr::filter(.data$variable == "B01001_001") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(total="estimate")
  
  youth <- pop %>% 
    dplyr::filter(.data$variable %in% youth_variables) %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarise(youth=sum(.data$estimate)) %>%
    tidyr::as_tibble()
  
  older <- pop %>% 
    dplyr::filter(.data$variable %in% older_variables) %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarise(older=sum(.data$estimate)) %>%
    tidyr::as_tibble()
  
  # Join Youth Population and Census Data
  age <- dplyr::left_join(tot, youth, by=c("GEOID")) %>%
    dplyr::mutate(share=.data$youth/.data$total) %>%
    dplyr::select("GEOID","share") %>%
    dplyr::mutate(share = tidyr::replace_na(.data$share,0)) %>%
    dplyr::rename(blockgroup_id="GEOID")
  
  temp <- dplyr::left_join(ofm, age, by=c("blockgroup_id")) %>%
    dplyr::mutate(estimate=round(.data$population*.data$share,0)) %>%
    dplyr::mutate(variable="Youth", geography_type="Census Block", metric="Population", grouping="Equity Focus Area") %>%
    dplyr::select(-"population", -"blockgroup_id", -"tract_id") %>%
    dplyr::rename(geography="geoid")
  
  population <- dplyr::bind_rows(population, temp)
  
  # Join Older Adults Population and Census Data
  age <- dplyr::left_join(tot, older, by=c("GEOID")) %>%
    dplyr::mutate(share=.data$older/.data$total) %>%
    dplyr::select("GEOID","share") %>%
    dplyr::mutate(share = tidyr::replace_na(.data$share,0)) %>%
    dplyr::rename(blockgroup_id="GEOID")
  
  temp <- dplyr::left_join(ofm, age, by=c("blockgroup_id")) %>%
    dplyr::mutate(estimate=round(.data$population*.data$share,0)) %>%
    dplyr::mutate(variable="Older Adults", geography_type="Census Block", metric="Population", grouping="Equity Focus Area") %>%
    dplyr::select(-"population", -"blockgroup_id", -"tract_id") %>%
    dplyr::rename(geography="geoid")
  
  population <- dplyr::bind_rows(population, temp)
  
  rm(pop, tot, youth, older, age, temp)
  
  #####################################################################################################
  # Get Population by Limited English Proficiency for Census Tracts
  #####################################################################################################
  print(paste0("Downloading ", census_year," Population Data by Census Tract by English Proficiency from Census."))
  pop <- psrccensus::get_acs_recs(geography = 'tract',table.names = c('C16002'), years=c(census_year))
  
  tot <- pop %>% 
    dplyr::filter(.data$variable == "C16002_001") %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::rename(total="estimate")
  
  lep <- pop %>% 
    dplyr::filter(stringr::str_detect(.data$label, "Limited English speaking household")) %>% 
    dplyr::select("GEOID","estimate") %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarise(lep=sum(.data$estimate)) %>%
    tidyr::as_tibble()
  
  # Join LEP Population and Census Data
  lep <- dplyr::left_join(tot, lep, by=c("GEOID")) %>%
    dplyr::mutate(share=.data$lep/.data$total) %>%
    dplyr::select("GEOID","share") %>%
    dplyr::mutate(share = tidyr::replace_na(.data$share,0)) %>%
    dplyr::rename(tract_id="GEOID")
  
  temp <- dplyr::left_join(ofm, lep, by=c("tract_id")) %>%
    dplyr::mutate(estimate=round(.data$population*.data$share,0)) %>%
    dplyr::mutate(variable="Limited English Proficiency", geography_type="Census Block", metric="Population", grouping="Equity Focus Area") %>%
    dplyr::select(-"population", -"blockgroup_id", -"tract_id") %>%
    dplyr::rename(geography="geoid")
  
  population <- dplyr::bind_rows(population, temp)
  
  rm(pop, tot, lep, temp)
  
  return(population)
}