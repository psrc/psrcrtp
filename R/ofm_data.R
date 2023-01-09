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
