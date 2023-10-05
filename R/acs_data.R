#' ACS data for PSRC Counties, Cities, Region and MPOs
#'
#' This function pulls and cleans ACS Data.
#' Pulls data by year for PSRC Counties, Cities, Region and MPOs
#' 
#' @param years Four digit integer (or vector of integers) for year(s) of analysis
#' @param acs_tbl ACS table to be downloaded: defaults to B08301 (Mode to Work)
#' @param acs_variables Name of variable look up to use: defaults to commute-modes
#' @return tibble of mode to work data by census year
#' 
#' @importFrom rlang .data
#' 
#' @examples
#' \dontrun{
#' mode_to_work <- process_acs_data(years=c(2016, 2021))}
#' 
#' @export
#'
process_acs_data <- function(years=c(2021), acs_tbl="B08301", acs_variables="commute-modes") {
  
  # Always use ACS 5yr data since we need it for places, tracts and MPO comparisons as well as 2020 data
  acs_type <- 'acs5'
  
  # Columns to keep from Tidy Census Pull
  cols_to_keep <- c("name", "variable", "estimate", "moe", "census_geography", "year")
  
  # Variables for dashboard
  variables <- readr::read_csv(system.file('extdata', paste0(acs_variables,".csv"), package='psrcrtp'), show_col_types = FALSE)
  
  working_data <- NULL
  for (y in years) {
    print(stringr::str_glue("Working on {y}"))
    
    # County & Region data for PSRC region
    # Download the Data
    county_data <- psrccensus::get_acs_recs(geography = 'county', table.names = acs_tbl, years=y, acs.type = acs_type) 
    # Variables of interest
    county_data <- county_data |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Clean up columns
    county_data <- county_data |> dplyr::select(tidyselect::all_of(cols_to_keep))
    # Add labels
    county_data <- dplyr::left_join(county_data, variables, by=c("variable"))
    # Consolidate rows based on simple labels
    county_data <- county_data |> 
      dplyr::group_by(.data$name, .data$census_geography, .data$year, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label")
    # Get totals
    total <- county_data |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    county_data <- dplyr::left_join(county_data, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    rm(total)
    
    # Cities in the PSRC region
    # Download the Data
    city_data <- psrccensus::get_acs_recs(geography = 'place', table.names = acs_tbl, years=y, acs.type = acs_type) |>
      dplyr::filter(.data$census_geography == "City")
    # Variables of interest
    city_data <- city_data |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Clean up columns
    city_data <- city_data |> dplyr::select(tidyselect::all_of(cols_to_keep))
    # Add labels
    city_data <- dplyr::left_join(city_data, variables, by=c("variable"))
    # Consolidate rows based on simple labels
    city_data <- city_data |> 
      dplyr::group_by(.data$name, .data$census_geography, .data$year, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label")
    # Get totals
    total <- city_data |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    city_data <- dplyr::left_join(city_data, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    rm(total)
    
    # Metro Areas
    mpo <- readr::read_csv(system.file('extdata', 'regional-councils-counties.csv', package='psrcrtp'), show_col_types = FALSE) |> 
      dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) |>
      dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) |>
      dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS))
    
    states <- mpo |> dplyr::select("STATE_FIPS") |> dplyr::distinct() |> dplyr::pull()
    counties <- mpo |> dplyr::select("GEOID") |> dplyr::distinct() |> dplyr::pull()
    
    mpo_data <- NULL
    for (st in states) {
      c <- mpo |> dplyr::filter(.data$STATE_FIPS %in% st) |> dplyr::select("COUNTY_FIPS") |> dplyr::pull()
      d <- tidycensus::get_acs(geography = "county", state=st, county=c, table = acs_tbl, year = y, survey = acs_type)
      ifelse(is.null(mpo_data), mpo_data <- d, mpo_data <- dplyr::bind_rows(mpo_data,d))
      rm(c, d)
    }
    
    # Variables of interest
    mpo_data <- mpo_data |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Add labels
    mpo_data <- dplyr::left_join(mpo_data, variables, by=c("variable"))
    # Add in MPO Information
    mpo_county_data <- dplyr::left_join(mpo, mpo_data, by="GEOID", multiple = "all")
    # Consolidate rows based on simple labels
    mpo_county_data <- mpo_county_data |> 
      dplyr::group_by(.data$MPO_AREA, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label", name = "MPO_AREA") |>
      dplyr::mutate(year=y, census_geography="Metro Areas")
    # Get totals
    total <- mpo_county_data |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    mpo_county_data <- dplyr::left_join(mpo_county_data, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    mpo_data <- mpo_county_data
    rm(total, mpo_county_data)
    
    d <- dplyr::bind_rows(county_data, city_data, mpo_data)
    
    if(is.null(working_data)) {working_data <- d} else {working_data <- dplyr::bind_rows(working_data, d)}
    
  }
  
  # Match column names to rtp-dashboard inputs
  working_data <- working_data |> 
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)), grouping="All", metric=acs_variables) |>
    dplyr::select("date", geography="name", variable="label", geography_type="census_geography", "grouping", "metric", "estimate", "share", "moe")
  
  return(working_data)
}
