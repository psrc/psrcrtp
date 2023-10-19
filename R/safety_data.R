#' Fatal Collisions from Washington Coded Fatal Crash (CFC) Data Files
#'
#' This function processes annual data from the summarized Fatal Collision Data
#' from the Washington State Traffic Safety Commission
#' It adds City, County, Regional Geography, Centers, 2010 and 2020 Census Tracts,
#' Displacement Risk Score and Historically Disadvantaged Communities to the
#' Fatal Collision Data. The final data contains accidents by race, gender, age,
#' mode, roadway type, day of week, time of day, lighting and distracted driving.
#'
#' @param data_years Two digit integer (or vector of integers) for analysis years
#' @return tibble of annual fatal collisions by year
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' fatals <- wstc_fatal_collisions()}
#'
#' @export
#'
wstc_fatal_collisions <- function(data_years=seq(10, 22, by = 1)) {

  wgs84 <- 4326
  spn <- 32148

  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)

  print("Getting City shapefile")
  city <- psrcelmer::st_read_elmergeo("cities") %>% dplyr::select(city="city_name") %>% sf::st_transform(spn)

  print("Getting 2010 shapefile loaded for spatial analysis of collisions prior to 2020")
  tract2010 <- psrcelmer::st_read_elmergeo("tract2010") %>% dplyr::select(tract10="geoid10") %>% sf::st_transform(spn)

  print("Getting 2020 shapefile loaded for spatial analysis of collisions from 2020 onward")
  tract2020 <- psrcelmer::st_read_elmergeo("tract2020") %>% dplyr::select(tract20="geoid20") %>% sf::st_transform(spn)

  print("Getting Regional Geography shapefile")
  regeo <- psrcelmer::st_read_elmergeo("regional_geographies") %>% dplyr::select(rgeo="class_desc") %>% sf::st_transform(spn)
  regeo <- regeo %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "CitiesTowns", "Cities & Towns")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "Core", "Core Cities")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "UU", "Urban Unincorporated")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "Metro", "Metropolitan Cities")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "HCT", "High Capacity Transit Communities"))

  print("Getting Regional Growth Centers shapefile")
  rgc <- psrcelmer::st_read_elmergeo("urban_centers") %>% dplyr::select(rgc="name") %>% sf::st_transform(spn)

  print("Getting Manufacturing and Industrial Centers shapefile")
  mic <- psrcelmer::st_read_elmergeo("micen") %>% dplyr::select(mic="mic") %>% sf::st_transform(spn)

  print("Getting Displacement Risk by Tract")
  displacement <- psrcelmer::st_read_elmergeo("displacement_risk") %>% dplyr::select(tract10="geoid10", "risk_score") %>% sf::st_drop_geometry()

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

  gender_lookup <- data.frame(sex = c(1, 2, 8, 9),
                              gender = c("Male", "Female", "Not Reported", "Unknown"))

  light_lookup <- data.frame(lightcond = c(1, 2, 3, 4,
                                           5, 6, 7, 8,
                                           9),
                             lighting = c("Daylight", "Dark - Not Lighted", "Dark - Lighted", "Dawn",
                                          "Dusk", "Dark - Lighting Unknown", "Other", "Not Reported",
                                          "Unknown"))

  distraction_lookup <- data.frame(distract1 = c(0,1,16,96,
                                                 3,4,5,6,
                                                 7,9,10,12,
                                                 13,14,15,17,
                                                 18,19,92,93,
                                                 97,98,99),
                                   distracted_details = c("Not Distracted", "Looked But Did Not See", "No Driver Present", "Not Reported",
                                                          "By Other Occupant", "By a Moving Object in Vehicle", "While Talking or Listening to Cellular Phone", "While Manipulating Cellular Phone",
                                                          "Adjusting Audio or Climate Controls", "While Using Other Component/Controls Integral to Vehicle", "While Using or Reaching For Device/Object Brought Into Vehicle", "Distracted by Outside Person, Object or Event",
                                                          "Eating or Drinking", "Smoking Related", "Other Cellular Phone Related", "Distraction/Inattention",
                                                          "Distraction/Careless", "Careless/Inattentive", "Distracted, Details Unknown", "Inattentive, Details Unknown",
                                                          "Lost in Thought / Day Dreaming", "Other Distraction", "Unknown if Distract"))


  # Initial processing of fatal crash data
  processed <- NULL
  for (year in data_years) {

    suppressWarnings({
      data_file <- stringr::str_glue("X:/DSA/rtp-dashboard/WTSC/Data/person{year}.xlsx")

      print(stringr::str_glue("Working on 20{year} data processing and cleanup."))

      print("Downloading Fatality Data")

      t <- dplyr::as_tibble(readxl::read_xlsx(data_file)) %>%
        dplyr::filter(.data$county %in% c(33, 35, 53, 61) & .data$injury %in% c(4)) %>%
        dplyr::select(report_number="par", date="crash_dt", time="crash_tm", "lightcond", "distract1", "year", county = "co_char", injuries="numfatal", "race_me", "ptype", lon="x", lat="y", "age", "sex", tidyselect::starts_with("func")) %>%
        dplyr::mutate(race = dplyr::case_when(
          stringr::str_detect(.data$race_me,"White") ~ "white",
          stringr::str_detect(.data$race_me,"Black") ~ "Black or African American",
          stringr::str_detect(.data$race_me,"AIAN") ~ "American Indian and Alaska Native",
          stringr::str_detect(.data$race_me,"API") ~ "Asian and Pacific Islander",
          stringr::str_detect(.data$race_me,"Unknown") ~ "Some other race",
          stringr::str_detect(.data$race_me,"Other") ~ "Some other race",
          stringr::str_detect(.data$race_me,"Oth/Unk ") ~ "Some other race",
          stringr::str_detect(.data$race_me,"Multiracial") ~ "Two or more races",
          stringr::str_detect(.data$race_me,"Hispanic") ~ "Hispanic or Latinx")) %>%
        dplyr::select(-"race_me") %>%
        dplyr::mutate(poc = dplyr::case_when(
          .data$race == "white" ~ "white",
          .data$race != "white" ~ "People of Color")) %>%
        dplyr::mutate(year = lubridate::year(date)) %>%
        dplyr::mutate(day_of_week = lubridate::wday(.data$date, label = TRUE)) %>%
        dplyr::mutate(hour = lubridate::hour(.data$time)) %>%
        dplyr::mutate(time_of_day = dplyr::case_when(
          .data$hour %in% c(22,23,0,1,2,3,4,5) ~ "Overnight",
          .data$hour %in% c(6,7,8) ~ "AM Peak",
          .data$hour %in% c(9,10,11,12,13,14) ~ "Midday",
          .data$hour %in% c(15,16,17) ~ "PM Peak",
          .data$hour %in% c(18,19,20,21) ~ "Evening")) %>%
        dplyr::select(-"time", -"hour") %>%
        dplyr::mutate(age_group = dplyr::case_when(
          .data$age <18 ~ "0 to 18",
          .data$age <30 ~ "18 to 29",
          .data$age <40 ~ "30 to 39",
          .data$age <50 ~ "40 to 49",
          .data$age <65 ~ "50 to 64",
          .data$age <=120 ~ "65+",
          .data$age >120 ~ "Unknown")) %>%
        dplyr::select(-"age") %>%
        dplyr::mutate(distracted = dplyr::case_when(
          .data$distract1 ==0 ~ "No",
          .data$distract1 !=96 ~ "Yes",
          .data$distract1 ==96 ~ "Unknown")) %>%
        tidyr::drop_na()

      print("Cleaning up Person Type")
      t <- dplyr::left_join(t, ptype_lookup, by = c("ptype" = "ptype")) %>% dplyr::select(-"ptype")

      print("Cleaning up Gender")
      t <- dplyr::left_join(t, gender_lookup, by = c("sex" = "sex")) %>% dplyr::select(-"sex")

      print("Cleaning up Light Level")
      t <- dplyr::left_join(t, light_lookup, by = c("lightcond" = "lightcond")) %>% dplyr::select(-"lightcond")

      print("Cleaning up Distraction")
      t <- dplyr::left_join(t, distraction_lookup, by = c("distract1" = "distract1"))%>% dplyr::select(-"distract1")

      print("Cleaning up Functional Classification")
      ifelse(t$year < 2015,
             t <- dplyr::left_join(t, funclass_lookup, by = c("funclass" = "funclass")) %>% dplyr::select(-"funclass"),
             t <- dplyr::left_join(t, funcsys_lookup, by = c("funcsystem" = "funcsystem")) %>% dplyr::select(-"funcsystem"))

      if(is.null(processed)) {processed<-t} else {processed<-dplyr::bind_rows(processed,t)}

    }) # end of warning suppression

  } # end of year loop

  processed <- processed %>% tibble::rowid_to_column("index")

  print("Creating a spatial layer for collisions to join with and buffering 100ft")
  collision_layer <- sf::st_as_sf(processed, coords = c("lon", "lat"), crs = wgs84) %>%
    sf::st_transform(spn) %>%
    dplyr::select("index") %>%
    sf::st_buffer(dist=100)

  print("Getting City name onto collisions")
  temp <- sf::st_intersection(collision_layer, city) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  processed <- dplyr::left_join(processed, temp, by="index") %>% dplyr::mutate(city = tidyr::replace_na(.data$city, "Unincorporated"))

  print("Getting Regional Geographies onto collisions")
  temp <- sf::st_intersection(collision_layer, regeo) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  processed <- dplyr::left_join(processed, temp, by="index") %>% dplyr::mutate(rgeo = tidyr::replace_na(.data$rgeo, "Rural"))

  print("Getting Regional Centers onto collisions")
  temp <- sf::st_intersection(collision_layer, rgc) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  processed <- dplyr::left_join(processed, temp, by="index") %>% dplyr::mutate(rgc = tidyr::replace_na(.data$rgc, "Not in a RGC"))

  print("Getting Manufacturing and Industrial Centers onto collisions")
  temp <- sf::st_intersection(collision_layer, mic) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  processed <- dplyr::left_join(processed, temp, by="index") %>% dplyr::mutate(mic = tidyr::replace_na(.data$mic, "Not in a MIC"))

  print("Getting 2010 Census Tracts onto collisions")
  temp <- sf::st_intersection(collision_layer, tract2010) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  processed <- dplyr::left_join(processed, temp, by="index") %>% dplyr::mutate(tract10 = tidyr::replace_na(.data$tract10, "0"))

  print("Getting 2020 Census Tracts onto collisions")
  temp <- sf::st_intersection(collision_layer, tract2020) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  processed <- dplyr::left_join(processed, temp, by="index") %>% dplyr::mutate(tract20 = tidyr::replace_na(.data$tract20, "0"))

  print("Getting Displacement Risk Score onto collisions")
  processed <- dplyr::left_join(processed, displacement, by="tract10") %>% dplyr::mutate(risk_score = tidyr::replace_na(.data$risk_score, 0))

  print("Getting HDC flag onto collisions")
  temp <- readr::read_csv("X:/DSA/shiny-uploads/data/RAISE_Persistent_Poverty.csv") %>%
    dplyr::filter(.data$State=="Washington" & .data$County %in% c("King","Kitsap", "Pierce", "Snohomish"))

  pop <- tidycensus::get_acs(geography = "tract", state="WA", county=c("King County","Kitsap County", "Pierce County", "Snohomish County"), variables = c("B01001_001"), year = 2019, survey = "acs5") %>%
    tidyr::separate(col=.data$NAME, sep=", ", into=c("Tract", "County", "State")) %>%
    dplyr::mutate(County = gsub(" County","",.data$County)) %>%
    dplyr::rename(population="estimate") %>%
    dplyr::select(-"variable", -"moe")

  temp <- dplyr::left_join(temp, pop, by=c("State","County","Tract")) %>%
    dplyr::select(tract10="GEOID", hdc="HDC_TRACT")

  processed <- dplyr::left_join(processed, temp, by="tract10") %>% dplyr::mutate(hdc = tidyr::replace_na(.data$hdc, "No"))

  print("Final Cleanup")
  processed <- processed %>%
    dplyr::mutate(injury_type="Traffic Related Deaths", injuries=1) %>%
    dplyr::rename(roadway="road_class") %>%
    dplyr::select(-"date", -"distracted_details")

  return(processed)

}

#' Serious Injury Collisions from WSDOT Data Files
#'
#' This function processes annual data from the summarized Injury Collision Data
#' from the Washington State Department of Transportation
#' It adds City, County and Census Tracts to the Collision Data
#' The final data contains collisions by gender, age, mode and roadway type
#'
#' @param data_file Path and Name of pre-processed data file if desired - defaults to NULL
#' @return tibble of annual serious injury collisions by year with city, county and tract ids
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' serious_injury_collision_data <- wsdot_serious_injury_collisions()}
#'
#' @export
#'
wsdot_serious_injury_collisions <- function(data_file=NULL) {
  
  # Coordinate Systems for Spatial Data Analysis
  wgs84 <- 4326
  spn <- 32148
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  if (is.null(data_file)) {
    
    file_path <- "X:/DSA/rtp-dashboard/WSDOT/"
    
    print("Reading data files. This process can take up to 20 minutes depending on network speed so please be patient.")
    
    processed <- NULL
    for (file in list.files(path = file_path, pattern = ".*.csv")) {
      
      t <- dplyr::as_tibble(data.table::fread(paste0(file_path, file), skip = 1)) %>%
        dplyr::filter(.data$`Total Serious Injuries` > 0 & stringr::str_detect(.data$`Injury Type`,"Serious")) %>%
        dplyr::transmute(report_number = .data$`Collision Report Number`,
                         lat = .data$Latitude,
                         lon = .data$Longitude,
                         county = .data$`County Name`,
                         city = ifelse(.data$`City Name` == "", "Unincorporated", .data$`City Name`),
                         inc_date = lubridate::mdy(gsub(" 0:00", "", .data$Date)),
                         inc_time = .data$`Full Time 24`,
                         injury_type = .data$`Injury Type`,
                         fatalities = .data$`Number of Fatalities`,
                         serious_injuries = .data$`Total Serious Injuries`,
                         evident_injuries = .data$`Total Evident Injuries`,
                         possible_injuries = .data$`Total Possible Injuries`,
                         total_injuries = .data$`Total Number of Injuries`,
                         num_bike = .data$`Number of Pedal Cyclists Involved`,
                         num_ped = .data$`Number of Pedestrians Involved`,
                         num_veh = .data$`Number of Vehicles Involved`,
                         roadway = .data$`Collision Report Type`,
                         junction = .data$`Junction Relationship`,
                         weather = .data$Weather,
                         surface = .data$`Roadway Surface Condition`,
                         lighting = .data$`Lighting Condition`,
                         unit_type = .data$`Unit Type Description`,
                         person_type = .data$`Involved Person Type`,
                         vehicle_type = .data$`Vehicle Type`,
                         age = .data$Age,
                         gender = .data$Gender,
                         state_fc = .data$`State Route Federal Functional Class Name`,
                         county_fc = .data$`County_Federal Functional Class Name`,
                         distracted = .data$`Distracted Involved Person Flag`
        )
      
      # Combine summarized tables
      ifelse(is.null(processed),
             processed <- t,
             processed <- dplyr::bind_rows(processed, t))
    } # end of file loop
    
  } else {
    
    print("Loading the pre-processed Serious Injury Collision Data from disk")
    processed <- readr::read_csv(data_file)
    
  }
  
  print("Cleaning up Collision data to match formate for Fatal Collisions")
  injuries <- processed %>%
    dplyr::mutate(inc_time = lubridate::hms(.data$inc_time)) %>%
    dplyr::mutate(hour = lubridate::hour(.data$inc_time)) %>%
    dplyr::mutate(time_of_day = dplyr::case_when(
      .data$hour %in% c(22,23,0,1,2,3,4,5) ~ "Overnight",
      .data$hour %in% c(6,7,8) ~ "AM Peak",
      .data$hour %in% c(9,10,11,12,13,14) ~ "Midday",
      .data$hour %in% c(15,16,17) ~ "PM Peak",
      .data$hour %in% c(18,19,20,21) ~ "Evening")) %>%
    dplyr::select(-"inc_time", -"hour") %>%
    dplyr::mutate(age_group = dplyr::case_when(
      .data$age <18 ~ "0 to 18",
      .data$age <30 ~ "18 to 29",
      .data$age <40 ~ "30 to 39",
      .data$age <50 ~ "40 to 49",
      .data$age <65 ~ "50 to 64",
      .data$age <=120 ~ "65+",
      is.na(.data$age) ~ "Unknown")) %>%
    dplyr::select(-"age") %>%
    dplyr::mutate(year = lubridate::year(.data$inc_date)) %>%
    dplyr::mutate(day_of_week = lubridate::wday(.data$inc_date, label = TRUE)) %>%
    dplyr::mutate(person_mode = dplyr::case_when(
      .data$unit_type =="Motor Vehicle" ~ "Motor Vehicle",
      .data$unit_type =="Pedestrian" ~ "Walking",
      .data$unit_type == "Pedalcyclist" ~ "Biking")) %>%
    dplyr::select(-"unit_type", -"person_type", -"vehicle_type") %>%
    dplyr::mutate(injury_type="Serious Injury", injuries=1) %>%
    dplyr::select(-"city", -"inc_date", -"fatalities", -"serious_injuries", -"evident_injuries", -"possible_injuries", -"total_injuries",-"num_bike",-"num_ped", -"num_veh", -"junction", -"weather", -"surface", -"state_fc", -"county_fc") %>%
    tidyr::drop_na(.data$lat) %>%
    tidyr::drop_na(.data$lon) %>%
    tibble::rowid_to_column("index")
  
  print("Getting City shapefile")
  city <- psrcelmer::st_read_elmergeo("cities") %>% dplyr::select(city="city_name") %>% sf::st_transform(spn)
  
  print("Getting 2010 shapefile loaded for spatial analysis of collisions prior to 2020")
  tract2010 <- psrcelmer::st_read_elmergeo("tract2010") %>% dplyr::select(tract10="geoid10") %>% sf::st_transform(spn)
  
  print("Getting 2020 shapefile loaded for spatial analysis of collisions from 2020 onward")
  tract2020 <- psrcelmer::st_read_elmergeo("tract2020") %>% dplyr::select(tract20="geoid20") %>% sf::st_transform(spn)
  
  print("Getting Regional Geography shapefile")
  regeo <- psrcelmer::st_read_elmergeo("regional_geographies") %>% dplyr::select(rgeo="class_desc") %>% sf::st_transform(spn)
  regeo <- regeo %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "CitiesTowns", "Cities & Towns")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "Core", "Core Cities")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "UU", "Urban Unincorporated")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "Metro", "Metropolitan Cities")) %>%
    dplyr::mutate(rgeo = stringr::str_replace_all(.data$rgeo, "HCT", "High Capacity Transit Communities"))
  
  print("Getting Regional Growth Centers shapefile")
  rgc <- psrcelmer::st_read_elmergeo("urban_centers") %>% dplyr::select(rgc="name") %>% sf::st_transform(spn)
  
  print("Getting Manufacturing and Industrial Centers shapefile")
  mic <- psrcelmer::st_read_elmergeo("micen") %>% dplyr::select(mic="mic") %>% sf::st_transform(spn)
  
  print("Creating a spatial layer for collisions to join with and buffering 100ft")
  collision_layer <- sf::st_as_sf(injuries, coords = c("lon", "lat"), crs = wgs84) %>%
    sf::st_transform(spn) %>%
    dplyr::select("index") %>%
    sf::st_buffer(dist=100)
  
  print("Getting City name onto collisions")
  temp <- sf::st_intersection(collision_layer, city) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  injuries <- dplyr::left_join(injuries, temp, by="index") %>% dplyr::mutate(city = tidyr::replace_na(.data$city, "Unincorporated"))
  
  print("Getting Regional Geographies onto collisions")
  temp <- sf::st_intersection(collision_layer, regeo) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  injuries <- dplyr::left_join(injuries, temp, by="index") %>% dplyr::mutate(rgeo = tidyr::replace_na(.data$rgeo, "Rural"))
  
  print("Getting Regional Centers onto collisions")
  temp <- sf::st_intersection(collision_layer, rgc) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  injuries <- dplyr::left_join(injuries, temp, by="index") %>% dplyr::mutate(rgc = tidyr::replace_na(.data$rgc, "Not in a RGC"))
  
  print("Getting Manufacturing and Industrial Centers onto collisions")
  temp <- sf::st_intersection(collision_layer, mic) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  injuries <- dplyr::left_join(injuries, temp, by="index") %>% dplyr::mutate(mic = tidyr::replace_na(.data$mic, "Not in a MIC"))
  
  print("Getting 2010 Census Tracts onto collisions")
  temp <- sf::st_intersection(collision_layer, tract2010) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  injuries <- dplyr::left_join(injuries, temp, by="index") %>% dplyr::mutate(tract10 = tidyr::replace_na(.data$tract10, "0"))
  
  print("Getting 2020 Census Tracts onto collisions")
  temp <- sf::st_intersection(collision_layer, tract2020) %>% sf::st_drop_geometry() %>% dplyr::distinct(.data$index, .keep_all = TRUE)
  injuries <- dplyr::left_join(injuries, temp, by="index") %>% dplyr::mutate(tract20 = tidyr::replace_na(.data$tract20, "0"))
  
  print("Getting Displacement Risk Score onto collisions")
  temp <- sf::st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Displacement_Risk_Data/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tract10="geoid10", "risk_score")
  injuries <- dplyr::left_join(injuries, temp, by="tract10") %>% dplyr::mutate(risk_score = tidyr::replace_na(.data$risk_score, 0))
  
  print("Getting HDC flag onto collisions")
  temp <- readr::read_csv("X:/DSA/shiny-uploads/data/RAISE_Persistent_Poverty.csv") %>%
    dplyr::filter(.data$State=="Washington" & .data$County %in% c("King","Kitsap", "Pierce", "Snohomish"))
  
  pop <- tidycensus::get_acs(geography = "tract", state="WA", county=c("King County","Kitsap County", "Pierce County", "Snohomish County"), variables = c("B01001_001"), year = 2019, survey = "acs5") %>%
    tidyr::separate(col=.data$NAME, sep=", ", into=c("Tract", "County", "State")) %>%
    dplyr::mutate(County = gsub(" County","",.data$County)) %>%
    dplyr::rename(population="estimate") %>%
    dplyr::select(-"variable", -"moe")
  
  temp <- dplyr::left_join(temp, pop, by=c("State","County","Tract")) %>%
    dplyr::select(tract10="GEOID", hdc="HDC_TRACT")
  
  injuries <- dplyr::left_join(injuries, temp, by="tract10") %>% dplyr::mutate(hdc = tidyr::replace_na(.data$hdc, "No"))
  
  return(injuries)
  
}

#' Summarize Collision Data for RTP Dashboard
#'
#' This function summarizes the processed fatal and serious collision data into a format
#' for the RTP Dahsboard. Summaries include by Region, County, Race, Age Group, Gender and Mode.
#'
#' @param data_file Path and Name of pre-processed data file
#' @return tibble of annual fatal and serious injury collisions and rates
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' collision_data <- summarise_collision_data(data_file="processed_collision_data.rds")}
#'
#' @export
#'
summarise_collision_data <- function(data_file) {
  
  collision_data <- readRDS(data_file)
  
  #########################################################################
  # Population Data by Year for Rate Calculations
  #########################################################################
  latest_census_year <- lubridate::year(Sys.Date())-2
  population_years <- seq(min(unique(collision_data$year)), latest_census_year, by=1)
  
  population <- NULL
  for (y in population_years) {
    
    # Age, Gender and Total Population is in Table B01001
    d <- psrccensus::get_acs_recs(geography = 'county', table.names = "B01001", years=y, acs.type = 'acs5')
    
    # County and Region
    c <-  d |>
      dplyr::filter(.data$variable ==  "B01001_001") |>
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)))|>
      dplyr::select("date", geography="name", population="estimate") |>
      dplyr::mutate(geography = stringr::str_remove_all(.data$geography, " County"), grouping = "All")
    
    # Gender
    g <- d |>
      dplyr::filter(.data$variable %in% c("B01001_002", "B01001_026") & .data$name == "Region") |>
      dplyr::mutate(grouping = dplyr::case_when(
        .data$variable ==  "B01001_002" ~ "Male",
        .data$variable ==  "B01001_026" ~ "Female")) |> 
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
      dplyr::select("date", geography="name", "grouping", population="estimate")
    
    # Age
    a <- d |>
      dplyr::filter(!(.data$variable %in% c("B01001_001", "B01001_002", "B01001_026"))) |>
      dplyr::filter(.data$name == "Region") |>
      dplyr::mutate(grouping = dplyr::case_when(
        .data$variable %in% c("B01001_003","B01001_004","B01001_005","B01001_006") ~ "0 to 18",
        .data$variable %in% c("B01001_027","B01001_028","B01001_029","B01001_030") ~ "0 to 18",
        .data$variable %in% c("B01001_007","B01001_008","B01001_009","B01001_010","B01001_011") ~ "18 to 29",
        .data$variable %in% c("B01001_031","B01001_032","B01001_033","B01001_034","B01001_035") ~ "18 to 29",
        .data$variable %in% c("B01001_012","B01001_013") ~ "30 to 39",
        .data$variable %in% c("B01001_036","B01001_037") ~ "30 to 39",
        .data$variable %in% c("B01001_014","B01001_015") ~ "40 to 49",
        .data$variable %in% c("B01001_038","B01001_039") ~ "40 to 49",
        .data$variable %in% c("B01001_016","B01001_017","B01001_018","B01001_019") ~ "50 to 64",
        .data$variable %in% c("B01001_040","B01001_041","B01001_042","B01001_043") ~ "50 to 64",
        .data$variable %in% c("B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025") ~ "65+",
        .data$variable %in% c("B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049") ~ "65+")) |> 
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
      dplyr::select("date", geography="name", "grouping", population="estimate") |>
      dplyr::group_by(.data$date, .data$geography, .data$grouping) |>
      dplyr::summarise(population = sum(.data$population)) |>
      dplyr::as_tibble()
    
    # Race & Ethnicity
    d <- psrccensus::get_acs_recs(geography = 'county', table.names = "B03002", years=y, acs.type = 'acs5')
    r <- d |>
      dplyr::filter(.data$variable %in% c("B03002_003", "B03002_004", "B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009", "B03002_012")) |>
      dplyr::filter(.data$name == "Region") |>
      dplyr::mutate(grouping = dplyr::case_when(
        .data$variable %in% c("B03002_003") ~ "white",
        .data$variable %in% c("B03002_004") ~ "Black or African American",
        .data$variable %in% c("B03002_005") ~ "American Indian and Alaska Native",
        .data$variable %in% c("B03002_006") ~ "Asian and Pacific Islander",
        .data$variable %in% c("B03002_007") ~ "Asian and Pacific Islander",
        .data$variable %in% c("B03002_008") ~ "Some other race",
        .data$variable %in% c("B03002_009") ~ "Two or more races",
        .data$variable %in% c("B03002_012") ~ "Hispanic or Latinx")) |>
      dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
      dplyr::select("date", geography="name", "grouping", population="estimate") |>
      dplyr::group_by(.data$date, .data$geography, .data$grouping) |>
      dplyr::summarise(population = sum(.data$population)) |>
      dplyr::as_tibble()
    
    t <- dplyr::bind_rows(c,g,a,r)
    if(is.null(population)) {population <- t} else {population <- dplyr::bind_rows(population, t)}
    rm(c,g,a,r,t,d)
  }
  
  #########################################################################
  # Collision Data by Year
  #########################################################################
  
  # Create Regional Summary by Year
  region <- collision_data |>
    dplyr::select("year", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography="Region", geography_type="Region", grouping="All", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create County Summary by Year
  county <- collision_data |>
    dplyr::select("year", "county", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$county, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", geography = "county") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="County", grouping="All", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create Race & Ethnicity Summary by Year
  race <- collision_data |>
    dplyr::select("year", "race", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$race, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "race") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Race", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate") |>
    dplyr::filter(.data$metric == "Traffic Related Deaths")
  
  # Create Age Group Summary by Year
  age <- collision_data |>
    dplyr::select("year", "age_group", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$age_group, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "age_group") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Age Group", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create Gender Summary by Year
  gender <- collision_data |>
    dplyr::select("year", "gender", "injury_type", "injuries") |>
    dplyr::mutate(gender = tidyr::replace_na(.data$gender, "Not Reported")) |>
    dplyr::group_by(.data$year, .data$gender, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "gender") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Gender", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create Person Mode Summary by Year
  mode <- collision_data |>
    dplyr::select("year", "person_mode", "injury_type", "injuries") |>
    dplyr::mutate(person_mode = dplyr::case_when(
      .data$person_mode %in% c("Biking", "Motor Vehicle", "Walking") ~ person_mode,
      !(.data$person_mode %in% c("Biking", "Motor Vehicle", "Walking")) ~ "Other")) |>
    dplyr::group_by(.data$year, .data$person_mode, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "person_mode") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Mode", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create Day of Week Summary by Year
  dow <- collision_data |>
    dplyr::select("year", "day_of_week", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$day_of_week, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "day_of_week") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Day of Week", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create Time of Day Summary by Year
  tod <- collision_data |>
    dplyr::select("year", "time_of_day", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$time_of_day, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "time_of_day") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Time of Day", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create Roadway Summary by Year
  roadway <- collision_data |>
    dplyr::select("year", "roadway", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$roadway, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "roadway") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Roadway Type", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  # Create HDC Summary by Year
  hdc <- collision_data |>
    dplyr::select("year", "hdc", "injury_type", "injuries") |>
    dplyr::group_by(.data$year, .data$hdc, .data$injury_type) |>
    dplyr::summarise(estimate = sum(.data$injuries)) |>
    dplyr::as_tibble() |>
    dplyr::rename(metric = "injury_type", grouping = "hdc") |>
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year))) |>
    dplyr::mutate(geography_type="Historically Disadvantaged Community", geography="Region", variable="Total") |>
    dplyr::select("date", "geography", "variable", "geography_type", "grouping", "metric", "estimate")
  
  collisions <- dplyr::bind_rows(region, county, race, age, gender, mode, dow, tod, roadway, hdc)
  rm(region, county, race, age, gender, mode, dow, tod, roadway, hdc)
  
  #########################################################################
  # Collision Rates by Year
  #########################################################################
  
  rates <- dplyr::left_join(collisions, population, by=c("date", "geography", "grouping")) |>
    dplyr::mutate(rate = .data$estimate / (.data$population/100000)) |>
    dplyr::mutate(variable = "Rate per 100k people") |>
    dplyr::select(-"population", -"estimate") |>
    dplyr::rename(estimate = "rate")
  
  final <- dplyr::bind_rows(collisions, rates)
  return(final)
}

#' Summarize MPO Collision Data for RTP Dashboard
#'
#' This function summarizes the processed fatal collision data from FARS into a format
#' for the RTP Dashboard. Summaries include Total and Rate per 100k people for the 27 largest MPO's.
#'
#' @param safety_yrs Vector of years in four digit integer format to analyze - defaults to 2010 to 2021
#' @return tibble of annual fatal collisions and rates for Metro regions
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' mpo_collisions <- process_mpo_fars_data(safety_yrs=c(seq(2020,2021,by=1)))}
#'
#' @export
#'
process_mpo_fars_data<- function(safety_yrs=c(seq(2010,2021,by=1))) {
  
  # Figure of which counties are in each MPO
  mpo_file <- system.file('extdata', 'regional-councils-counties.csv', package='psrcrtp')
  
  mpo <- readr::read_csv(mpo_file, show_col_types = FALSE) |> 
    dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) |>
    dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) |>
    dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS))
  
  states <- mpo |> dplyr::select("STATE_FIPS") |> dplyr::distinct() |> dplyr::pull()
  counties <- mpo |> dplyr::select("GEOID") |> dplyr::distinct() |> dplyr::pull()
  
  # Get Population Data from ACS using TidyCensus
  mpo_county_data <- NULL
  for(yr in safety_yrs) {
    print(paste0("Working of population data for ",yr))
    
    for (st in states) {
      
      c <- mpo |> dplyr::filter(.data$STATE_FIPS %in% st) |> dplyr::select("COUNTY_FIPS") |> dplyr::pull()
      
      pop <- tidycensus::get_acs(geography = "county", state=st, county=c, variables = c("B03002_001"), year = yr, survey = "acs5") |> 
        dplyr::select(-"moe") |> 
        dplyr::mutate(data_year=yr, variable="Population") |>
        dplyr::select(-"NAME")
      
      ifelse(is.null(mpo_county_data), mpo_county_data <- pop, mpo_county_data <- dplyr::bind_rows(mpo_county_data,pop))
      
      rm(c, pop)
    }
  }
  
  mpo_county_data <- dplyr::left_join(mpo, mpo_county_data, by="GEOID")
  
  pop_data <- mpo_county_data |>
    dplyr::select("MPO_AREA", "variable", "estimate", "data_year") |>
    dplyr::rename(geography="MPO_AREA") |>
    dplyr::group_by(.data$geography, .data$variable, .data$data_year) |>
    dplyr::summarise(estimate=sum(.data$estimate)) |>
    tidyr::as_tibble() |>
    dplyr::mutate(metric="Population", geography_type="Metro Regions", grouping="All", variable="Total")
  
  # Fatality Data
  collision_data <- NULL
  for (y in safety_yrs) {
    
    # Open Current Years FARS Accident Data
    
    all_files <- as.character(utils::unzip(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), list = TRUE)$Name)
    
    print(paste0("Working of FARS Fatalities data for ",y))
    f <- readr::read_csv(unz(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), all_files[1]), show_col_types = FALSE) |>
      dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY, width=3, side=c("left"), pad="0")) |>
      dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE, width=2, side=c("left"), pad="0")) |>
      dplyr::mutate(GEOID = paste0(.data$STATE_FIPS, .data$COUNTY_FIPS)) |>
      dplyr::filter(.data$GEOID %in% counties) |>
      dplyr::select("GEOID", "FATALS") |>
      dplyr::group_by(.data$GEOID) |>
      dplyr::summarise(estimate=sum(.data$FATALS)) |>
      tidyr::as_tibble() |>
      dplyr::mutate(data_year=y, variable="Total")
    
    ifelse(is.null(collision_data), collision_data <- f, collision_data <- dplyr::bind_rows(collision_data,f))
    
    rm(f)
    
  }
  
  # Annual Fatality Data
  mpo_county_data <- dplyr::left_join(mpo, collision_data, by="GEOID")
  
  fatality_data <- mpo_county_data |>
    dplyr::select("MPO_AREA", "variable", "estimate", "data_year") |>
    dplyr::rename(geography="MPO_AREA") |>
    dplyr::group_by(.data$geography, .data$variable, .data$data_year) |>
    dplyr::summarise(estimate=sum(.data$estimate)) |>
    tidyr::as_tibble() |>
    dplyr::mutate(metric="Traffic Related Deaths", geography_type="Metro Regions", grouping="All") |>
    tidyr::drop_na()
  
  # Calculate Per Capita Rates
  p <- pop_data |>
    dplyr::select("geography", "data_year", "estimate") |> 
    dplyr::rename(population="estimate")
  
  rate_data <- dplyr::left_join(fatality_data, p, by=c("geography","data_year")) |>
    dplyr::mutate(estimate=(.data$estimate/.data$population)*100000, variable="Rate per 100k people") |>
    dplyr::select(-"population")
  
  fars_data <- dplyr::bind_rows(fatality_data, rate_data) |>
    dplyr::mutate(date=lubridate::ymd(paste0(.data$data_year,"-12-01"))) |>
    dplyr::select(-"data_year")
  
  return(fars_data)
}
