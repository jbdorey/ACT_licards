# This function was written by James B Dorey from the 28th of February 2023
# Its purpose is to create series of output figures (interactive .html maps) for specific taxa
# Please contact jbdorey@me.com for help

interactiveMapR <- function(
    # occurrence data
  database = NULL,
  # Directory where to save files
  dir = NULL,
  column2Plot = NULL,
  # lat long columns
  longitude = "decimalLongitude",
  latitude = "decimalLatitude",
  # Occurrence dataset column with species names
  taxonomyColumn = "scientificName",
  # Which species to map — a character vector of names or "ALL"
  speciesList = NULL,
  jitterValue = NULL,
  overWrite = TRUE,
  showAllOnStart = FALSE,
  palette = "cartography::blue.pal"
){
  require(htmlwidgets)
  require(leaflet)
  require(DT)
  require(fs)
  require(dplyr)
  require(geojsonsf)
  
  #### 0.0 Prep ####
  ##### 0.1 Errors ####
  ###### a. FATAL errors ####
  if(is.null(database)){
    stop(paste0(" — No database was given. Please specify the data that you want to map ",
                "for your data-cleaning adventures. I'll do the rest."))
  }
  if(is.null(dir)){
    stop(paste0(" — No dir was given. Please specify the directory to save the maps to."))
  }
  if(is.null(column2Plot)){
    stop(paste0(" — No column2Plot was given. Please specify the column to map."))
  }
  
  
  ##### 0.2 Packages ####
  # Package names
  packages <- c("leaflet", "htmlwidgets", "plotly", "dplyr")
  
  # Install packages not yet installed, if needed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  
  ##### 0.3 Directories ####
  # Create directory if it does not exist
  if (!fs::dir_exists(dir)) {
    fs::dir_create(dir, recurse = TRUE)}
  # Set directory
  setwd(dir) #directory of work
  
  #### 1.0 Data prep ####
  ##### 1.1 Remove na ####
  database <- database %>%
    tidyr::drop_na(tidyselect::any_of(c(longitude, latitude))) %>%
    tidyr::drop_na(tidyselect::any_of(column2Plot)) %>%
    tidyr::drop_na(tidyselect::any_of(taxonomyColumn))
    
  
  ##### 1.2 Prep. eventDate ####
    # format event date using lubridate
  database <- database %>%
    dplyr::mutate(eventDate = eventDate %>%
                    lubridate::ymd_hms(truncated = 5, quiet = TRUE)) %>%
    dplyr::arrange(eventDate)
  
  
  ##### 1.3 Species list ####
  if(any(stringr::str_detect(speciesList, "ALL")) == FALSE){
    # Prepare the data for the loop
    database <- database %>% 
      # Select ONLY the species requested
      dplyr::filter(.data[[taxonomyColumn]] %in% speciesList)
  }else{
    speciesList <- unique(database[[taxonomyColumn]])
  } # END if else statement
  
  
  ##### 1.4 Overwrite ####
  if(overWrite == FALSE){
    # Find already completed species
    existingFiles <- list.files(path = dir) %>%
      stringr::str_remove("\\.html")
    # remove them from the to-do list
    speciesList <- setdiff(speciesList, existingFiles)
    # Re-filter the database to use only wanted species
    database <- database %>% 
      # Select ONLY the species requested
      dplyr::filter(.data[[taxonomyColumn]] %in% speciesList)
  }
  
  ##### 1.5 Jitter ####
  # If the user specifies a jitter value, add that value
  if(!is.null(jitterValue)){
    database <- database %>%
      dplyr::mutate(
        decimalLongitude = base::jitter(database[[longitude]], amount = jitterValue),
        decimalLatitude = base::jitter(database[[latitude]], amount = jitterValue)
      )
  }else{
    # If no jitter, ensure that the lat lon columns are the same
    database <- database %>%
      dplyr::mutate(
        decimalLongitude = database[[longitude]],
        decimalLatitude = database[[latitude]])
  } # END Jitter
  
  ##### 1.6 Colour ####
    # make the user-specified column into a factor
  database[[column2Plot]] <- database[[column2Plot]] %>% 
    factor()
   # Make colour palette using paleteer
    pointColours <- paletteer_dynamic(palette, n = database[[column2Plot]] %>% 
                                        levels() %>%
                                        length(), direction = 1)
      # Prepare the colour palette for use with leaflet
    colPal = leaflet::colorFactor(pointColours %>% as.character(), 
                                  levels = levels(database[[column2Plot]]))
  
  
  # ensure UTF-8 encoding — otherwise, leaflet will have a bad time
  options(encoding = "UTF-8")
  database <- database %>% mutate(across(where(is.character), 
                                         function(x){iconv(x, 
                                                           to = "UTF-8",
                                                           sub = "")}))
  
  #### 2.0 produce maps ####
  #function for leaflet maps
  # Loop through all of the provided taxa and make a map for each
  for(x in 1:length(speciesList)){
    # Filter to the xth species
    databaseSpp <- database %>% 
      dplyr::filter(.data[[taxonomyColumn]] == speciesList[[x]] %>% 
                        # Ensure UTF-8 encoding in species name (to match the database)
                      iconv(x, from = "UTF-8", to = "UTF-8", sub = "")) %>%
        # Remove occurrences wihtout coordinates
      tidyr::drop_na(longitude, latitude) %>%
      # Create an sf object for point input
      sf::st_as_sf(.,
                   coords = c(longitude, latitude),
                   na.fail = TRUE)
    
    
    ##### 2.1 Base map ####
      # Build the basemap with leaflet
    mdatabaseSpp <<- leaflet::leaflet(data = databaseSpp) %>% 
      # Base map groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        # Add the legend
      leaflet::addLegend(color = pointColours[1:length(levels(databaseSpp[[column2Plot]]))],
                         labels = levels(databaseSpp[[column2Plot]]), 
                         group = levels(databaseSpp[[column2Plot]]),
                         title = column2Plot) %>%
        # Add the basemap controller
      leaflet::addLayersControl(
        baseGroups = c("OSM (default)", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)) %>%
        # Add a scale bar
      leaflet::addScaleBar(position = c("bottomleft"), 
                           options = scaleBarOptions(metric = TRUE))
    
    #### 2.2 Points ####
        # Add the circles and timeslider to the map
        mdatabaseSpp <<- mdatabaseSpp %>%
          leaflet.extras2::addTimeslider(data = databaseSpp,
                                    options = timesliderOptions(
                                      position = "topright",
                                      timeAttribute = "eventDate",
                                      showAllOnStart = showAllOnStart,
                                      alwaysShowDate = TRUE,
                                      rezoom = FALSE,
                                      range = TRUE),
                                    #group = taxonomyColumn,
                                      ###### a. popup ####
                                    # Build the .html code for the occurrence data points' popups
                                           popup = stringr::str_c(
                                             sep = "",
                                             "<b>Basic data </b> — ",
                                             if("family" %in% colnames(databaseSpp)){
                                               paste0("Family: ", databaseSpp$family, 
                                                      ";   ")},
                                             if("scientificName" %in% colnames(databaseSpp)){
                                               paste0("Species: ", databaseSpp$scientificName, 
                                                      ";   ")},
                                             if("institutionCode" %in% colnames(databaseSpp)){
                                               paste0(" institutionCode: ", databaseSpp$institutionCode, 
                                                      ";   ")},
                                             if("catalogNumber" %in% colnames(databaseSpp)){
                                               paste0("catalogNumber: ", databaseSpp$catalogNumber, 
                                                      ";   ")},
                                             if("verbatimScientificName" %in% colnames(databaseSpp)){
                                               paste0("Original name: ", databaseSpp$verbatimScientificName, 
                                                      ";   ")},
                                             if("scientificNameAuthorship" %in% colnames(databaseSpp)){
                                               paste0("Authority: ", databaseSpp$scientificNameAuthorship, 
                                                      ";   ")},
                                             "<p></p><b>Collection data</b> — ",
                                             if("recordedBy" %in% colnames(databaseSpp)){
                                               paste0("Collector(s): ", databaseSpp$recordedBy, 
                                                      ";   ")},
                                             if("year" %in% colnames(databaseSpp)){
                                               paste0("Year: ", databaseSpp$year, 
                                                      ";   ")},
                                             if("identifiedBy" %in% colnames(databaseSpp)){
                                               paste0("Identified by: ", databaseSpp$identifiedBy, 
                                                      ";   ")},
                                             if("country" %in% colnames(databaseSpp)){
                                               paste0("Country: ", databaseSpp$country, 
                                                      ";   ")},
                                             if("references" %in% colnames(databaseSpp)){
                                               paste0("References: ", databaseSpp$references, 
                                                      "   ")},
                                             "<p></p><b>column2Plot</b> — ",
                                               paste0("column2Plot: ", databaseSpp[[column2Plot]], 
                                                      ";   ")
                                           ), #you can add what do you want from columns of your database
                                           
                                           #### b. colour ####
                                           fillOpacity = 0.5,
                                           opacity = 1,
                                           # colour determined by if else
                                           fillColor = colPal(databaseSpp[[column2Plot]]),
                                           # Stroke color
                                           color = "black",
                                           # Internal size
                                           radius = 8,
                                           # border size
                                           weight = 1.5) 
  
    #### 2.3 Save ####
    #then, it is to save in html format
    htmlwidgets::saveWidget(plotly::as_widget(mdatabaseSpp), 
                            file.path(dir, #directory to save files
                                      paste0(speciesList[[x]],".html")),
                            selfcontained = TRUE,
                            title = paste0(speciesList[[x]]))
  } # END for loop
  
} # END function

