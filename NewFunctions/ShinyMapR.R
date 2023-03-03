# This function was written by James B Dorey from the 28th of February 2023
# Its purpose is to create series of output figures or csv files for specified species
# Please contact jbdorey@me.com for help

ShinyMapR <- function(
    # occurrence data
  database = NULL,
  # Directory where to save files
  dir = NULL,
  column2Plot = NULL,
  # Occurrence dataset column with species names
  taxonomyColumn = "scientificName",
  # Which species to map — a character vector of names or "ALL"
  speciesList = NULL,
  jitterValue = NULL,
  taxon2map = NULL,
  palette = "cartography::blue.pal"
){
  require(htmlwidgets)
  require(leaflet)
  require(DT)
  require(fs)
  require(dplyr)
  require(shiny)
  
  
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
  
  if(is.null(taxon2map)){
    stop(paste0(" — No taxon2map was given. Please specify a taxon to map."))
  }
  
  ##### 0.2 Packages ####
  # Package names
  packages <- c("leaflet", "htmlwidgets", "plotly", "dplyr")
  
  # Install packages not yet installed
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
  
  # database$IDall <- paste0(1:nrow(database)) #to add an ID by row
  
  #### 1.0 Data prep ####
  ##### 1.1 Remove na ####
  database <- database %>%
    tidyr::drop_na(decimalLongitude, decimalLatitude) %>%
    tidyr::drop_na(tidyselect::any_of(column2Plot))
  
  ##### 1.2 Prep. eventData ####
  database <- database %>%
    dplyr::mutate(eventDate = eventDate %>%
                    lubridate::year()) %>%
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
  
  ##### 1.4 Jitter ####
  # If the user specifies a jitter value, add that calue
  if(!is.null(jitterValue)){
    database <- database %>%
      dplyr::mutate(
        decimalLongitude = base::jitter(database$decimalLongitude, amount = jitterValue),
        decimalLatitude = base::jitter(database$decimalLatitude, amount = jitterValue)
      )
  }
  
  ##### 1.5 colour ####
  database[[column2Plot]] <- database[[column2Plot]] %>% 
    factor()
  # Make colour palette 
  pointColours <- paletteer_dynamic(palette, n = database[[column2Plot]] %>% 
                                      levels() %>%
                                      length(), direction = 1)
  colPal = leaflet::colorFactor(pointColours %>% as.character(), 
                                levels = levels(database[[column2Plot]]))
  
  
  
  # ensure UTF-8 encoding
  options(encoding = "UTF-8")
  database <- database %>% mutate(across(where(is.character), 
                                         function(x){iconv(x, 
                                                           to = "UTF-8",
                                                           sub = "")}))
  
  #### 2.0 produce maps ####
    ##### 2.1 Prep. data ####
    # Filter to the xth species
    databaseSpp <- database %>% 
      dplyr::filter(.data[[taxonomyColumn]] == taxon2map %>% 
                      # Ensure UTF-8 encoding in text
                      iconv(., from = "UTF-8", to = "UTF-8", sub = "")) %>%
      tidyr::drop_na(decimalLongitude, decimalLatitude, eventDate) %>%
      dplyr::mutate(lng = decimalLongitude %>% as.numeric(),
                    lat = decimalLatitude)
    
      ##### 2.2 Build ui ####
    # Create a Shiny page
    ui <- shiny::bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        # Render the leaflet output
      leaflet::leafletOutput("map", width = "100%", height = "100%"),
      
      shiny::absolutePanel(top = 10, right = 10,
                    shiny::sliderInput("range", "Dates", min(databaseSpp$eventDate, na.rm = TRUE), 
                                max(databaseSpp$eventDate, na.rm = TRUE),
                                value = range(databaseSpp$eventDate, na.rm = TRUE), step = 1
                    ),
                    shiny::selectInput("colors", "Color Scheme",
                                       rownames(subset(RColorBrewer::brewer.pal.info, category %in% c("qual")))                    ),
                    shiny::checkboxInput("legend", "Show legend", TRUE)
      )) # End bootstrap
    
      ##### 2.3 Build server ####
      # Reactive expression for the data subsetted to what the user selected
    server <- function(input, output, session) {
      filteredData <- shiny::reactive({
        databaseSpp %>%
          dplyr::filter(eventDate <= input$range)
      })

      
      # This reactive expression represents the palette function,
      # which changes as the user makes selections in UI.
      colorpal <- shiny::reactive({
        leaflet::colorFactor(input$colors, databaseSpp[[column2Plot]])
      })
      
      output$map <- leaflet::renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet::leaflet(data = databaseSpp ) %>% 
          # Base map groups
          leaflet::addTiles(group = "OSM (default)") %>%
          #leaflet::addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          leaflet::fitBounds(~min(decimalLongitude), ~min(decimalLatitude), 
                             ~max(decimalLongitude), ~max(decimalLatitude))
      })
      # Incremental changes to the map (in this case, replacing the
      # circles when a new color is chosen) should be performed in
      # an observer. Each independent set of things that can change
      # should be managed in its own observer.
      shiny::observe({
        pal <- colorpal()
        leaflet::leafletProxy("map", data = filteredData()) %>%
          leaflet::clearMarkers() %>%
          leaflet::addCircleMarkers(#data = filteredData(),
                                    lng = ~lng, lat = ~lat, ###then you can specify what do you want in the popup window from your database
                                    group = column2Plot,
                                    #group = taxonomyColumn,
                                    #### a. point popup ####
                                    popup = stringr::str_c(
                                      sep = "",
                                      "<b>Basic data </b> — ",
                                      if("family" %in% colnames(databaseSpp)){
                                        paste0("Family: ", filteredData()$family, 
                                               ";   ")},
                                      if("scientificName" %in% colnames(databaseSpp)){
                                        paste0("Species: ", filteredData()$scientificName, 
                                               ";   ")},
                                      if("institutionCode" %in% colnames(databaseSpp)){
                                        paste0(" institutionCode: ", filteredData()$institutionCode, 
                                               ";   ")},
                                      if("catalogNumber" %in% colnames(databaseSpp)){
                                        paste0("catalogNumber: ", filteredData()$catalogNumber, 
                                               ";   ")},
                                      if("verbatimScientificName" %in% colnames(databaseSpp)){
                                        paste0("Original name: ", filteredData()$verbatimScientificName, 
                                               ";   ")},
                                      if("scientificNameAuthorship" %in% colnames(databaseSpp)){
                                        paste0("Authority: ", filteredData()$scientificNameAuthorship, 
                                               ";   ")},
                                      "<p></p><b>Collection data</b> — ",
                                      if("recordedBy" %in% colnames(databaseSpp)){
                                        paste0("Collector(s): ", filteredData()$recordedBy, 
                                               ";   ")},
                                      if("year" %in% colnames(databaseSpp)){
                                        paste0("Year: ", filteredData()$year, 
                                               ";   ")},
                                      if("identifiedBy" %in% colnames(databaseSpp)){
                                        paste0("Identified by: ", filteredData()$identifiedBy, 
                                               ";   ")},
                                      if("country" %in% colnames(databaseSpp)){
                                        paste0("Country: ", filteredData()$country, 
                                               ";   ")},
                                      if("references" %in% colnames(databaseSpp)){
                                        paste0("References: ", filteredData()$references, 
                                               "   ")},
                                      "<p></p><b>column2Plot</b> — ",
                                      paste0("column2Plot: ", filteredData()[[column2Plot]], 
                                             ";   ")
                                    ), #you can add what do you want from columns of your database
                                    
                                    #### b. colour ####
                                    fillOpacity = 0.5,
                                    opacity = 1,
                                    # colour determined by if else
                                    fillColor = pal(filteredData()[[column2Plot]]),
                                    # Stroke color
                                    color = "black",
                                    # Internal size
                                    radius = 8,
                                    # border size
                                    weight = 1.5
                                    )
      })
      shiny::observe({
        proxy <- leaflet::leafletProxy("map", data = databaseSpp)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% leaflet::clearControls()
        if (input$legend) {
          pal <- colorpal()
          proxy %>% leaflet::addLegend(pal = pal,
                                       values = ~databaseSpp[[column2Plot]],
                                       title = column2Plot,
                                       position = "bottomright")
        }
      }) # END observe
    }# END server function
    
  ##### 2.4 Return map ####
    return(shiny::shinyApp(ui, server))
    
  
} # END function

