# This script was written from the 28th of Feb 2023 to create maps of lizard species in the 
  # Australian Capital Territory. For questions, please contact James B Dorey at jbdorey@me.com


#### 0.0 Script preparation ####
##### 0.1 Working directory ####
# Choose the path to the root folder in which all other folders and data can/will be found
RootPath <- "/Users/jamesdorey/Desktop/Uni/PostDoc_applications/ALA_Tech/ACT_lizards"
setwd(RootPath)

##### 0.2 Packages ####
  ###### a. renv init ####
# Begin by installing and loading renv
install.packages("renv")
library(renv)
# initialise an renv project at the working directory (RootPath)
renv::init() 


  ###### b. install packages #####
# Install only those packages that are not already present in your system
# Choose packages that need to be installed 
# You may need to install gdal on your computer. This can be done on mac by using
# Homebrew in the terminal and the command "brew install gdal"
list.of.packages <- c("R.utils",           # To use gunzip
                      "bdc",               # data cleaning package
                      "tidyr",             #  Part of the tidyverse
                      "magrittr",          # to use pipes
                      "dplyr",             #  Part of the tidyverse
                      "tibble",            # To use tibbles
                      "forcats",           # tidyverse for working with factors
                      "galah",             #  To download ALA data
                      "rlang",             #  Part of the tidyverse — core functions
                      "stringr",           #  Part of the tidyverse — works with text strings
                      "lubridate",         #  Part of the tidyverse — works with dates
                      "tidyselect",        #  Part of the tidyverse
                      "rvest",             # Package for interfacing with and downloading from the internet
                      "rnaturalearth",     #  Global vector map data 
                      "rnaturalearthdata", #  To access the above global map data
                      "janitor",
                      "paletteer",
                      "leaflet",
                      "leaflet.extras2",
                      "crosstalk",
                      "geojsonsf",
                      "readr",             #  Part of the tidyverse — reads files (e.g., .csv)
                      "ggspatial")         #  Makes ggplot2 create north arrows or scale bars

# Install sf separately
renv::install(c("sf"), type = "binary")
# List the new (not installed) packages and then if there are any, install them.
renv::install(packages = c(list.of.packages, "ropensci/rnaturalearthhires"), 
              rebuild = FALSE) # try changing to TRUE if you're having package troubles

  ###### c. load packages ####
# Load all packages from the list specified above, with the addition of "rnaturalearthhires" and "sf"
lapply(c(list.of.packages, "rnaturalearthhires", "sf"), 
       library, character.only = TRUE)

# Create a folder for any potential new functions and read it into the environment
if(!fs::dir_exists(paste0(RootPath, "/NewFunctions"))) {
  fs::dir_create(paste0(RootPath, "/NewFunctions"), recurse = TRUE)
  warning(paste0(" — We created the directory: ", RootPath, "/NewFunctions"))}
ScriptPath <- paste0(RootPath, "/NewFunctions")

### Load in R scripts and character strings in our package
sapply(list.files(file.path(ScriptPath), pattern = ".R$", full.names = TRUE), source)  
# Save a snapshot of the environment
renv::snapshot()


#### 1.0 ALA data ####
  ##### 1.1 Data directory ####
# Create a folder for any potential new functions and read it into the environment
if(!fs::dir_exists(paste0(RootPath, "/lizardData"))) {
  fs::dir_create(paste0(RootPath, "/lizardData"), recurse = TRUE)
  warning(paste0(" — We created the directory: ", RootPath, "/lizardData"))}
DataPath <- paste0(RootPath, "/lizardData")
  
  ##### 1.2 Read data ####
lizardData <- readr::read_csv(paste0(DataPath, 
                                     "/lizardRecords-2023-02-28/lizardRecords-2023-02-28.csv"),
                                # Assign column types and only select a subset
                              col_types = ColTypeR(videos = col_character())) %>%
  suppressWarnings()
  # There are some duplicate columns, but these are not critical for our mapping

# Summary of number of species
table(lizardData$scientificName)
  # there are lots of non-species

  # Filter to species-level records only — if mapping by species
#lizardData <- lizardData %>%
#  dplyr::filter(stringr::str_detect(scientificName, "[A-Z][a-z]+ [a-z]+"))

#### 2. Maps ####
# Make interactive maps to the family level based on a user input column
  # I'm interested in looking at how sampling has changed through time according to basis of record.
  # So, we will produce interactive maps that show this with a slider for filtering dates
source(paste(ScriptPath, "interactiveMapR.R", sep = "/"))
interactiveMapR(
    # occurrence data
  database = lizardData %>%
      # Reassign some levels of basis of record
    dplyr::mutate(basisOfRecord = stringr::str_replace_all(
      basisOfRecord,
      c("^OBSERVATION$" = "HUMAN_OBSERVATION",
      "MATERIAL_SAMPLE" = "PRESERVED_SPECIMEN"))),
    # choose the column to plot  
  column2Plot = "basisOfRecord",
  # lat long columns
  longitude = "decimalLongitude",
  latitude = "decimalLatitude",
    # the associated colour palette — from paleteer and passed to paletteer_dynamic (test below)
  palette = "ggthemes_solarized::violet",
  # Directory where to save files
  dir = paste0(RootPath, "/interactiveMaps"),
  # Occurrence dataset column with species names... in this case use genus
  taxonomyColumn = "family",
  # Which species to map — a character vector of names or "ALL"
  speciesList = "ALL",
    # Jitter points to see stacking
  jitterValue = NULL,
    # Change to TRUE if you want the map to show all points from when it opens
  showAllOnStart = FALSE,
    # Overwrite matching maps in the directory?
  overWrite = TRUE
)
# TEST palettes here, if you want.
paletteer_dynamic("ggthemes_solarized::violet", n = 3)


# Make an interactive map in shiny of the above. These are not as conveniently saved, but prettier
source(paste(ScriptPath, "ShinyMapR.R", sep = "/"))
ShinyMapR(
  # occurrence data
  database = lizardData %>%
    dplyr::mutate(basisOfRecord = stringr::str_replace_all(
      basisOfRecord,
      c("^OBSERVATION$" = "HUMAN_OBSERVATION",
        "MATERIAL_SAMPLE" = "PRESERVED_SPECIMEN"))),
  # column to plot  
  column2Plot = "basisOfRecord",
  # the associated colour palette
  palette = "ggthemes_solarized::violet",
  # Directory where to save files
  dir = paste0(RootPath, "/interactiveMaps"),
  # Occurrence dataset column with species names... in this case use genus
  taxonomyColumn = "family",
  taxon2map = "Elapidae",
  # Which species to map — a character vector of names or "ALL"
  speciesList = "ALL",
  # Jitter points to see stacking
  jitterValue = NULL
)


# Fin.













