#'========================================================================================================================================
#' Project:  CSIP
#' Subject:  Script to process FAOSTAT data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode")



### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Scripts/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Scripts/Set_country.R")


### DOWNLOAD
ciat_cde_version <- "CIAT_CDE_20180202"

# FAOSTAT and other emissions
ciat_raw <- read_excel(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/CIAT_CDE/", ciat_cde_version, "/CW_CAIT_GHG_Emissions_31102017.xlsx"), sheet = "GHG Emissions", skip = 1) %>%
  gather(variable, value, -Country, -Year) %>%
  mutate(variable = tolower(variable), 
         source = "CIAT_CDE",
         unit = "MtCO2e") %>%
  rename(year = Year)
  
# UNFCC
unfcc_raw <- read_excel(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/CIAT_CDE/", ciat_cde_version, "/CW UNFCCC_GHG_Emissions_31102017.xlsx"), sheet = "CW_HistoricalEmissions", skip = 1, na = c("NA", "NO")) %>%
  filter(!is.na(Country)) %>%
  gather(year, value, -Country, -Source, -Sector, -Gas, -GWP) %>%
  setNames(tolower(names(.))) %>%
  rename(variable = sector, iso3c = country) %>%
  mutate(year = as.integer(year),
         unit = "MtCO2e",
         variable = tolower(variable))
  


### EXTRACT
# ciat
ciat <- ciat_raw %>%
  mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
  filter(iso3c == iso3c_sel) %>%
  dplyr::select(-Country) %>%
  na.omit

# unfcc
unfcc <- unfcc_raw %>%
  filter(iso3c == iso3c_sel) %>%
  na.omit


# save files
write_csv(ciat, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ciat_emis_", iso3c_sel, ".csv")))
write_csv(unfcc, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/unfcc_emis_", iso3c_sel, ".csv")))
