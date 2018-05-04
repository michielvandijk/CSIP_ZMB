#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Script to process aquastat irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("countrycode")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
aquastat_version <- "AQUASTAT_20171113"

# Crop mapping
aquastat2crop_lvst <- read_excel(file.path(dataPath, "Data/Mappings/Mappings.xlsx"), sheet = "aquastat2crop_lvst")

# Aquastat raw
aquastat_raw <- read_excel(file.path(dataPath, paste0("Data/Global/", aquastat_version, "/aquastat_irrigation.xlsx")), sheet = "data")


### PROCESS IRRIGATED AREA PER CROP
# Clean up database
aquastat <- aquastat_raw %>%
  filter(`Area Id` == fao_sel) %>%
  mutate(iso3c = iso3c_sel) %>%
  transmute(iso3c, variable = `Variable Name`, variable_code = `Variable Id`, year = Year, value = Value)

# Create irrigated area df
# Note that "Total harvested irrigated crop area (full control irrigation)" (4379) is only presented if all crops are included
ir_area <- aquastat %>%
  dplyr::filter(grepl("Harvested irrigated temporary crop area", variable)|
                grepl("Harvested irrigated permanent crop area", variable)|
                variable_code %in% c(4379, 4313)) %>%
  separate(variable, c("variable", "aquastat_crop"), sep = ":") %>%
  mutate(aquastat_crop = trimws(aquastat_crop),
         aquastat_crop = ifelse(is.na(aquastat_crop), "Total", aquastat_crop),
         aquastat_crop = ifelse(aquastat_crop == "total", "Total", aquastat_crop),
         value = value * 1000) # to ha

# Map to crop_lvst crops
# NB: Depending on country select tropical (trof) or temperate fruits for "Other fruits" category if available
# NB: fodder and Grass and fodder are not mapped

ir_area <- ir_area %>%
  left_join(aquastat2crop_lvst) %>%
  group_by(iso3c, variable, year, short_name) %>%
  summarize(value = sum(value, na.rm = T))

# Save
write_csv(ir_area, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/aquastat_ir_crops_", iso3c_sel, ".csv")))
         

### PROCESS IRRIGATED AREA PER TYPE
# To add if needed