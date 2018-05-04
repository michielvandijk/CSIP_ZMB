#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to prepare conservation agriculture data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "haven")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")



### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### LOAD DATA
# crop output
ca1_raw <- read_dta(file.path(raw_path, "Household_surveys\\2012\\RALS2012\\field.dta")) %>%
  transmute(cluster, hh_code = hh, field, land_use = as_factor(F01), 
            area_ha = hect, dambo = as_factor(F08), crop_hs = as.character(as_factor(F15)), crop_hs_2010 = as.character(as_factor(F13)), residue = as_factor(F14))
            
ca2_raw <- read_dta(file.path(raw_path, "household_surveys/2012/RALS2012/field_cult.dta")) %>%
  transmute(cluster, hh_code = hh, field, monocrop = as_factor(FL02), 
            tillage = as_factor(FL04))

### CLEAN UP

