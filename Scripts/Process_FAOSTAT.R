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
faostat_version <- "FAOSTAT_20170117"

# Load crop_lvst2FCL
crop_lvst2FCL <- read_excel("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data\\Mappings\\Mappings.xlsx", sheet = "crop_lvst2FCL") %>%
  dplyr::select(short_name, FCL_item_code) %>%
  na.omit()

# Trade
trade_raw <- read_csv(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/FAOSTAT/", faostat_version, "/Trade_Crops_Livestock_E_All_Data_(Norm).csv"))

# Crop production
prod_raw <- read_csv(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/FAOSTAT", faostat_version, "/Production_Crops_E_All_Data_(Normalized).csv"))

# Livestock production
lvst_raw <- read_csv(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/FAOSTAT", faostat_version, "/Production_Livestock_E_All_Data_(Normalized).csv"))

# Land use
land_raw <- read_csv(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/FAOSTAT", faostat_version, "/Inputs_Land_E_All_Data_(Normalized).csv"))

# Emissions
#emis_ag_raw <- read_csv(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/FAOSTAT", faostat_version, "/Emissions_Agriculture_Agriculture_total_E_All_Data_(Norm).csv"))
#emis_lu_raw <- read_csv(paste0("H:/MyDocuments/Projects/Global-to-local-GLOBIOM/Data/global/FAOSTAT", faostat_version, "/Emissions_Land_Use_Land_Use_Total_E_All_Data_(Norm).csv"))



### PROCESS CROPS 
# Extract country data
crops_raw <- prod_raw %>%
  filter(`Area Code` == fao_sel) %>%
  mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
         iso3c = iso3c_sel) %>%
  dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
  left_join(., crop_lvst2FCL) %>%
  filter(!is.na(value))

# Create files for relevant variables
area <- crops_raw %>%
  filter(unit == "ha", variable == "area") %>%
  na.omit() %>%# remove rows with na values for value
  group_by(short_name, unit, year, variable) %>%
  summarize(value = sum(value, na.rm = T))

prod <- crops_raw %>%
  filter(unit == "tonnes", 1990, variable == "production") %>%
  mutate(unit = replace(unit, unit=="tonnes", "tons")) %>%
  na.omit() %>%# remove rows with na values for value
  group_by(short_name, unit, year, variable) %>%
  summarize(value = sum(value, na.rm = T))
  

# Note that for some crops area or prodution is not available resulting in NA for yield
yld <- bind_rows(area, prod) %>%
  ungroup() %>%
  dplyr::select(-unit) %>%
  spread(variable, value) %>%
  mutate(value = production/area,
         unit = "tons/ha",
         variable = "yield") %>%
  dplyr::select(-production, -area)
  
# number_faostat <- faostat_lvst %>%
#   filter(year >1990) 

# Bind area, production and yield
crops <- bind_rows(area, prod, yld) %>%
  ungroup() %>%
  mutate(source = "FAOSTAT",
         adm_level = 0,
         adm = iso3c_sel) %>%
  na.omit()
summary(crops)
str(crops)

# save files
write_csv(crops, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/faostat_crops_", iso3c_sel, ".csv")))


### LIVESTOCK
# faostat_lvst <- faostat_lvst_raw %>%
#   mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
#          iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
#   dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
#   filter(iso3c == "ZMB") %>%
#   left_join(., FCL) %>%
#   mutate(value = ifelse(FCL_title %in% c("Poultry Birds", "Chickens"), value*1000, value),
#          unit = "Head") %>%
#   filter(!is.na(value), !(class == "total")) %>%
#   dplyr::select(-class)


### TRADE
# Extract country data
trade <- trade_raw %>%
  filter(`Country Code` == fao_sel) %>% 
  mutate(variable = dplyr::recode(Element, "Import Quantity" = "impo_q", "Export Quantity" = "expo_q",
                                  "Import Value" = "impo_v", "Export Value" = "expo_v"),
         iso3c = iso3c_sel) %>%
  dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value, Item) %>%
  left_join(., crop_lvst2FCL) %>%
  filter(!is.na(value)) %>%
  na.omit %>%
  group_by(iso3c, short_name, year, variable, unit) %>%
  summarize(value = sum(value, na.rm = T)) 
# Filter out unmatched aggregate and processed products, such as meat, pellets, etc. 

# save files
write_csv(trade, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/faostat_trade_", iso3c_sel, ".csv")))


### LAND
# Extract country data
land <- land_raw %>%
  filter(`Area Code` == fao_sel) %>%
  dplyr::select(variable = Element, year = Year, unit = Unit, value = Value, item = Item) %>%
  mutate(iso3c = iso3c_sel,
         variable = tolower(variable),
         item = tolower(item)) %>%
  filter(!is.na(value)) %>%
  na.omit # Filter out unmatched aggregate and processed products, such as meat, pellets, etc. 

# save files
write_csv(land, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/faostat_land_", iso3c_sel, ".csv")))

# 
# ### EMISSIONS
## TAKEN FROM CIAT_CDE
# # Extract country data
# emis_ag <- emis_ag_raw %>%
#   filter(`Country Code` == fao_sel) %>%
#   dplyr::select(variable = Element, year = Year, unit = Unit, value = Value, item = Item) %>%
#   mutate(iso3c = iso3c_sel,
#          variable = tolower(variable),
#          item = tolower(item)) %>%
#   filter(!is.na(value))
# 
# emis_lu <- emis_lu_raw %>%
#   filter(`Country Code` == fao_sel) %>%
#   dplyr::select(variable = Element, year = Year, unit = Unit, value = Value, item = Item) %>%
#   mutate(iso3c = iso3c_sel,
#          variable = tolower(variable),
#          item = tolower(item)) %>%
#   filter(!is.na(value))
# 
# # save files
# write_csv(emis_lu, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/faostat_emis_lu_", iso3c_sel, ".csv")))
# write_csv(emis_ag, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/faostat_emis_ag_", iso3c_sel, ".csv")))
