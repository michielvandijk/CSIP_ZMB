#'========================================================================================================================================
#' Project:  CSIP Zambia
#' Subject:  Script to prepare datafile for CBA analysis of FAO
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
p_load("WDI", "countrycode", "gdxrrw", "ggthemes", "viridis")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)


### SET DATAPATH
source(file.path(root, "Scripts/support/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### PREPARE GAMS LINK
igdx(GAMSPath)


### LOAD DATA
# Zambia GLOBIOM OUTPUT Data
zmb_raw <- rgdx.param(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "OUTPUT_ZMB") %>%
  setNames(c("scenario2", "variable", "unit", "ANYREGION", "item", "ssp", "scenario", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year)),
         item = as.character(item)) %>%
  filter(ANYREGION == "ZambiaReg") %>%
  droplevels

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v3.xlsx")) 

# Zambia GLOBIOM CROPTECH_COMPARE
crop_tech_raw <- rgdx.param(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "CROPTECH_COMPARE") %>%
  setNames(c("scenario2", "ANYREGION", "item", "system", "tech", "ssp", "scenario", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year)),
         item = as.character(item),
         unit = "1000 ha") %>%
  filter(ANYREGION == "ZambiaReg") %>%
  droplevels


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year)),
         variable = toupper(variable)) %>%
  dplyr::filter(variable %in% c("YILM", "EMIS", "LAND", "XPRP", "FRTN", "ANIM", "CONS", "PROD", "NETT", "CALO", "EXPO", "IMPO", "NTMS", "NTMS2"))

# Check for missing 2010 values
check2010 <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
zmb <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  filter(any(year==2010))
xtabs(~item + variable, data = zmb)

# Add growth and index
zmb <- zmb %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  mutate(
    index = value/value[year == 2010],
    growth = (index-1)*100)

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl", "div", "def")))

# Only select msd
zmb <- zmb %>%
  filter(option %in% c("msd", "none", "af", "ca", "rr"))



### PROCESS DATA
# Ha area per crop and CSA practice
area_crop_csa <- crop_tech_raw %>%
  left_join(., scen_def) %>%
  mutate(tech = recode(tech, "notill" = "msd", "r100"= "rr", "af_csip"= "af", "ca_csip" = "ca")) %>%
  filter(option == tech) %>% # only select option data for option specific scenarios (i.e. not msd for af scenarios)
  filter(year %in% c(2010, 2030, 2050)) %>%
  group_by(ssp, item, option, scenario, year, unit) %>%
  summarize(value = sum(value, na.rm = T))  %>%
  ungroup() %>%
  group_by(ssp, option, scenario, year, unit) %>%
  mutate(crop_share = value/sum(value, na.rm = T)*100)
  


# Prod
prod <- zmb %>%
  filter(variable == "PROD", unit == "1000 t dm") %>%
  filter(year %in% c(2010, 2030, 2050))

# Land
land <- zmb %>%
  filter(variable == "LAND") %>%
  filter(year %in% c(2010, 2030, 2050))

# Fert
fert <- zmb %>%
  filter(variable == "FRTN", unit == "1000 t") %>%
  filter(year %in% c(2010, 2030, 2050))

# Emis
emis <- zmb %>%
  ungroup %>%
  filter(variable == "EMIS", year %in% c(2010, 2030, 2050)) %>%
  filter(!item %in% c("LUCF", "Net", "Soil_N2O", "TOT")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management N20",
                       "ManmgtTot_CH4" = "Manure Management CH4",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change",
                       "LUCP" = "Deforestation",
                       "LUCC" = "Other land use change",
                       "LUCG" = "other land use change")) %>%
  group_by(variable, year, item, ssp, scen_type, gcm, rcp, scenario, unit, crop_model) %>%
  summarize(value = sum(value))

# Price
price <- zmb %>%
  filter( variable == "XPRP", year %in% c(2010, 2030, 2050))


### COMBINE IN ONE FILE
library(openxlsx)

l_db <- list("area_crop_csa" = area_crop_csa, "production" = prod, "land" = land,
             "fertilizer_N" = fert, "emissions" = emis, "price" = price)
write.xlsx(l_db, file.path(projectPath, paste0("Data/ZMB/Processed/For_FAO/GLOBIOM_output_for_FAO_", Sys.Date(), ".xlsx")))
