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

# Historical FAO data
fao_hist_globiom_raw <- read_csv(file.path(projectPath, "/Data/ZMB/Processed/Agricultural_statistics/faostat_hist_globiom_ZMB.csv"))

# Historical lvst statistics
lvst_hist_raw <- read_csv(file.path(projectPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_lvst_ZMB.csv")) 


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year)),
         variable = toupper(variable)) %>%
  dplyr::filter(variable %in% c("YILM", "ASYS2", "EMIS", "LAND", "XPRP", "FRTN", "ANIM", "CONS", "PROD", "NETT", "CALO", "EXPO", "IMPO", "NTMS", "NTMS2"))

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
  filter(option %in% c("none", "msd", "none", "af", "ca", "rr"))


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

# Livestock
lvst_globiom <- c("BOVO", "BOVD", "BOVF")

# Projections
lvst <- zmb %>% 
  filter(item %in% lvst_globiom,  
         year %in% c(2010, 2030, 2050)) %>%
  group_by(scenario, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "catt",
         legend = "BAU")

lvst_hist <- lvst_hist_raw %>%
  mutate(legend = "Historical",
         value = value/1000) %>%
  rename(item = lvst) %>%
  filter(item == "catt",
         year %in% c(1961, 1970, 1980, 1990, 2000))

# Rescale to FAOSTAT
lvst_proj <- zmb %>% 
  filter(item %in% lvst_globiom,  
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
         year %in% c(2000:2050)) %>%
  group_by(scenario, scenario, year) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, scenario) %>%
  mutate(value = (value/value[year == 2000])* lvst_hist$value[lvst_hist$year == 2000],
         item = "catt",
         legend = "BAU") %>%
  filter(year %in% c(2010, 2030, 2040, 2050))

# Price
price <- zmb %>%
  filter( variable == "XPRP", year %in% c(2010, 2030, 2050))

# Trade
# Projected exports
crop_trade <- c("Corn", "Cass", "Mill", "Gnut", "Cott", "Rice", "Soya", "BVMEAT", "PGMEAT", "SugC", "SwPo", "Whea")

trade_proj <- zmb %>%
  filter(variable %in% c("NETT"), 
         unit %in% c("1000 t"),
         year %in% c(2010, 2030, 2050), item %in% c(crop_trade, "BVMEAT", "PGMEAT")) %>%
  ungroup() %>%
  mutate(item = recode(item, 
                       "Barl" = "Barley",
                       "BeaD" = "Dry beans",
                       "Cass" = "Cassava",
                       "ChkP" = "Chick peas",
                       "Corn" = "Maize",
                       "Cott" = "Cotton",
                       "Gnut" = "Groundnuts",
                       "Mill" = "Millet",
                       "Pota" = "Potatoes",
                       "Rape" = "Rapeseed",
                       "Rice" = "Rice",
                       "Soya" = "Soybeans",
                       "Srgh" = "Sorghum",
                       "SugC" = "Sugarcane",
                       "sunf" = "Sunflowers",
                       "SwPo" = "Sweet potatoes", 
                       "Whea" = "Wheat",
                       "BVMEAT" = "Bovine meat",
                       "PGMEAT" = "Pig meat"))


# Crop yield
yld_crops_sel <- c("Corn", "Cass", "Gnut", "Mill")

# projections
yld_proj <- zmb %>%
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",  
         gcm == "noCC", rcp == "noCC") %>%
  ungroup() %>%
  dplyr::select(year, item, value, variable, scenario, ssp) %>%
  mutate(item = recode(item, 
                       "Barl" = "Barley",
                       "BeaD" = "Dry beans",
                       "Cass" = "Cassava",
                       "ChkP" = "Chick peas",
                       "Corn" = "Maize",
                       "Cott" = "Cotton",
                       "Gnut" = "Groundnuts",
                       "Mill" = "Millet",
                       "Pota" = "Potatoes",
                       "Rape" = "Rapeseed",
                       "Rice" = "Rice",
                       "Soya" = "Soybeans",
                       "Srgh" = "Sorghum",
                       "SugC" = "Sugarcane",
                       "sunf" = "Sunflowers",
                       "SwPo" = "Sweet potatoes", 
                       "Whea" = "Wheat")) %>%
filter(year %in% c(2010, 2030, 2050))

# Irrigation
irr_proj <- zmb %>%
  filter(variable %in% c("ASYS2"), 
         scenario == "0_Ref",
         item %in% c("IR_basin", "IR_drip", "IR_sprink")) %>%
  group_by(scenario, ssp, year, unit) %>%
  summarize(value = sum(value, na.rm = T)) 

### LAND
# Need to recalibrate to FAO grassland in 2000 as we did in the report
# Combine CrpLnd, Grsland and forest data (only GLOBIOM CROPS)
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")

land_hist <- bind_rows(
  fao_hist_globiom_raw %>%
    filter(variable == "AREA", crop %in% crop_globiom) %>%
    group_by(year) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(item = "Cropland", scenario = "Historical"),
  fao_hist_globiom_raw %>%
    filter(crop %in% c("LVS")) %>%
    mutate(scenario = "Historical", 
           item = "Grassland"))

land_proj <- zmb %>%
  filter(variable == "LAND") %>%
  filter(year %in% c(2000, 2010, 2030, 2050))

# Correct land: scale grassland to FAO as we do in the figures
# Correct Grassland so that it links with historical information. Take from natural land
hist_2000 <- land_hist$value[land_hist$item == "Grassland" & land_hist$year == 2000]
proj_2000 <- unique(land_proj$value[land_proj$item == "GrsLnd" & land_proj$year == 2000])
corr <- hist_2000-proj_2000

land_proj$value[land_proj$item == "GrsLnd"] <- land_proj$value[land_proj$item == "GrsLnd"] + corr
land_proj$value[land_proj$item == "NatLnd"] <- land_proj$value[land_proj$item == "NatLnd"] - corr

land_proj <- filter(land_proj, year %in% c(2010, 2030, 2050))


### COMBINE IN ONE FILE
library(openxlsx)

l_db <- list("area_crop_csa" = area_crop_csa, "production" = prod, "land" = land_proj,
             "fertilizer_N" = fert, "emissions" = emis, "price" = price, "lvst" = lvst_proj,
             "trade" = trade_proj, "yield" = yld_proj, "irrigation" = irr_proj)
write.xlsx(l_db, file.path(projectPath, paste0("Data/ZMB/Processed/For_FAO/GLOBIOM_output_for_FAO_", Sys.Date(), ".xlsx")))
