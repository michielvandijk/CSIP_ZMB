#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to make figures using GLOBIOM output
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "gdxrrw", "ggthemes", "viridis")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)


### SET DATAPATH
source(file.path(root, "Scripts/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### PREPARE GAMS LINK
igdx(GAMSPath)


### SET COUNTRY
#source("Scripts/Set_country.R")
iso3c_sel <- "ZambiaReg"
country_sel <- "Zambia"


### SET FILE, SCENARIOS AND COLOURS
# File
globiom_file <- "/GLOBIOM/results/a6_SSP2_Zambia"

# Select scenarios
scen <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

# set colours
scen_col <- c("blue", "red", "green", "yellow", "grey")
names(scen_col) <- scen 



### LOAD RAW DATA AND MAPPINGS
# Historical FAO data
fao_hist_raw <- rgdx.param("P:/globiom/Data/FAOSTAT/Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx", "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# Crop data
#crop_raw <- rgdx.param(file.path(modelPath, globiom_file), "CROP_DATA_COMPARE") %>%
#  droplevels()

# Emissions
ghg_proj_raw <- rgdx.param(file.path(dataPath, globiom_file), "GHG_Compare") %>%
  droplevels()

account_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "Account")

# Calories
calo_proj_raw <- rgdx.param(file.path(dataPath, globiom_file), "CALORIECONS2") %>%
  droplevels()

# Prices
price_proj_raw <- rgdx.param(file.path(dataPath, globiom_file), "PRICE_COMPARE2") %>%
  droplevels()

# Land use
land_proj_raw <- rgdx.param(file.path(dataPath, globiom_file), "LAND_COMPARE2") %>%
  droplevels()

lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")





### GHG
## NB ONLY FOR SSP3
ghg_proj <- ghg_proj_raw %>%
  mutate(year = as.integer(as.character(AllScenYear))) %>%
  left_join(account_map) %>%
  group_by(ANYREGION, AllMacroScen, ALLBioenScen, IEA_SCEN, year, ghg_source2) %>%
  summarize(value = sum(GHG_Compare)) %>%
  filter(ANYREGION %in% iso3c_sel)

fig_ghg <- ggplot() +
  geom_line(data = ghg_proj, aes(x = year, y = value, colour = ghg_source2)) +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "Emissions", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none")


### CALORIE CONSUMPTION
# Load historical data
calo_hist <- read.csv(file.path(dataPath, "Data/Historical/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(iso3c %in% "ZMB") %>%
  dplyr::select(iso3c, year = Year, value = Value) 

calo_hist_base <- filter(calo_hist, year == 2000) %>%
  dplyr::rename(base_2000 = value) %>%
  ungroup() %>%
  dplyr::select(-year)


# Projected data
calo_proj <- calo_proj_raw %>% 
  mutate(year = as.integer(as.character(ScenYear))) %>%
  rename(value = CALORIECONS2) %>%
  filter(NUTR_SOURCE == "TOT", ANYREGION == iso3c_sel)

# Rebase simulations 2000 to historical data (2000=100)
# calo_proj <- calo_proj %>%
#   left_join(., calo_hist_base) %>%
#   mutate(value = base_2000*index)

fig_calo <- ggplot() +
  geom_line(data = calo_proj, aes(x = year, y = value)) +
  geom_line(data = calo_hist, aes(x = year, y = value), colour = "blue") +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "kcal/cap/day", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none")


### Prices
# Load historical data
price_hist_raw <- read.csv(file.path(dataPath, "Data/Historical/Prices_E_All_Data_(Normalized).csv"))

# Mean national food price [Probably still need to deflate]
price_hist <- price_hist_raw %>%
  rename(value = Value, year = Year) %>%
  mutate(iso3c = countrycode(Area.Code, "fao", "iso3c")) %>%
  filter(iso3c %in% "ZMB", Unit == "USD", Item == "Maize")


# Projected data
crop_sel <- c("Corn")
price_proj <- price_proj_raw %>% 
  mutate(year = as.integer(as.character(AllScenYear))) %>%
  rename(value = Price_Compare2) %>%
  filter(ANYREGION == "ZambeziBasin", ALLPRODUCT %in% crop_sel)

fig_price <- ggplot() +
  geom_line(data = price_hist, aes(x = year, y = value), colour = "blue") +
  geom_line(data = price_proj, aes(x = year, y = value, colour = ALLPRODUCT)) +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "USD/ton", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none")


### LAND USE
# Historical CrpLnd data (only GLOBIOM CROPS)
crplnd_hist <- fao_hist_raw %>%
  filter(iso3c == "ZMB") %>%
  filter(variable == "AREA", crop %in% c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott",
                                         "Gnut", "Mill", "Pota", "Rape", "Rice", "Soya",
                                         "Srgh", "SugC", "sunf", "SwPo", "Whea")) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(lc_class = "CrpLnd", AllMacroScen = "AllMacroScen")
  

# Load historical data for other land use classes
land_hist_raw <- read.csv(file.path(dataPath, "Data/Historical/Inputs_Land_E_All_Data_(Normalized).csv"))

land_hist <- land_hist_raw %>%
  rename(value = Value, year = Year) %>%
  mutate(iso3c = countrycode(Area.Code, "fao", "iso3c")) %>%
  filter(iso3c %in% "ZMB", Element == "Area", Item.Code %in% c(6620, 6655, 6661)) %>%
  mutate(lc_class = dplyr::recode(Item.Code, `6655` = "GrsLnd", `6661` = "For", .default = NA_character_),
         AllMacroScen = "Historical") %>%
  dplyr::select(year, lc_class, value, AllMacroScen) %>%
  na.omit %>%
  bind_rows(crplnd_hist)

# Projected data
land_proj <- land_proj_raw %>%
  mutate(year = as.integer(as.character(AllScenYear))) %>%
  rename(value = Land_Compare2) %>%
  filter(COUNTRY == country_sel) %>%
  left_join(lc_type_map) %>%
  group_by(lc_class, year, AllMacroScen) %>%
  summarize(value = sum(value, na.rm = T))

fig_land <- ggplot() +
  geom_line(data = land_hist, aes(x = year, y = value, colour = lc_class), linetype = "solid") +
  geom_line(data = land_proj, aes(x = year, y = value, colour = lc_class), linetype = "dashed") +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none")

  