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
source(file.path(root, "Scripts/support/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### PREPARE GAMS LINK
igdx(GAMSPath)

# For now set gdx file here as all results have not yet merged
#globiom_file <- "output_CSIP_ZMB_SSP2_23oct"

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

# carbon price scenarios
carbon_price_raw <- read_csv(file.path(projectPath, "Data/carbon_price/carbon_price_scenarios.csv"))


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year)),
         variable = toupper(variable)) %>%
  dplyr::filter(variable %in% c("YILM", "EMIS", "LAND", "XPRP", "ANIM", "CONS", "PROD", "NETT", "CALO", "EXPO", "IMPO", "NTMS", "NTMS2"))

# Check for missing 2010 values
check2010 <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  filter(!any(year==2010))

# # Remove series with missing values in 2010
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

# Add scenario and carbon price definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl", "div", "def")),
         carbon_price = factor(carbon_price, levels = c("vl1", "vl2", "vl3", "low", "high", "vhigh", "max")))

# # Select relevant scenarios
for_options <- zmb %>%
  filter(option %in% c("none"),
         rcp == "noCC",
         year == 2050) 


### PLOT FUNCTIONS
# Colours
# col_options <- brewer.pal(n = 9, name = "Set1")
# names(col_options) = c("none", "af", "ca", "rr", "msd", "dtm", "phl", "div")

crop_globiom <- "Corn"

### PRODUCTION
# Total production
prod_opt_bau <- for_options %>%
  filter(variable == "PROD", item %in% crop_globiom, unit == "1000 t dm", ssp == "SSP2") %>%
  ungroup() %>%
  group_by(year, variable, unit, gcm, crop_model, scen_type, rcp, ssp, scenario, option, carbon_price) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "TOT") %>%
  ungroup() %>%
  filter(option == "none") %>%
  group_by(variable, item, unit, ssp, option) %>%
  mutate(dif = (value-value[scenario == "0_Ref"])/value[scenario == "0_Ref"]*100) %>%
  filter(!is.na(carbon_price)) %>%
  mutate(scen_type = ifelse(scen_type == "FixAg", "LUC ag", "LUC crp"))

prod_opt_option <- for_options %>%
  filter(variable == "PROD", item %in% crop_globiom, unit == "1000 t dm", ssp == "SSP2") %>%
  ungroup() %>%
  group_by(year, variable, unit, gcm, crop_model, scen_type, rcp, ssp, scenario, option, carbon_price) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "TOT") %>%
  ungroup() %>%
  filter(!scen_type %in% c("none", "CSA")) %>%
  group_by(variable, item, unit, ssp, scen_type, carbon_price) %>%
  mutate(dif = (value-value[option == "none"])/value[option == "none"]*100,
        class = paste(scen_type, option, sep = "_")) %>%
  filter(option != "none")

# Plot
fig_for_prod_bau <- ggplot(data = prod_opt_bau) +
    geom_col(aes(x = carbon_price, y = dif, fill = scen_type), position = "dodge2", colour = "black") +
    #scale_fill_manual(values = col_options) +
    labs(x = "", y = "Difference to BAU scenario in 2050 (%)", fill = "") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.05, 0)), breaks = pretty_breaks(n = 10)) +
    scale_fill_manual(values =  brewer.pal(n = 8, name = "Paired")[c(5,6)]) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
  theme(legend.position = "bottom")
 

### CARBON PRICE SCENARIOS
carbon_price <- carbon_price_raw %>%
  gather(price, value, - year)

fig_carbon_price <- ggplot(data = carbon_price) +
  geom_line(aes(x = year, y = value, colour = price, group = price)) +
  theme_bw() +
  labs(y = "USD/t CO2eq",
       x = "",
       colour = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom")



### CROPLAND AREA
cropland <- for_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd"), ssp == "SSP2") %>%
  mutate(carbon_price = as.character(carbon_price), 
         carbon_price = ifelse(is.na(carbon_price), "none", carbon_price),
         carbon_price = factor(carbon_price, levels = c("vl1", "vl2", "vl3", "low", "high", "vhigh", "max", "none")),
         scen_type = ifelse(scen_type == "none", "BAU", scen_type),
         scen_type = recode(scen_type, "FixAg" = "LUC ag", "FixCrp" = "LUC crp"),
         value = value/1000)

fig_for_cropland <- ggplot(data = cropland) +
  geom_col(aes(x = carbon_price, y = value, fill = scen_type), position = "dodge2", colour = "black") +
  theme_bw() +
  scale_y_continuous(expand = expand_scale(mult = c(0.00, 0.1))) +
  labs(y = "Mha",
       x = "",
       fill = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom")



rm(cropland)
