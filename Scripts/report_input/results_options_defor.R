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

# LC Map
#lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")


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
         carbon_price = factor(carbon_price, levels = c("vl1", "vl2", "vl3", "low", "high", "100", "max")))

# # Select relevant scenarios
for_options <- zmb %>%
  filter(option %in% c("msd", "rr", "none"),
         rcp == "noCC",
         year == 2050) 


### PLOT FUNCTIONS
# Colours
# col_options <- brewer.pal(n = 9, name = "Set1")
# names(col_options) = c("none", "af", "ca", "rr", "msd", "dtm", "phl", "div")

### PRODUCTION
# Total production
prod_opt <- for_options %>%
  filter(variable == "PROD", item %in% crop_globiom, unit == "1000 t dm") %>%
  ungroup() %>%
  group_by(year, variable, unit, gcm, crop_model, scen_type, rcp, ssp, scenario, option, carbon_price) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "TOT") %>%
  ungroup() %>%
  filter(option == "none") %>%
  group_by(variable, item, unit, ssp, option) %>%
  mutate(dif = (value-value[scenario == "0_Ref"])/value[scenario == "0_Ref"]*100)

# Plot
fig_for_prod <- ggplot(data = prod_opt) +
    geom_col(aes(x = carbon_price, y = dif, fill = carbon_price), colour = "black") +
    guides(fill=F, colour = F) +
    #scale_fill_manual(values = col_options) +
    labs(x = "", y = "dif BAU-scen in 2050 (%)") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~scen_type, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
 

### LAND
land_opt <- for_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "NatLnd", "GrsLnd", "Forest")) %>%
  filter(option == "none") %>%
  ungroup() %>%
  group_by(variable, item, unit, ssp, option) %>%
  mutate(dif_abs = (value-value[scen_type == "none"])) %>%
  mutate(dif = (value-value[scenario == "0_Ref"])/value[scenario == "0_Ref"]*100)

# Plot
fig_for_land <- ggplot(data = land_opt) +
  geom_col(aes(x = carbon_price, y = dif_abs, fill = carbon_price), colour = "black") +
  guides(fill=F, colour = F) +
  #scale_fill_manual(values = col_options) +
  labs(x = "", y = "dif BAU-scen in 2050 (1000 ha)") + 
  theme_bw() +
  scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(item~scen_type, scales = "free") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 


### EMISSIONS
# Aggregated
emis_opt <- for_options %>%
  filter(variable == "EMIS") %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "TOT")) %>%
  ungroup() %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management",
                       "ManmgtTot_CH4" = "Manure Management",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change")) %>%
  group_by(year, variable, item, unit, gcm, crop_model, rcp, ssp, option, scenario, scen_type, crop_model, carbon_price) %>%
  summarize(value = sum(value)) %>%
  filter(option == "none")
  
emis_opt_ag <- emis_opt %>%
  #filter(item != "Land use change") %>%
  filter(item == "Synthetic Fertilizers") %>%
  ungroup() %>%
  group_by(variable, item, unit, ssp, option) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

emis_opt_lulucf <- emis_opt %>%
  filter(item == "Land use change") %>%
  ungroup() %>%
  group_by(variable, item, unit, ssp, option) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

# Plot
fig_for_emis <- ggplot(data = emis_opt_lulucf) +
  geom_col(aes(x = carbon_price, y = dif, fill = carbon_price), colour = "black") +
  guides(fill=F, colour = F) +
  #scale_fill_manual(values = col_options) +
  labs(x = "", y = "dif BAU-scen in 2050 (%)") + 
  theme_bw() +
  scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~scen_type, scales = "free") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
