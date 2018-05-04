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
zmb_raw <- rgdx.param(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "OUTPUT") %>%
  setNames(c("scenario", "variable", "unit", "ANYREGION", "item", "ssp", "bioscen", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(ANYREGION == "ZambiaReg")

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def.xlsx"))

# LC Map
#lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(scenario = gsub("-", "_", scenario),
         year = as.integer(as.character(year))) 

# Check for missing 2010 values
check2010 <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, bioscen, year) %>%
  group_by(variable, scenario, item, unit, ssp, bioscen) %>%
  filter(!any(year==2010))

# # Remove series with missing values in 2010
zmb <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, bioscen, year) %>%
  group_by(variable, scenario, item, unit, ssp, bioscen) %>%
  filter(any(year==2010))
xtabs(~item + variable, data = zmb)

# Add growth and index
zmb <- zmb %>%
  group_by(variable, scenario, item, unit, ssp, bioscen) %>%
  mutate(
    index = value/value[year == 2010],
    growth = (index-1)*100)

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def)


### YIELD
# Selected crops
crop_sel <- c("Corn", "Cass", "Gnut", "Mill")

# projections
yld_opt <- zmb %>%
  filter(item %in% crop_sel, variable == "YIRF", unit == "fm t/ha") %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)

# yld_proj_df <- bind_rows(
#   filter(yld_proj, scen_def == "Baseline", year == 2010) %>% 
#     mutate(option = "2010"), 
#   filter(yld_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
#          SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
#   group_by(option) %>%
#   mutate(min_val = min(value),
#          max_val = max(value)) %>%
#   filter(gcm == "nocc") %>%
#   ungroup()

fig_opt_yld <- ggplot(data = yld_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### EMISSIONS
emis_opt <- zmb %>%
  filter(variable == "EMIS") %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)

fig_opt_emis <- ggplot(data = emis_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### LAND
land_opt <- zmb %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "Forest","GrsLnd", "PriFor")) %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
       gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)


fig_opt_land <- ggplot(data = land_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### PRICE
price_opt <- zmb %>%
  filter(variable == "XPRP", item %in% crop_sel) %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)

fig_opt_price <- ggplot(data = price_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### TRADE
trade_opt <- zmb %>%
  filter(variable == "NTMS", item %in% crop_sel) %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)

fig_opt_trade <- ggplot(data = trade_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### LIVESTOCK
lvst_opt <- zmb %>%
  filter(variable == "Anim") %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)

fig_opt_lvst <- ggplot(data = lvst_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### CONSUMPTION
cons_opt <- zmb %>%
  filter(variable == "CONS", item %in% crop_sel) %>%
  filter(option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         gcm == "nocc", SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)

fig_opt_cons <- ggplot(data = cons_opt) +
  geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


