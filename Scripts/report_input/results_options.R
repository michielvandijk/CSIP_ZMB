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
  setNames(c("scenario", "variable", "unit", "ANYREGION", "item", "ssp", "bioscen", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(ANYREGION == "ZambiaReg")

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v2.xlsx"))

# LC Map
#lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year))) 

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


### PLOT FUNCTIONS

# Growth 2000-2050 
plot_growth <- function(df){
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
    #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
    guides(fill=F, colour = F) +
    labs(x = "", y = "growth (2000-2050)", title = unique(df$variable)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free")
  p
}

# Difference in 2050 (%)
plot_dif <- function(df){
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
    #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
    guides(fill=F, colour = F) +
    labs(x = "", y = "dif BAU-option in 2050 (%)", title = unique(df$variable)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free")
  p
}


# Baseline selection
base_options <- zmb %>% 
  ungroup() %>%
  filter(scen_type %in% c("baseline", "CSA", "irrigation"), 
         gcm == "noCC", SSP == "SSP2", year == 2050)
  

### YIELD
# Selected crops
crop_sel <- c("Corn", "Cass", "Gnut", "Mill")

# projections
yld_opt <- base_options %>%
  filter(item %in% crop_sel, variable == "YIRF", unit == "fm t/ha") %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)

fig_opt_yld <- plot_growth(yld_opt)
  
fig_opt_yld_dif <- plot_dif(yld_opt)

  
### EMISSIONS
emis_opt <- base_options %>%
  filter(variable == "EMIS") %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O")) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)

fig_opt_emis <- plot_growth(emis_opt)

fig_opt_emis_dif <- plot_dif(emis_opt)


### LAND
land_opt <- base_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "Forest","GrsLnd", "NatLnd")) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)

fig_opt_land <- plot_growth(land_opt)

fig_opt_land_dif <- plot_dif(land_opt)


### PRICE
price_opt <- base_options %>%
  filter(variable == "XPRP", item %in% crop_sel) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)


fig_opt_price <- plot_growth(price_opt)

fig_opt_price_dif <- plot_dif(price_opt)


### TRADE
trade_opt <- base_options %>%
  filter(variable == "NTMS", item %in% crop_sel) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"]))*100)


fig_opt_trade <- ggplot(data = trade_opt) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050)", title = unique(trade_opt$variable)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")

fig_opt_trade_dif <- ggplot(data = trade_opt) +
  geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", y = "growth (2000-2050)", title = unique(trade_opt$variable)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~item, scales = "free")


### LIVESTOCK
lvst_opt1 <- base_options %>%
  filter(variable == "Anim", item %in% c("BVMEAT","SGMEAT", "PGMEAT", "PTMEAT","ALMILK")) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)


fig_opt_lvst1 <- plot_growth(lvst_opt1)

fig_opt_lvst_dif1 <- plot_dif(lvst_opt1)


lvst_opt2 <- base_options %>%
  filter(variable == "Anim", item %in% c("PIGS","BOVD", "BOVO", "BOVF","SGTO", "PTRB","PTRH")) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)

fig_opt_lvst2 <- plot_growth(lvst_opt2)

fig_opt_lvst_dif2 <- plot_dif(lvst_opt2)


### CONSUMPTION
cons_opt <- base_options %>%
  filter(variable == "CONS", item %in% crop_sel) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)


fig_opt_cons <- plot_growth(cons_opt)

fig_opt_cons_dif <- plot_dif(cons_opt)


### PRODUCTION
prod_opt <- base_options %>%
  filter(variable == "Prod", item %in% crop_sel) %>%
  ungroup() %>%
  group_by(variable, item, unit) %>%
  mutate(dif = ((value-value[scenario == "output_CSIP_ZMB-1"])/value[scenario == "output_CSIP_ZMB-1"])*100)


fig_opt_prod <- plot_growth(prod_opt)

fig_opt_prod_dif <- plot_dif(prod_opt)

