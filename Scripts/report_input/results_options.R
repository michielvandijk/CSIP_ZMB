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
  mutate(year = as.integer(as.character(year))) %>%
  filter(ANYREGION == "ZambiaReg") %>%
  droplevels

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v3.xlsx")) 

# LC Map
#lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year))) 

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

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl")))


### PLOT FUNCTIONS

# # Growth 2000-2050 
# plot_growth <- function(df){
#   p = ggplot(data = df) +
#     geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
#     #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
#     guides(fill=F, colour = F) +
#     labs(x = "", y = "growth (2000-2050)", title = unique(df$variable)) + 
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     facet_wrap(~item, scales = "free")
#   p
# }
# 
# # Difference in 2050 (%)
# plot_dif <- function(df){
#   p = ggplot(data = df) +
#     geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
#     #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
#     guides(fill=F, colour = F) +
#     labs(x = "", y = "dif BAU-option in 2050 (%)", title = unique(df$variable)) + 
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     facet_wrap(~item, scales = "free")
#   p
# }


# Growth 2000-2050 with cc
plot_growth <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel) %>%
    ungroup %>%
    group_by(option, item) %>%
    mutate(max_val = max(growth, na.rm = T),
           min_val = min(growth, na.rm = T)) %>%
    filter(rcp == "noCC", year == 2050)

    p = ggplot(data = df) +
    geom_col(aes(x = option, y = growth, fill = scenario, colour = scenario)) +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    labs(x = "", y = "growth (2010-2050)", title = unique(df$variable)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free")
  p
}

# Difference in 2050 (%) with cc
plot_dif <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel, option != "none") %>%
    ungroup %>%
    group_by(option, item) %>%
    mutate(max_val = max(dif, na.rm = T),
           min_val = min(dif, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
           ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    #geom_errorbar(aes(x = option, ymin = l_qtl, ymax = u_qtl), width = 0.5, colour = "green") +
    guides(fill=F, colour = F) +
    labs(x = "", y = "dif BAU-option in 2050 (%)", title = unique(df$variable)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free")
  p
}


# Plot absolute values in 2010 and 2050
plot_abs <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel) %>%
    ungroup %>%
    group_by(option, item, year) %>%
    mutate(max_val = max(value, na.rm = T),
           min_val = min(value, na.rm = T)) %>%
    filter(rcp == "noCC", year %in% c(2050))
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    labs(x = "", y = unique(df$unit), title = unique(df$variable)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~ item, scales = "free")
  p
}

# Baseline selection
# base_options <- zmb %>% 
#   ungroup() %>%
#   filter(scen_type %in% c("baseline", "CSA", "irrigation"), 
#          gcm == "noCC", ssp == "SSP2", year == 2050)

cc_options <- zmb  %>%
  ungroup() %>%
  filter(scen_type %in% c("none", "CSA")) %>%
  filter(year == 2050) 


### YIELD
# Selected crops
crop_sel <- c("Corn", "Cass", "Gnut", "Mill")
crop_sel <- c("Corn")


# projections
yld_opt <- cc_options %>%
  filter(item %in% crop_sel, variable == "YILM", unit == "fm t/ha") %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[option == "none"])/value*100)

fig_opt_yld <- plot_growth(yld_opt, "SSP2")
  
fig_opt_yld_dif <- plot_dif(yld_opt, "SSP2")

fig_opt_yld_abs <- plot_abs(yld_opt, "SSP2")


### EMISSIONS
emis_opt <- cc_options %>%
  filter(variable == "EMIS") %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O")) %>%
  filter(item %in% "TOT") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)

fig_opt_emis <- plot_growth(emis_opt, "SSP2")

fig_opt_emis_dif <- plot_dif(emis_opt, "SSP2")

fig_opt_emis_abs <- plot_abs(emis_opt, "SSP2")


### LAND
land_opt <- cc_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "NatLnd")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)

fig_opt_land <- plot_growth(land_opt, "SSP2")

fig_opt_land_dif <- plot_dif(land_opt, "SSP2")

fig_opt_land_abs <- plot_abs(land_opt, "SSP2")


### PRICE
price_opt <- cc_options %>%
  filter(variable == "XPRP", item %in% crop_sel) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)


fig_opt_price <- plot_growth(price_opt, "SSP2")

fig_opt_price_dif <- plot_dif(price_opt, "SSP2")

fig_opt_price_abs <- plot_abs(price_opt, "SSP2")


### TRADE
# trade_opt <- cc_options %>%
#   filter(variable == "NTMS2", item %in% crop_sel) %>%
#   ungroup() %>%
#   group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
#   mutate(dif = ((value-value[scen_type == "none"]))*100)
# 
# fig_opt_trade_dif <- plot_dif(trade_opt, "SSP2")


### LIVESTOCK
lvst_opt1 <- cc_options %>%
  filter(variable == "Anim", item %in% c("BVMEAT","SGMEAT", "PGMEAT", "PTMEAT","ALMILK")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)


fig_opt_lvst1 <- plot_growth(lvst_opt1, "SSP2")

fig_opt_lvst_dif1 <- plot_dif(lvst_opt1, "SSP2")

fig_opt_lvst_abs1 <- plot_abs(lvst_opt1, "SSP2")

lvst_opt2 <- cc_options %>%
  filter(variable == "Anim", item %in% c("PIGS","BOVD", "BOVO", "BOVF","SGTO", "PTRB","PTRH")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)

fig_opt_lvst2 <- plot_growth(lvst_opt2, "SSP2")

fig_opt_lvst_dif2 <- plot_dif(lvst_opt2, "SSP2")

fig_opt_lvst_abs2 <- plot_abs(lvst_opt2, "SSP2")


### CONSUMPTION
cons_opt <- cc_options %>%
  filter(variable == "CONS", item %in% crop_sel, unit == "1000 t") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)

fig_opt_cons <- plot_growth(cons_opt, "SSP2")

fig_opt_cons_dif <- plot_dif(cons_opt, "SSP2")

fig_opt_cons_abs <- plot_abs(cons_opt, "SSP2")


### PRODUCTION
prod_opt <- cc_options %>%
  filter(variable == "Prod", item %in% crop_sel, unit == "1000 t dm") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100)


fig_opt_prod <- plot_growth(prod_opt, "SSP2")

fig_opt_prod_dif <- plot_dif(prod_opt, "SSP2")

fig_opt_prod_abs <- plot_abs(prod_opt, "SSP2")
