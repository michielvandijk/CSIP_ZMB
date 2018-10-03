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

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  ungroup() %>%
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl", "div", "def")),
         ssp = factor(ssp,levels = c("SSP1", "SSP2", "SSP3")))

# Remove additonal scenarios for now
zmb <- zmb %>%
   filter(!option %in% c("def", "ir"))


### PLOT FUNCTIONS
# Colours
col_options <- brewer.pal(n = 9, name = "Set1")
names(col_options) = c("none", "af", "ca", "rr", "msd", "dtm", "phl", "div")

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
    geom_col(aes(x = option, y = growth, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "growth (2010-2050)", title = unique(df$variable)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
  p
}

# Difference in 2050 (%) with cc
plot_dif <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp %in% ssp_sel, option != "none") %>%
    ungroup %>%
    group_by(option, item, ssp) %>%
    mutate(max_val = max(dif, na.rm = T),
           min_val = min(dif, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
    ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = ssp, y = dif, fill = option), colour = "black", position = "dodge") +
    facet_grid(item~ option, scales = "free") +
    geom_errorbar(aes(x = ssp, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "dif BAU-option in 2050 (%)") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  # geom_text(aes(x = ssp, y = dif, label = round(dif, 0)), hjust = 0, vjust = 0) +
  # geom_text(aes(x = ssp, y = min_val, label = round(min_val, 0)), hjust = 0, vjust = 1) +
  # geom_text(aes(x = ssp, y = max_val, label = round(max_val, 0)), hjust = 0, vjust = -1)
  p
}


# Difference in 2050 (%) with cc: no facet_wrap
plot_dif2 <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp %in% ssp_sel, option != "none") %>%
    ungroup %>%
    group_by(option, item, ssp) %>%
    mutate(max_val = max(dif, na.rm = T),
           min_val = min(dif, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
    ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = ssp, y = dif, fill = option), colour = "black", position = "dodge") +
    facet_wrap(~ option, scales = "free") +
    geom_errorbar(aes(x = ssp, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "dif BAU-option in 2050 (%)") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  # geom_text(aes(x = ssp, y = dif, label = round(dif, 0)), hjust = 0, vjust = 0) +
  # geom_text(aes(x = ssp, y = min_val, label = round(min_val, 0)), hjust = 0, vjust = 1) +
  # geom_text(aes(x = ssp, y = max_val, label = round(max_val, 0)), hjust = 0, vjust = -1)
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
    geom_col(aes(x = option, y = value, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
    labs(x = "", y = unique(df$unit)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~ item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
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
crop_sel <- c("Corn", "Cass", "Gnut", "Mill", "SugC", "Cott", "Soya")
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")


# projections
yld_opt <- cc_options %>%
  filter(item %in% crop_sel, variable == "YILM", unit == "fm t/ha") %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[option == "none"])/value[option == "none"]*100)

fig_opt_yld_ssp_dif <- plot_dif(yld_opt, c("SSP1", "SSP2", "SSP3"))


### PRODUCTION
# Total production
prod_opt_tot <- cc_options %>%
  filter(variable == "PROD", item %in% crop_globiom, unit == "1000 t dm") %>%
  ungroup() %>%
  group_by(year, variable, unit, gcm, crop_model, scen_type, rcp, ssp, scenario, option) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "TOT")

prod_opt <- bind_rows(
  cc_options %>%
    filter(variable == "PROD", item %in% crop_sel, unit == "1000 t dm"),
  prod_opt_tot) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_prod_ssp_dif <- plot_dif(prod_opt, c("SSP1", "SSP2", "SSP3"))


### LAND
land_opt <- cc_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "NatLnd", "GrsLnd", "Forest")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_land_ssp_dif <- plot_dif(land_opt, c("SSP1", "SSP2", "SSP3"))


### CALORIES
calo_opt <- cc_options %>%
  filter(variable == "CALO") %>%
  filter(item %in% "TOT") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_calo_ssp_dif <- plot_dif2(calo_opt, c("SSP1", "SSP2", "SSP3"))


### EMISSIONS
# Aggregated
emis_opt <- cc_options %>%
  filter(variable == "EMIS") %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "TOT")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management",
                       "ManmgtTot_CH4" = "Manure Management",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change")) %>%
  group_by(year, variable, item, unit, gcm, crop_model, rcp, ssp, option, scenario, scen_type, crop_model) %>%
  summarize(value = sum(value)) 
  
emis_opt_ag <- emis_opt %>%
  #filter(item != "Land use change") %>%
  filter(item == "Synthetic Fertilizers") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

emis_opt_lulucf <- emis_opt %>%
  filter(item == "Land use change") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

#fig_opt_emis <- plot_growth(emis_opt, "SSP2")

fig_opt_ag_emis_ssp_dif <- plot_dif2(emis_opt_ag, c("SSP1", "SSP2", "SSP3"))

fig_opt_lulucf_emis_ssp_dif <- plot_dif2(emis_opt_lulucf, "SSP2")


