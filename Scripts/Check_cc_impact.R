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
  dplyr::filter(variable %in% c("YILM", "EMIS", "LAND", "XPRP", "ANIM", "CONS", "PROD"))

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
# 
# # Constrain to most extreme cc scenario
# zmb <- zmb %>%
#   filter(gcm %in% c("noCC", "IPSL"), crop_model %in% c("LPJmL", "noCC"))


### YILD
# Baseline selection
# base_options <- zmb %>% 
#   ungroup() %>%
#   filter(scen_type %in% c("baseline", "CSA", "irrigation"), 
#          gcm == "noCC", ssp == "SSP2", year == 2050)

cc_options <- zmb  %>%
  ungroup() %>%
  filter(scen_type %in% c("none", "CSA")) %>%
  filter(year == 2050) 

yld_opt <- cc_options %>%
  filter(item %in% "Corn", variable == "YILM", unit == "fm t/ha", option %in% c("af","none")) %>%
  filter(ssp == "SSP2", scenario %in% c("1_CC8p5_IPSL_LPJmL","0_Ref", "9_af_csip", "9_af_csip_IPSL_LPJmL")) %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[option == "none"])/value[option == "none"]*100)

### PLOT FUNCTIONS
# Colours
col_options <- brewer.pal(n = 8, name = "Set1")
names(col_options) = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl")

# show cc scenario impac

df <- cc_options %>%
 
  group_by(option, item) %>%
    mutate(max_val = max(dif, na.rm = T),
           min_val = min(dif, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
    ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = dif, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "dif BAU-option in 2050 (%)") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(x = option, y = dif, label = round(dif, 2)), hjust = 0, vjust = 0) +
    geom_text(aes(x = option, y = min_val, label = round(min_val, 2)), hjust = 0, vjust = 1) +
    geom_text(aes(x = option, y = max_val, label = round(max_val, 2)), hjust = 0, vjust = -1) 
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


