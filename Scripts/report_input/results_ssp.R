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


# Historical FAO data
fao_hist_globiom_raw <- read_csv(file.path(projectPath, "/Data/ZMB/Processed/Agricultural_statistics/faostat_hist_globiom_ZMB.csv"))

#account_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "Account")

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v3.xlsx")) 

# Historical lvst statistics
lvst_hist_raw <- read_csv(file.path(projectPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_lvst_ZMB.csv")) 

# Historical calorie availability
calo_hist <- read.csv(file.path(projectPath, "Data/Historical/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c"),
         legend = "Historical (FAOSTAT)") %>%
  filter(iso3c %in% "ZMB") %>%
  dplyr::select(iso3c, year = Year, value = Value, legend) 

# Load historical data for other land use classes
land_hist_raw <- read.csv(file.path(projectPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_land_ZMB.csv"))

# conversion to dry matter
dm_conv <- read_excel(file.path(projectPath, "Data/Mappings/GLOBIOM_conversion.xlsx")) %>%
  rename(crop = ALLPRODUCT)

# LC Map
#lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")

# Visions
vision <- read_excel(file.path(projectPath, "Data/ZMB/Processed/Visions/visions.xlsx"))


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year)),
         variable = toupper(variable)) %>%
  dplyr::filter(variable %in% c("YILM", "EMIS", "LAND", "XPRP", "ANIM", "CONS", "PROD", "CALO", "AREA", "YILD",
                                "NTMS", "NTMS2", "NETT", "IMPO", "EXPO", "XPRP")) %>%
  dplyr::filter(!item %in% c("W_Elect", "W_Heat")) # Items with NAN values that give problems

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl", "div", "def"))) %>%
  dplyr::filter(scen_type == "none") #, gcm == "noCC", rcp == "noCC")

# Check for missing 2010 values
check2010 <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, gcm, rcp, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2, gcm, rcp) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
zmb <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, gcm, rcp, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2, gcm, rcp) %>%
  filter(any(year==2010)) %>%
  ungroup()
xtabs(~item + variable, data = zmb)

# Add growth and index
zmb <- zmb %>%
  group_by(variable, scenario, item, unit, ssp, scenario2, gcm, rcp) %>%
  mutate(
    index = value/value[year == 2010],
    growth = (index-1)*100) %>%
  ungroup()

# Add order of ssps
zmb <- zmb %>%
  mutate(ssp = factor(ssp, levels = c("SSP1", "SSP2", "SSP3")))

# clean up
rm(zmb_raw, check2010)


### SET AESS SSPs
col_ssp <- c("green", "blue", "red", "black")
names(col_ssp) <- c("SSP1", "SSP2", "SSP3", "Historical")
type_ssp <- c("solid", "solid", "solid", "solid")
names(type_ssp) <- c("SSP1", "SSP2", "SSP3", "Historical")


### CROP PRODUCTION
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")


# Historical
prod_hist_tot <- fao_hist_globiom_raw %>%
  filter(variable == "PROD", crop %in% crop_globiom) %>%
  left_join(dm_conv) %>%
  mutate(value = value * dm_conv) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(ssp = "Historical",
         unit = "1000 t dm") 

# Projections
prod_proj <- zmb %>% 
  filter(variable == "PROD", 
         unit == '1000 t dm', 
         item %in% crop_globiom,  
         gcm == "noCC", rcp == "noCC") %>%
  filter(year >=2000) %>%
  group_by(year, ssp) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "GLOBIOM",
         unit = "1000 t dm") 

# Errorbar
prod_eb <- zmb %>% 
  filter(variable == "PROD", 
         unit == '1000 t dm', 
         item %in% crop_globiom) %>%
  group_by(variable, year, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

# Plot
pd <- position_dodge(width = 2)

fig_ssp_crop_prod <- ggplot() +
  geom_line(data = prod_proj, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_line(data = prod_hist_tot, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_errorbar(data = filter(prod_eb, year == 2050), aes(x = year, ymin = min_val, ymax = max_val,colour = ssp), width = 2, size = 1, position = pd) +
  scale_x_continuous(limits = c(1958, 2053), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 8000))  +
  scale_colour_manual(values = col_ssp) +
  scale_linetype_manual(values = type_ssp) +
  annotate("text", x = 1980, y = 7000, label = "Historical") +
  annotate("text", x = 2030, y = 7000, label = "GLOBIOM") +
  theme_bw() +
  labs(x = "", y = "Production (1000 tons dry matter)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))


### YIELD
# Set crops
yld_crops_sel <- c("Corn", "Cass", "Gnut", "Mill")

# Historical
yld_hist <- fao_hist_globiom_raw %>%
  filter(variable == "YILD",
         crop %in% yld_crops_sel) %>%
  mutate(ssp = "Historical") %>%
  rename(item = crop)

# projections
yld_proj <- zmb %>%
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",  
         gcm == "noCC", rcp == "noCC") %>%
  ungroup() %>%
  dplyr::select(year, item, value, variable, scenario, ssp) %>%
  group_by(item, variable, ssp) %>%
  mutate(index = value/value[year == 2000])

# Errorbar
yld_eb <- zmb %>% 
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",
         year > 2010) %>%
  ungroup() %>%
  group_by(variable, item, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

# Vision
yld_fact <- vision$parameter[vision$variable == "yld"]

yld_vis <- yld_hist %>%
  filter(year %in% c(2009, 2010, 2011)) %>%
  group_by(item) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(value = yld_fact * value,
         label = "Vision",
         year = 2050,
         variable = "YILD") %>%
  filter(year == 2050)

# Plot
fig_ssp_yld <- ggplot() +
  geom_line(data = filter(yld_hist, year <= 2020), aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1) +
  geom_line(data = yld_proj, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1) +
  geom_point(data = yld_vis, aes(x = year, y = value), colour = "gold", shape = 8, size = 5) +
  geom_errorbar(data = filter(yld_eb, year == 2050), aes(x = year, ymin = min_val, ymax = max_val, colour = ssp), width = 2, size = 1, position = pd) +
  geom_text(data = yld_vis, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_x_continuous(limits = c(1960, 2055), breaks = seq(1960, 2050, 10)) +
  #scale_y_continuous(limits = c(0, 7.5))  +
  scale_colour_manual(values = col_ssp) +
  scale_linetype_manual(values = type_ssp) + 
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~item, scales = "free")

# Clean up
rm(yld_hist, yld_proj, yld_vis)




### GHG
# Historical GHG information is only available for a few items.
ghg_hist <- fao_hist_globiom_raw %>%
  filter(variable == "EMIS") %>%
  rename(item = crop) %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "Burn_Savanna_N2O",
                      "Burn_Biomass_N2O", "Burn_Biomass_CH4", "Burn_CropRes_CH4", "Burn_Savanna_CH4", "Soil_Organic_N2O",
                      "Burn_CropRes_CH4", "Burn_CropRes_N2O", "CropRes_N2O")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management",
                       "ManmgtTot_CH4" = "Manure Management",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change")) %>%
  filter(year < 2000) %>%
  group_by(year, item) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "Historical")

ghg_hist_tot <- fao_hist_globiom_raw %>%
  filter(variable == "EMIS") %>%
  rename(item = crop) %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "Burn_Savanna_N2O",
                      "Burn_Biomass_N2O", "Burn_Biomass_CH4", "Burn_CropRes_CH4", "Burn_Savanna_CH4", "Soil_Organic_N2O",
                      "Burn_CropRes_CH4", "Burn_CropRes_N2O", "CropRes_N2O")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management",
                       "ManmgtTot_CH4" = "Manure Management",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change")) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(ssp = "Historical") %>%
  filter(year <= 2012) 

# Proj
ghg_ag_proj <- zmb %>%
  filter(variable == "EMIS", 
         scen_type == "none", gcm == "noCC", rcp == "noCC",
         year >= 2000) %>%
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
  filter(item != "Land use change") %>%
  group_by(year, ssp) %>%
  summarize(value = sum(value)) %>%
  mutate(scenario = "GLOBIOM")

# Errorbar
ghg_ag_eb <- zmb %>% 
  filter(variable == "EMIS", 
         year > 2010) %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "TOT")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure management",
                       "ManmgtTot_CH4" = "Manure management",
                       "Rice_CH4" = "Rice cultivation",
                       "CropSoil_N2O" = "Synthetic fertilizers",
                       "ManaplTot_N2O" = "Manure applied to soils",
                       "ManprpTot_N2O"= "Manure left on pasture",
                       "CropRes_N2O" = "Crop residues",
                       "LUC" = "Land use change")) %>%
  filter(item != "Land use change") %>%
  group_by(year, variable, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

# LULUCF emis
ghg_lulucf_proj <- zmb %>%
  filter(variable == "EMIS", 
         scen_type == "none", gcm == "noCC", rcp == "noCC",
         year >= 2000) %>%
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
  filter(item == "Land use change") %>%
  group_by(year, ssp) %>%
  summarize(value = sum(value)) %>%
  mutate(scenario = "GLOBIOM")

# Errorbar
ghg_lulucf_eb <- zmb %>% 
  filter(variable == "EMIS", 
         year > 2010) %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "TOT")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure management",
                       "ManmgtTot_CH4" = "Manure management",
                       "Rice_CH4" = "Rice cultivation",
                       "CropSoil_N2O" = "Synthetic fertilizers",
                       "ManaplTot_N2O" = "Manure applied to soils",
                       "ManprpTot_N2O"= "Manure left on pasture",
                       "CropRes_N2O" = "Crop residues",
                       "LUC" = "Land use change")) %>%
  filter(item == "Land use change") %>%
  group_by(year, variable, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

# vision
emis_low_fact <- vision$parameter[vision$variable == "emis1"]
emis_high_fact <- vision$parameter[vision$variable == "emis2"]

ghg_ag_proj_2050 <- ghg_ag_proj %>%
  filter(year %in% c(2050)) %>%
  group_by(year, ssp) %>%
  summarize(value = sum(value))

ghg_lulucf_proj_2050 <- ghg_lulucf_proj %>%
  filter(year %in% c(2050))

ghg_ag_vis <- data.frame(year = c(2050, 2050), label = c("limited support", "substantial support"),                        
                         value = c(ghg_ag_proj_2050$value[ghg_ag_proj_2050$ssp == "SSP2"] *(1-emis_low_fact/100), 
                                   ghg_ag_proj_2050$value[ghg_ag_proj_2050$ssp == "SSP2"] *(1-emis_high_fact/100)))

ghg_lulucf_vis <- data.frame(year = c(2050, 2050), label = c("limited support", "substantial support"),                       
                             value = c(ghg_lulucf_proj_2050$value *(1-emis_low_fact/100), ghg_lulucf_proj_2050$value *(1-emis_high_fact/100)))

# Plot ghg_ag
fig_ssp_ag_emis <- ggplot() +
  geom_line(data = ghg_ag_proj, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_line(data = ghg_hist_tot, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_errorbar(data = filter(ghg_ag_eb, year == 2050), aes(x = year, ymin = min_val, ymax = max_val,colour = ssp), width = 2, size = 1, position = pd) +
  geom_point(data = ghg_ag_vis, aes(x = year-1, y = value), colour = "gold", shape = 8, size = 5) +
  geom_text(data = ghg_ag_vis, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_x_continuous(limits = c(1958, 2053), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  geom_point(data = ghg_ag_vis, aes(x = year-1, y = value), colour = "gold", shape = 8, size = 5) +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 6))  +
  scale_colour_manual(values = col_ssp) +
  scale_linetype_manual(values = type_ssp) +
  annotate("text", x = 1980, y = 5.5, label = "Historical") +
  annotate("text", x = 2030, y = 5.5, label = "BAU") +
  theme_bw() +
  labs(x = "", y = "Mt CO2eq/yr", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))


# Clean up
rm(ghg_proj)


# Plot ghg_emis
fig_ssp_lulucf_emis <- ggplot() +
  geom_line(data = ghg_lulucf_proj, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_errorbar(data = filter(ghg_lulucf_eb, year == 2050), aes(x = year, ymin = min_val, ymax = max_val,colour = ssp), width = 2, size = 1, position = pd) +
  geom_point(data = ghg_lulucf_vis, aes(x = year-1, y = value), colour = "gold", shape = 8, size = 5) +
  scale_colour_manual(values = col_ssp) +
  scale_linetype_manual(values = type_ssp) +
  theme_bw() +
  labs(x = "", y = "Mt CO2eq/yr", colour = "", linetype = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))

# Clean up
rm(ghg_proj)


### CALORIE CONSUMPTION
# calo_hist_base <- filter(calo_hist, year == 2000) %>%
#   dplyr::rename(base_2000 = value) %>%
#   ungroup() %>%
#   dplyr::select(-year)



### LAND USE
# Historical crop land per crop
crp_area_hist <- fao_hist_globiom_raw %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  filter(year <= 1999)
#filter(year %in% c(1961, 1970, 1980, 1990, 2000))

# Total historical cropland
crplnd_hist <- fao_hist_globiom_raw %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(item = "CrpLnd", ssp = "Historical")

# Vision
land_fact <- vision$number[vision$variable == "land"]/1000

land_vis <- crplnd_hist %>%
  filter(year %in% c(2000:2013)) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(label = "Vision",
         year = 2050,
         value = value + land_fact,
         variable = "AREA") %>%
  filter(year == 2050)
land_target <- data.frame(year = 2050, value = land_vis$value, label = "Vision", item = "CrpLnd")

# Errorbar
crp_area_eb <- zmb %>% 
  filter(variable == "AREA", 
         year > 2010,
         item %in% crop_globiom) %>%
  group_by(year, variable, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()


### LAND COVER
# Projection
crp_area_proj <- zmb %>%
  filter(variable == "AREA", 
         scen_type == "none", gcm == "noCC", rcp == "noCC", 
         year %in% c(2000:2050),
         item %in% crop_globiom) %>%
  group_by(year, ssp) %>%
  mutate(share = value/sum(value)*100,
         tot = sum(value = sum(value)))


# Combine CrpLnd, Grsland and forest data (only GLOBIOM CROPS)
land_hist <- bind_rows(
  fao_hist_globiom_raw %>%
    filter(variable == "AREA", crop %in% crop_globiom) %>%
    group_by(year) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(item = "CrpLnd", ssp = "Historical"),
  fao_hist_globiom_raw %>%
    filter(crop %in% c("LVS")) %>%
    mutate(ssp = "Historical", 
           item = "GrsLnd"))

# Projected data
land_proj <- bind_rows(
  zmb %>%
    filter(variable == "LAND",
           scen_type == "none", gcm == "noCC", rcp == "noCC", 
           year %in% c(2000:2050)) %>%
    filter(item %in% c("GrsLnd", "NatLnd", "Forest")),
  crp_area_proj %>%
    group_by(year, ssp) %>%
    summarize(value = sum(value)) %>%
    mutate(item = "CrpLnd")) %>%
  ungroup() %>%
  mutate(scenario = "GLOBIOM")


# Errorbar
lc_eb <- zmb %>% 
  filter(variable == "LAND",
         year > 2010) %>%
  filter(item %in% c("GrsLnd", "NatLnd", "Forest")) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp, item) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup() 

# Plot No error bar because they are very small
fig_ssp_lc <- bind_rows(land_hist, land_proj) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1) +
  geom_point(data = land_target, aes(x = year, y = value), colour = "gold", shape = 8) +
  geom_text(data = land_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -3) +
  scale_colour_manual(values = col_ssp) +
  scale_linetype_manual(values = type_ssp) +
  theme_bw() +
  scale_x_continuous(limits = c(1960, 2050), breaks = c(1960, seq(1960, 2050, 10)))  +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  facet_wrap(~item, scales = "free")

