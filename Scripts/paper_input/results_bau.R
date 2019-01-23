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

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v3.xlsx")) 

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
  dplyr::filter(ssp == "SSP2", scen_type == "none") #, gcm == "noCC", rcp == "noCC")

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

# Remove carbon price column which is causing problems in some of the coding
zmb <- dplyr::select(zmb, -carbon_price)

# clean up
rm(zmb_raw, check2010)


### SET AES SSPs
col_bau <- c("black", "blue")
names(col_bau) <- c("Historical", "BAU")
type_bau <- c("solid", "solid")
names(type_bau) <- c("BAU", "Historical")


### CROP PRODUCTION
# Add labels to GLOBIOM crops
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")

# Historical
prod_hist <- fao_hist_globiom_raw %>%
  filter(variable == "PROD", crop %in% crop_globiom, year %in% c(1961:1999)) %>%
  left_join(dm_conv) %>%
  mutate(scenario = "Historical",
         value = value * dm_conv,
         unit = "1000 t dm",
         crop = recode(crop, 
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
                       "Whea" = "Wheat")) 

prod_hist_tot <- fao_hist_globiom_raw %>%
  filter(variable == "PROD", crop %in% crop_globiom) %>%
  left_join(dm_conv) %>%
  mutate(value = value * dm_conv) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "Total production (FAOSTAT)",
         unit = "1000 t dm") 

# Projections
prod_proj <- zmb %>% 
  filter(variable == "PROD", 
         unit == '1000 t dm', 
         item %in% crop_globiom,  
         gcm == "noCC", rcp == "noCC") %>%
  filter(year >=2000) %>%
  group_by(year) %>%
  mutate(share = value/sum(value)*100,
         tot = sum(value = sum(value)),
         item = recode(item, 
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
                       "Whea" = "Wheat"))

# Errorbar
prod_eb <- zmb %>% 
  filter(variable == "PROD", 
         unit == '1000 t dm', 
         item %in% crop_globiom,
         year > 2010) %>%
  group_by(variable, year, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
         min_val = min(value, na.rm = T)) %>%
           ungroup()
         
# Plot
fig_bau_crop_prod <- ggplot() +
  geom_area(data = prod_hist, aes(x = year, y = value, fill = crop), colour = "black") +
  geom_area(data = prod_proj, aes(x = year, y = value, fill = item), colour = "black") +
  geom_line(data = prod_hist_tot, aes(x = year, y = value, linetype = scenario), colour = "black", size = 1.5) +
  scale_linetype_manual(values = "solid") +
  geom_errorbar(data = prod_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 2, size = 1) +
  scale_x_continuous(limits = c(1958, 2053), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 9000), breaks = scales::pretty_breaks(n = 10))  +
  annotate("text", x = 1980, y = 7000, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 7000, label = "GLOBIOM") +
  theme_bw(base_size = 14) +
  labs(x = "", y = "Production (1000 tons dry matter)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  guides(fill = guide_legend(""))

# clean up
rm(prod_hist, prod_proj, dm_conv)


### YIELD
# Set crops
yld_crops_sel <- c("Corn", "Cass", "Gnut", "Mill")

# Historical
yld_hist <- fao_hist_globiom_raw %>%
  filter(variable == "YILD",
         crop %in% yld_crops_sel) %>%
  mutate(ssp = "Historical") %>%
  rename(item = crop) %>%
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
                "Whea" = "Wheat"))

# projections
yld_proj <- zmb %>%
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",  
         gcm == "noCC", rcp == "noCC") %>%
  ungroup() %>%
  dplyr::select(year, item, value, variable, scenario, ssp) %>%
  group_by(item, variable) %>%
  mutate(index = value/value[year == 2000],
         ssp = "BAU") %>%
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
                       "Whea" = "Wheat"))

# Errorbar
yld_eb <- zmb %>% 
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",
         year > 2010) %>%
  ungroup() %>%
  group_by(variable, item, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(ssp = "BAU") %>%
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
                       "Whea" = "Wheat"))

# Plot
fig_bau_yld <- ggplot() +
  geom_line(data = filter(yld_hist, year <= 2020), aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1) +
  geom_line(data = yld_proj, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1) +
  geom_errorbar(data = yld_eb, aes(x = year, ymin = min_val, ymax = max_val, colour = ssp), width = 2, size = 1) +
  scale_x_continuous(limits = c(1960, 2055), breaks = seq(1960, 2050, 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))  +
  scale_colour_manual(values = c(col_bau)) +
  scale_linetype_manual(values = type_bau) + 
  theme_bw(base_size = 13) +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~item, scales = "free")

# Clean up
rm(yld_base_2000, yld_hist, yld_proj)


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
  mutate(scenario = "Historical") %>%
  filter(year <= 2012) 

# Proj
ghg_proj <- zmb %>%
  filter(variable == "EMIS", year >= 2000) %>%
  filter(!item %in% c("LUCF", "Net", "Soil_N2O", "TOT")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management",
                       "ManmgtTot_CH4" = "Manure Management",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change",
                       "LUCP" = "Deforestation",
                       "LUCC" = "Other land use change",
                       "LUCG" = "Other land use change")) %>%
  group_by(variable, year, item, ssp, scen_type, gcm, rcp, scenario, unit, crop_model) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(scenario = "GLOBIOM")
 

# Ag emissions
ghg_ag_proj <- ghg_proj %>%
  filter(item %in% c("Enteric fermentation",
                     "Manure Management",
                     "Rice Cultivation",
                     "Synthetic Fertilizers",
                     "Manure applied to Soils",
                     "Manure left on Pasture",
                     "Crop Residues")) %>%
  filter(ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC") %>%
  group_by(year) %>%
  mutate(share = value/sum(value)*100,
         tot = sum(value = sum(value)))
  

# Errorbar
ghg_ag_eb <- ghg_proj %>% 
  filter(item %in% c("Enteric fermentation", 
                     "Manure Management",
                     "Rice Cultivation",
                     "Synthetic Fertilizers",
                     "Manure applied to Soils",
                     "Manure left on Pasture",
                     "Crop Residues")) %>%
  filter(year > 2010) %>%
  group_by(variable, year, ssp, scen_type, gcm, rcp, crop_model, scenario, unit) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

ghg_ag_proj_2050 <- ghg_ag_proj %>%
    filter(year %in% c(2050)) %>%
    group_by(year) %>%
    summarize(value = sum(value))

# Plot ghg_ag
fig_bau_ag_emis <- ggplot() +
  geom_area(data = ghg_hist, aes(x = year, y = value, fill = item), colour = "black") +
  geom_area(data = ghg_ag_proj, aes(x = year, y = value, fill = item), colour = "black") +
  geom_line(data = ghg_hist_tot, aes(x = year, y = value), colour = "black",size = 1.5) +
  geom_errorbar(data = ghg_ag_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 2, size = 1) +
  scale_x_continuous(limits = c(1958, 2053), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 8), breaks = scales::pretty_breaks(n = 10))  +
  annotate("text", x = 1980, y = 6.5, label = "Historical") +
  annotate("text", x = 2030, y = 6.5, label = "BAU") +
  theme_bw(base_size = 14) +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme_bw(base_size = 14) +
  labs(x = "", y = "Mt CO2eq/yr", colour = "", linetype = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))

# Clean up
rm(ghg_proj)


### CALORIE CONSUMPTION
# Projected data
calo_proj <- zmb %>% 
  filter(variable == "CALO",
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
         item == "TOT") %>%
  mutate(legend = "GLOBIOM")

calo_df <- bind_rows(calo_proj, calo_hist) %>%
  filter(year >= 1985)

# Errorbar
calo_eb <- zmb %>% 
  filter(variable == "CALO", item == "TOT", year > 2010) %>%
  ungroup() %>%
  group_by(variable, item, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

# Plot
fig_bau_cal <- ggplot() +
  geom_line(data = calo_df, aes(x = year, y = value, colour = legend, linetype = legend), size = 2) +
  geom_errorbar(data = calo_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 2, size = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_colour_manual(values = c("blue", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw(base_size = 14) +
  labs(x = "", y = "kcal/cap/day", linetype = "", colour = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


### CROP LAND USE
# Historical crop land per crop
crp_area_hist <- fao_hist_globiom_raw %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  filter(year <= 1999) %>%
  mutate(crop = recode(crop, 
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
                       "Whea" = "Wheat"))

# Total historical cropland
crplnd_hist <- fao_hist_globiom_raw %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(item = "CrpLnd", scenario = "Total cropland (FAOSTAT)")

# Projection
crp_area_proj <- zmb %>%
  filter(variable == "AREA", 
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC", 
         year %in% c(2000:2050),
         item %in% crop_globiom) %>%
  group_by(year) %>%
  mutate(share = value/sum(value)*100,
         tot = sum(value = sum(value))) %>%
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
                       "Whea" = "Wheat"))

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

# Plot
fig_bau_luc <- ggplot() +
  geom_area(data = crp_area_hist, aes(x = year, y = value, fill = crop), colour = "black") +
  geom_area(data = crp_area_proj, aes(x = year, y = value, fill = item), colour = "black") +
  geom_line(data = crplnd_hist, aes(x = year, y = value, linetype = scenario), colour = "black", size = 1.5) +
  scale_linetype_manual(values = "solid") +
  geom_errorbar(data = crp_area_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 2, size = 1) +
  scale_x_continuous(limits = c(1958, 2052), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 3000), breaks = scales::pretty_breaks(n = 10))  +
  annotate("text", x = 1980, y = 2500, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 2500, label = "GLOBIOM") +
  theme_bw(base_size = 14) +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom", legend.box = "vertical") +
  guides(fill = guide_legend(""))

### LAND COVER
# Combine CrpLnd, Grsland and forest data (only GLOBIOM CROPS)
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

# Projected data
land_proj <- bind_rows(
  zmb %>%
    filter(variable == "LAND",
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC", 
         year %in% c(2000:2050)) %>%
    filter(item %in% c("GrsLnd", "NatLnd", "Forest")),
  crp_area_proj %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    mutate(item = "CrpLnd")) %>%
  ungroup() %>%
  mutate(scenario = "GLOBIOM") %>%
  group_by(year) %>%
  mutate(share = value/sum(value)*100,
         tot = sum(value = sum(value))) %>%
  mutate(item = recode(item, 
                       "CrpLnd" = "Cropland",
                       "GrsLnd" = "Grassland",
                       "NatLnd" = "Natural land"))

# Correct Grassland so that it links with historical information. Take from natural land
hist_2000 <- land_hist$value[land_hist$item == "Grassland" & land_hist$year == 2000]
proj_2000 <- land_proj$value[land_proj$item == "Grassland" & land_proj$year == 2000]
corr <- hist_2000-proj_2000

land_proj$value[land_proj$item == "Grassland"] <- land_proj$value[land_proj$item == "Grassland"] + corr
land_proj$value[land_proj$item == "Natural land"] <- land_proj$value[land_proj$item == "Natural land"] - corr

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
fig_bau_lc <- bind_rows(land_hist, land_proj) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, colour = item, linetype = scenario), size = 1) +
  #geom_errorbar(data = lc_eb, aes(x = year, ymin = min_val, ymax = max_val, colour = lc_class), width = 3) +
  #scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw(base_size = 14) +
  scale_x_continuous(limits = c(1960, 2050), breaks = c(1960, seq(1960, 2050, 10)))  +
  scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
  theme_bw(base_size = 14) +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom", legend.box = "vertical")

