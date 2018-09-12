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
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl"))) %>%
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

# clean up
rm(zmb_raw, check2010)


### CROP PRODUCTION
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")


# Historical
prod_hist <- fao_hist_globiom_raw %>%
  filter(variable == "PROD", crop %in% crop_globiom, year %in% c(1961, 1970, 1980, 1990, 2000)) %>%
  #group_by(year) %>%
  #summarize(value = sum(value, na.rm = T)) %>%
  left_join(dm_conv) %>%
  mutate(scenario = "Historical",
         value = value * dm_conv,
         unit = "1000 t dm") 

# Projections
prod_proj <- zmb %>% 
  filter(variable == "PROD", 
         unit == '1000 t dm', 
         item %in% crop_globiom,  
         gcm == "noCC", rcp == "noCC") %>%
  filter(year >2000) # Slight mismatch between 2000 hist and prod values so we select hist

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
         

fig_bau_crop_prod <- ggplot() +
  geom_col(data = prod_hist, aes(x = year, y = value, fill = crop), colour = "black") +
  geom_col(data = prod_proj, aes(x = year, y = value, fill = item), colour = "black") +
  geom_errorbar(data = prod_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 3) +
  scale_x_continuous(limits = c(1955, 2055), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 10000))  +
  annotate("text", x = 1980, y = 9000, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 9000, label = "GLOBIOM") +
  theme_bw() +
  labs(x = "", y = "Production (1000 tons dry matter)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))

# clean up
rm(prod_hist, prod_proj, dm_conv)


### IRRIGATION
# Projections
# ir_proj <- zmb %>% 
#   filter(variable == "ASYS2")


### YIELD
# Set crops
yld_crops_sel <- c("Corn", "Cass", "Gnut", "Mill")

# Historical
yld_hist <- fao_hist_globiom_raw %>%
  filter(variable == "YILD",
         crop %in% yld_crops_sel) %>%
  mutate(legend = "Historical (FAOSTAT)") %>%
  rename(item = crop)

# projections
yld_proj <- zmb %>%
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",  
         gcm == "noCC", rcp == "noCC") %>%
  ungroup() %>%
  dplyr::select(year, item, value, variable, scenario) %>%
  group_by(item, variable) %>%
  mutate(index = value/value[year == 2000],
         legend = "GLOBIOM")

# # base year
# yld_base_2000 <- yld_hist %>%
#   filter(year %in% c(1998:2001)) %>%
#   group_by(item) %>%
#   summarize(base2000 = mean(value, na.rm = T)) %>%
#   dplyr::select(base2000, item)
# 
# # Create t/ha series
# yld_proj <- yld_proj %>%
#   left_join(yld_base_2000) %>%
#   mutate(value = base2000*index) %>%
#   dplyr::select(-base2000)


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
fig_bau_yld <- ggplot() +
  geom_line(data = filter(yld_hist, year <= 2020), aes(x = year, y = value, colour = item, linetype = legend), size = 1) +
  geom_line(data = yld_proj, aes(x = year, y = value, colour = item, linetype = legend), size = 1) +
  geom_point(data = yld_vis, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_errorbar(data = yld_eb, aes(x = year, ymin = min_val, ymax = max_val, colour = item), width = 3) +
  geom_text(data = yld_vis, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_x_continuous(limits = c(1960, 2055), breaks = seq(1960, 2050, 10)) +
  #scale_y_continuous(limits = c(0, 7.5))  +
  scale_colour_discrete(breaks = yld_crops_sel) +
  scale_linetype_manual(values = c("dashed", "solid")) + 
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~item, scales = "free") +
  guides(colour = F)

# Clean up
rm(yld_base_2000, yld_hist, yld_proj, yld_vis)


### LIVESTOCK PRODUCTION
lvst_globiom <- c("BOVO", "BOVD", "BOVF")

lvst_hist <- lvst_hist_raw %>%
  mutate(legend = "Historical (FAOSTAT)",
         value = value/1000) %>%
  rename(item = lvst) %>%
  filter(item == "catt",
         year %in% c(1961, 1970, 1980, 1990))

# Projections
lvst_proj <- zmb %>% 
  filter(item %in% lvst_globiom,  
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
         year %in% c(2000:2050)) %>%
  group_by(scenario, scenario, year) %>%
  summarize(value = sum(value)*2) %>%
  mutate(item = "catt",
         legend = "GLOBIOM")

# Errorbar (no cc impact as shocks are same for cc scenarios)
# lvst_eb <- zmb %>% 
#   filter(item %in% lvst_globiom,
#          year > 2010) %>%
#   group_by(variable, year, scenario, unit, ssp, rcp, gcm) %>%
#   summarize(value = sum(value)) %>%
#   ungroup() %>%
#   group_by(variable, year, unit, ssp) %>%
#   summarize(max_val = max(value, na.rm = T),
#             min_val = min(value, na.rm = T)) %>%
#   ungroup()

# vision
catt_fact <- vision$number[vision$variable == "catt"]
smru_fact <- vision$parameter[vision$variable == "smru"]

lvst_vis <- bind_rows(
  lvst_hist_raw %>%
    filter(year %in% c(2012:2014), lvst == "smru") %>%
    group_by(lvst) %>%
    summarize(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(label = "Vision",
           year = 2050,
           value = value * smru_fact),
  data.frame(value = catt_fact, lvst = "catt", year = 2050, label = "Vision")) %>%
  mutate(value = value/1000) %>%
  filter(year == 2050, lvst == "catt")

lvst_target <- data.frame(year = 2050, value = 6000, label = "Normative scenario")

col_bau <- c("black", "blue")
names(col_bau) <- c("Historical (FAOSTAT)", "GLOBIOM")

fig_bau_lvst <- ggplot() +
  geom_col(data = lvst_hist, aes(x = year, y = value, fill = legend)) +
  geom_col(data = lvst_proj, aes(x = year, y = value, fill = legend)) +
  geom_point(data = lvst_vis, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = lvst_vis, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_fill_manual(values = col_bau) +
  scale_x_continuous(limits = c(1955, 2055), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 6500))  +
  theme_bw() +
  labs(x = "", y = "1000 Heads", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))

# Clean up
rm(lvst_hist_raw, lvst_hist, lvst_proj, lvst_target, lvst_vis)


### BVMEAT
meat_globiom <- c("BVMEAT")


# Projections
meat_proj <- zmb %>% 
  filter(item %in% meat_globiom, variable == "PROD",
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
         year %in% c(2000:2050)) %>%
  mutate(legend = "GLOBIOM")

#col_bau <- c("blue")
#names(col_bau) <- c("GLOBIOM")

fig_bau_meat <- ggplot() +
  geom_col(data = meat_proj, aes(x = year, y = value, fill = legend), colour = "black") +
  scale_fill_manual(values = col_bau) +
  scale_x_continuous(limits = c(2005, 2055), breaks = c(2000, seq(2000, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 150))  +
  theme_bw() +
  labs(x = "", y = "1000 t", colour = "", linetype = "") +
  #geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))

# Clean up



### GHG
# Historical GHG information is only available for a few items.
# ghg_hist <- fao_hist_globiom_raw %>%
#   filter(variable == "EMIS") %>%
#   rename(item = crop) %>%
#   filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "Burn_Savanna_N2O",
#                       "Burn_Biomass_N2O", "Burn_CropRes_CH4", "Burn_Savanna_CH4", "Soil_Organic_N2O",
#                       "Burn_CropRes_CH4", "Burn_CropRes_N2O", "CropRes_N2O")) %>%
#   mutate(item = ifelse(item %in% c("ManmgtTot_N2O", "ManaplTot_N2O", "ManprpTot_N2O", "ManmgtTot_CH4"), "Manure", item)) %>%
#   group_by(year, item) %>%
#   summarize(value = sum(value)) %>%
#   filter(year <= 2000) %>%
#   #group_by(year, unit) %>%
#   #summarize(value = sum(value, na.rm = T)) %>%
#   mutate(scenario = "Historical")

# Proj
ghg_proj <- zmb %>%
  filter(variable == "EMIS", 
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
         year > 2000) %>%
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
  group_by(year, item) %>%
  summarize(value = sum(value)) %>%
  mutate(scenario = "GLOBIOM")

# Errorbar
ghg_eb <- zmb %>% 
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
  group_by(year, variable, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, scenario, unit, ssp, rcp, gcm) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year, unit, ssp) %>%
  summarize(max_val = max(value, na.rm = T),
            min_val = min(value, na.rm = T)) %>%
  ungroup()

# Checking outlier - to remove
# filter(scenario %in% c("1_CC8p5_IPSL_EPIC", "0_Ref")) %>%
#   dplyr::select(item, scenario, value, year) %>%
#   spread(scenario, value)

# Plot
fig_bau_emis <- ghg_proj %>%
  ggplot() +
  #geom_col(data = prod_hist, aes(x = year, y = value, fill = crop)) +
  geom_col(data = ghg_proj, aes(x = year, y = value, fill = item), colour = "black") +
  geom_errorbar(data = ghg_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 3) +
  scale_x_continuous(expand = c(0.0,0.0), breaks = seq(2010, 2050, 10), limits = c(2005, 2055))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 13))  +
  #annotate("text", x = 1980, y = 9000, label = "Historical (FAOSTAT)") +
  #annotate("text", x = 2030, y = 25, label = "GLOBIOM") +
  theme_bw() +
  labs(x = "", y = "Mt CO2eq/yr", colour = "", linetype = "") +
  #geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  #theme(panel.grid.minor = element_blank()) +
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

# Projected data
calo_proj <- zmb %>% 
  filter(variable == "CALO",
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
         item == "TOT") %>%
  mutate(legend = "GLOBIOM")

calo_df <- bind_rows(calo_proj, calo_hist) %>%
  filter(year >= 1985)

# Target
calo_fact <- vision$number[vision$variable == "calo"]
calo_target <- data.frame(year = 2050, value = calo_fact, label = "Caloric norm")

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
  geom_errorbar(data = calo_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 3) +
  geom_point(data = calo_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = calo_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_x_continuous(limits = c(1985, 2055), breaks = seq(1960, 2050, 10))  +
  scale_colour_manual(values = c("blue", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  labs(x = "", y = "kcal/cap/day", linetype = "", colour = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  #guides(linetype = "none", colour = "none") +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Clean up
rm(calo_df, calo_hist, calo_proj, calo_target)

# 
# ### Prices
# # Load historical data
# price_hist_raw <- read.csv(file.path(projectPath, "Data/Historical/Prices_E_All_Data_(Normalized).csv"))
# 
# # Mean national food price [Probably still need to deflate]
# price_hist <- price_hist_raw %>%
#   ungroup() %>%
#   rename(value = Value, year = Year) %>%
#   mutate(iso3c = countrycode(Area.Code, "fao", "iso3c")) %>%
#   filter(iso3c %in% "ZMB", Unit == "USD", Item %in% c("Cassava", "Maize")) %>%
#   mutate(item = ifelse(Item == "Maize", "Corn", "Cass"))
# 
# 
# Projected data
# crop_price_sel <- c("Corn", "Cass")
# price_proj <- zmb %>%
#   filter(variable == "XPRP",
#          ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC",
#          year %in% c(2000:2050), item %in% crop_globiom)
# 
# fig_bau_price <- ggplot() +
#   #geom_line(data = price_hist, aes(x = year, y = value), colour = "blue") +
#   geom_line(data = price_proj, aes(x = year, y = value, colour = item)) +
#   #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
#   #scale_colour_manual(values = scen_col, name = "SSPs") +
#   theme_bw() +
#   labs(x = "", y = "USD/ton", colour = "", linetype = "") +
#   geom_vline(xintercept = 2000, linetype = "dashed") +
#   theme(panel.grid.minor = element_blank()) +
#   #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#   guides(linetype = "none") +
#   facet_wrap(~item, scales = "free")
# 
# # Clean up
# rm(price_hist_raw, price_hist, price_proj)


### CROP LAND USE
# Historical crop land per crop
crp_area_hist <- fao_hist_globiom_raw %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  filter(year %in% c(1961, 1970, 1980, 1990, 2000))

# Projection
crp_area_proj <- zmb %>%
  filter(variable == "AREA", 
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC", 
         year %in% c(2010:2050),
         item %in% crop_globiom) %>%
  group_by(year) %>%
  mutate(share = value/sum(value)*100)

# Vision
land_fact <- vision$number[vision$variable == "land"]/1000

land_vis <- crplnd_hist %>%
  filter(year %in% c(2000: 2013)) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(label = "Vision",
         year = 2050,
         value = value + land_fact,
         variable = "AREA") %>%
  filter(year == 2050)
land_target <- data.frame(year = 2050, value = land_vis$value, label = "Vision")

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
  geom_col(data = crp_area_hist, aes(x = year, y = value, fill = crop), colour = "black") +
  geom_col(data = crp_area_proj, aes(x = year, y = value, fill = item), colour = "black") +
  geom_errorbar(data = crp_area_eb, aes(x = year, ymin = min_val, ymax = max_val), width = 3) +
  geom_point(data = land_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = land_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -3) +
  scale_x_continuous(limits = c(1955, 2055), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 3000))  +
  annotate("text", x = 1980, y = 2500, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 2500, label = "GLOBIOM") +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))

# Clean up
rm(crp_area_hist, crp_area_proj)

 
### LAND COVER
# Cropland historical
crplnd_hist<- fao_hist_globiom_raw %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(lc_class = "CrpLnd", scenario = "Historical")

# Combine CrpLnd, Grsland and forest data (only GLOBIOM CROPS)
land_hist <- bind_rows(
  crplnd_hist,
  fao_hist_globiom_raw %>%
    filter(variable == "AREA", crop %in% crop_globiom) %>%
    group_by(year) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(lc_class = "Cropland", scenario = "Historical"),
  fao_hist_globiom_raw %>%
    filter(crop %in% c("LVS")) %>%
    mutate(scenario = "Historical", 
         lc_class = "GrsLnd"))

# Projected data
land_proj <- bind_rows(
  zmb %>%
    filter(variable == "LAND",
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC", 
         year %in% c(2000:2050)) %>%
    filter(item %in% c("GrsLnd", "NatLnd", "Forest")) %>%
    rename(lc_class = item),
  crp_area_proj %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    mutate(lc_class = "CrpLnd")) %>%
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
  ungroup() %>%
  rename(lc_class = item)

# Plot No error bar because they are very small
fig_bau_lc <- bind_rows(land_hist, land_proj) %>%
  ggplot() +
  geom_line(aes(x = year, y = value, colour = lc_class, linetype = scenario), size = 1) +
  #geom_errorbar(data = lc_eb, aes(x = year, ymin = min_val, ymax = max_val, colour = lc_class), width = 3) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  scale_x_continuous(limits = c(1960, 2050), breaks = c(1960, seq(1960, 2050, 10)))  +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom")

fig_bau_crplnd <- bind_rows(land_hist, land_proj) %>%
  filter(lc_class == "CrpLnd") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, linetype = scenario), colour = "#CD9600", size = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_point(data = land_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = land_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  theme_bw() +
  scale_x_continuous(limits = c(1960, 2050), breaks = c(1960, seq(1960, 2050, 10)))  +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  guides(colour = "none", linetype = "none") 


### EXPO
#expo vision
expo_fact <- vision$parameter[vision$variable == "expo"]

# Projected exports
trade_proj <- zmb %>%
  filter(variable %in% c("NETT"), 
         ssp == "SSP2", scen_type == "none", gcm == "noCC", rcp == "noCC", unit == "1000 t",
         year %in% c(2010, 2050)) %>%
  filter(item %in% c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott",
                      "Gnut", "Mill", "Pota", "Rape", "Rice", "Soya",
                      "Srgh", "SugC", "sunf", "SwPo", "Whea")) 

expo_vis <- trade_proj %>%
  filter(year == 2010, variable == "NETT") %>%
  mutate(expo_vis = ifelse(value >= 0, value * expo_fact, NA),
         year = 2050)

# Plot
fig_bau_trade <- ggplot(data = trade_proj) +
  geom_col(aes(x = factor(year), y = value, fill = factor(year)), position = "dodge", colour = "black") +
  facet_wrap(~item, scales = "free") +
  geom_point(data = expo_vis, aes(x = factor(year), y = expo_vis), colour = "yellow", shape = 8, size = 5) +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = expand_scale(mult = c(.1, .1))) +
  theme_bw() +
  labs(x = "", y = "Nett trade (1000 t)", colour = "", fill = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  guides(fill = F) 
  
