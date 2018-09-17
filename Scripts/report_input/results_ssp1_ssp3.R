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

fig_bau_crop_prod <- ggplot() +
  geom_line(data = prod_proj, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_line(data = prod_hist_tot, aes(x = year, y = value, colour = ssp, linetype = ssp), size = 1.5) +
  geom_errorbar(data = filter(prod_eb, year == 2050), aes(x = year, ymin = min_val, ymax = max_val,colour = ssp), width = 2, size = 1, position = pd) +
  scale_x_continuous(limits = c(1958, 2053), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 8000))  +
  scale_colour_manual(values = col_ssp) +
  scale_linetype_manual(values = type_ssp) +
  annotate("text", x = 1980, y = 7000, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 7000, label = "GLOBIOM") +
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
  
