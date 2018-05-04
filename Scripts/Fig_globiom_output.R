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
source(file.path(root, "Scripts/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### PREPARE GAMS LINK
igdx(GAMSPath)


### SET COUNTRY
#source("Scripts/Set_country.R")
iso3c_sel <- "ZambiaReg"
country_sel <- "Zambia"


### SOURCE NORMATIVE SCENARIOS
#source(file.path(root, "Scripts/normative_scenario.r"))


### SET FILE, SCENARIOS AND COLOURS

# Select scenarios
scen <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

# set colours
scen_col <- c("blue", "red", "green", "yellow", "grey")
names(scen_col) <- scen 


### LOAD RAW DATA AND MAPPINGS
# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(dataPath, "Data/Historical/OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

account_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "Account")

# GLOBIOM
# Scenario definitions
scen_def <- read_excel(file.path(dataPath, "/GLOBIOM/results/20180417 2nd workshop/scenario_def.xlsx"))

# File
globiom_file <- "/GLOBIOM/results/20180417 2nd workshop/output_CSIP_ZMB_all_14apr18"

#globiom_raw <- rgdx.param(file.path(dataPath, globiom_file), "OUTPUT") %>%
  setNames(c("scenario", "variable", "unit", "ANYREGION", "item", "ssp", "bioscen", "enscen", "year", "value")) %>%
  filter(ANYREGION == "ZambiaReg") %>%
  droplevels()
  
#saveRDS(globiom_raw, file.path(root, paste0("Cache/globiom_raw_", Sys.Date(), ".rds")))
globiom_raw <- readRDS(file.path(root, paste0("Cache/globiom_raw_2018-04-14.rds"))) 

# LC Map
lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")


### PROCESS RAW DATA
# Add scenario definitions
globiom <- globiom_raw %>%
  mutate(scenario = gsub("-", "_", scenario),
         year = as.integer(as.character(year))) 

# Check for missing 2010 values
check2010 <- globiom %>%
  arrange(variable, scenario, item, unit, ssp, bioscen, year) %>%
  group_by(variable, scenario, item, unit, ssp, bioscen) %>%
  filter(!any(year==2010))

# # Remove series with missing values in 2010
globiom <- globiom %>%
  arrange(variable, scenario, item, unit, ssp, bioscen, year) %>%
  group_by(variable, scenario, item, unit, ssp, bioscen) %>%
  filter(any(year==2010))
xtabs(~item + variable, data = globiom)

# Add growth
globiom <- globiom %>%
  group_by(variable, scenario, item, unit, ssp, bioscen) %>%
  mutate(
    index = value/value[year == 2010],
    growth = (index-1)*100)

# Add scenario definition
globiom <- globiom %>% 
  left_join(., scen_def)

### YIELD
# Selected crops
crops_sel <- c("Corn")

# # Historical
# yld_hist <- fao_hist_raw %>%
#   filter(country == "Zambia", 
#          variable == "YILD",
#          crop %in% crops_sel) %>%
#   mutate(scenario = "Historical") 

# projections
yld_proj <- globiom %>%
  filter(item %in% crops_sel, variable == "YIRF", unit == "fm t/ha") %>%
  filter(year %in% c(2010, 2050)) 

yld_proj_df <- bind_rows(
  filter(yld_proj, scen_def == "Baseline", year == 2010) %>% 
    mutate(option = "2010"), 
  filter(yld_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50"), 
         ssp == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
  group_by(option) %>%
  mutate(min_val = min(value),
         max_val = max(value)) %>%
  filter(gcm == "nocc") %>%
  ungroup()

ggplot(data = yld_projX) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "")


### EMISSIONS
emis_proj <- globiom %>%
  filter(variable == "EMIS") %>%
  filter(year %in% c(2010, 2050)) 

emis_proj_df <- bind_rows(
  filter(emis_proj, scen_def == "Baseline", year == 2010) %>% 
    mutate(option = "2010"), 
  filter(emis_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50"), 
         ssp == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
  group_by(option, item) %>%
  mutate(min_val = min(value),
         max_val = max(value)) %>%
  filter(gcm == "nocc") %>%
  ungroup()

ggplot(data = emis_proj_df) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "") +
  facet_wrap(~item, scales = "free")

ggplot(data = emis_proj_df) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "") +
  facet_wrap(~item, scales = "free")



# # base year
# yld_base_2000 <- yld_reg_hist %>%
#   filter(year == 2000) %>%
#   rename(base2000 = value) %>%
#   dplyr::select(-year, -scenario)
# 
# # Add index = 1 for 2000
# yld_reg_proj_2000 <- expand.grid(scenario = unique(yld_reg_proj$scenario), crop = unique(yld_reg_proj$crop), year = 2000) %>%
#   mutate(growth = 1,
#          year = as.integer(as.character(year)))
# 
# # Create t/ha series
# yld_reg_proj <- bind_rows(yld_reg_proj_2000, yld_reg_proj) %>%
#   left_join(yld_base_2000) %>%
#   mutate(value = base2000*growth) %>%
#   dplyr::select(-base2000) %>%
#   filter(scenario %in% c("SSP1", "SSP2", "SSP3"))

# Plot
ggplot() +
  #geom_line(data = filter(yld_reg_hist, year <= 2000), aes(x = year, y = value, colour = crop), size = 1) +
  geom_line(data = yld_proj, aes(x = year, y = value, colour = item, linetype = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10))
  scale_y_continuous(limits = c(0, 7.5))  +
  scale_colour_discrete(breaks = crops_sel,
                        labels= crops_label) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash")) + 
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  




### Crop production
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")

# Historical
prod_hist <- fao_hist_raw %>%
  filter(country == "Zambia", variable == "PROD", crop %in% crop_globiom, year %in% c(1961, 1970, 1980, 1990)) %>%
  #group_by(year) %>%
  #summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "Historical")

# Projections
prod_proj <- globiom %>% 
  filter(variable == "Prod", unit == '1000 t', item %in% crop_globiom,  
  scenario == "output_CSIP_ZMB_1")

ggplot() +
  #geom_col(data = prod_hist, aes(x = year, y = value, fill = crop)) +
  geom_col(data = prod_proj, aes(x = year, y = value, fill = item)) +
  scale_x_continuous(limits = c(1960, 2059), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 20000))  +
  theme_bw() +
  labs(x = "", y = "Production (tons)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))


### LIVESTOCK PRODUCTION
lvst_globiom <- c("BOVO", "BOVD", "BOVF")

# Historical
lvst_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_lvst_ZMB.csv")) 

lvst_hist_2 <- lvst_raw %>%
  mutate(scenario = "Historical",
         value = value/1000*0.5) %>%
  rename(item = short_name) %>%
  filter(item == "catt",
         year %in% c(1960, 1970, 1980, 1990, 2000, 2010))

# Projections
lvst_proj <- globiom %>% 
  filter(item %in% lvst_globiom,  
         scenario == "output_CSIP_ZMB_1", year %in% c(2000:2050)) %>%
  group_by(scenario, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "catt")


ggplot() +
  geom_col(data = lvst_hist_2, aes(x = year, y = value, fill = scenario)) +
  geom_col(data = lvst_proj, aes(x = year, y = value, fill = scenario)) +
  scale_x_continuous(limits = c(1960, 2059), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 20000))  +
  theme_bw() +
  labs(x = "", y = "Production (tons)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))


### GHG
ghg_proj <- output_proj_raw %>% 
  mutate(year = as.integer(as.character(year))) %>%
  filter(variable == "EMIS", ANYREGION == "ZambiaReg",  
         option == "output_CSIP_ZMB-1")
%>%
  filter(item %in% c("TOT", "LUC"))

fig_ghg <- ggplot() +
  geom_line(data = ghg_proj, aes(x = year, y = value, colour = item)) +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "Emissions", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none")


### CALORIE CONSUMPTION
# Load historical data
calo_hist <- read.csv(file.path(dataPath, "Data/Historical/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(iso3c %in% "ZMB") %>%
  dplyr::select(iso3c, year = Year, value = Value) 

calo_hist_base <- filter(calo_hist, year == 2000) %>%
  dplyr::rename(base_2000 = value) %>%
  ungroup() %>%
  dplyr::select(-year)

xtabs(~ANYREGION + variable, data = output_proj_raw)

# Projected data
calo_proj <- output_proj_raw %>% 
  filter(variable == "CALO")
         , ANYREGION == "ZambiaReg",  
         option == "output_CSIP_ZMB-1")
  filter(variable == "CALO", ANYREGION == "ZambiaReg", item == "TOT") %>%
  mutate(year = as.integer(as.character(year)))

# Rebase simulations 2000 to historical data (2000=100)
# calo_proj <- calo_proj %>%
#   left_join(., calo_hist_base) %>%
#   mutate(value = base_2000*index)

fig_calo <- ggplot() +
  geom_line(data = calo_proj, aes(x = year, y = value)) +
  geom_line(data = calo_hist, aes(x = year, y = value), colour = "blue") +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "kcal/cap/day", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none")


### Prices
# Load historical data
price_hist_raw <- read.csv(file.path(dataPath, "Data/Historical/Prices_E_All_Data_(Normalized).csv"))

# Mean national food price [Probably still need to deflate]
price_hist <- price_hist_raw %>%
  ungroup() %>%
  rename(value = Value, year = Year) %>%
  mutate(iso3c = countrycode(Area.Code, "fao", "iso3c")) %>%
  filter(iso3c %in% "ZMB", Unit == "USD", Item %in% c("Cassava", "Maize")) %>%
  mutate(item = ifelse(Item == "Maize", "Corn", "Cass"))


# Projected data
crop_sel <- c("Corn", "Cass")
price_proj <- output_proj_raw %>% 
  mutate(year = as.integer(as.character(year))) %>%
  filter(ANYREGION == "ZambiaReg", variable == "XPRP", 
         option == "output_CSIP_ZMB-1", year %in% c(2000:2050), item %in% crop_globiom)

item %in% crop_sel, 

fig_price <- ggplot() +
  #geom_line(data = price_hist, aes(x = year, y = value), colour = "blue") +
  geom_line(data = price_proj, aes(x = year, y = value, colour = item)) +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "USD/ton", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none") +
  facet_wrap(~item, scales = "free")


### LAND USE
# Historical CrpLnd data (only GLOBIOM CROPS)
crplnd_hist <- fao_hist_raw %>%
  filter(iso3c == "ZMB") %>%
  filter(variable == "AREA", crop %in% c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott",
                                         "Gnut", "Mill", "Pota", "Rape", "Rice", "Soya",
                                         "Srgh", "SugC", "sunf", "SwPo", "Whea")) %>%
  group_by(year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(lc_class = "CrpLnd", scenario = "Historical")
  

# Load historical data for other land use classes
land_hist_raw <- read.csv(file.path(dataPath, "Data/Historical/Inputs_Land_E_All_Data_(Normalized).csv"))

land_hist <- land_hist_raw %>%
  rename(value = Value, year = Year) %>%
  mutate(iso3c = countrycode(Area.Code, "fao", "iso3c")) %>%
  filter(iso3c %in% "ZMB", Element == "Area", Item.Code %in% c(6620, 6655, 6661)) %>%
  mutate(lc_class = dplyr::recode(Item.Code, `6655` = "GrsLnd", `6661` = "For", .default = NA_character_),
         scenario = "Historical") %>%
  dplyr::select(year, lc_class, value, scenario) %>%
  na.omit %>%
  bind_rows(crplnd_hist)

# Projected data
land_proj <- output_proj_raw %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(ANYREGION  == "ZambiaReg", variable == "LAND", 
         option == "output_CSIP_ZMB-1", year %in% c(2000:2050)) %>%
  rename(LC_TYPE = item) %>%
  left_join(lc_type_map) %>%
  group_by(lc_class, year, option) %>%
  summarize(value = sum(value, na.rm = T))

fig_land <- ggplot() +
  geom_line(data = land_hist, aes(x = year, y = value, colour = lc_class), linetype = "solid") +
  geom_line(data = land_proj, aes(x = year, y = value, colour = lc_class), linetype = "dashed") +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none")

  