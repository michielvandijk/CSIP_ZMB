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

# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(GLOBIOMPath, "/Data/FAOSTAT/Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

#account_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "Account")

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

# Add growth
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
crop_sel <- c("Corn")

# projections
yld_proj <- zmb %>%
  filter(item %in% crop_sel, variable == "YIRF", unit == "fm t/ha") %>%
  filter(year %in% c(2010, 2050)) 

yld_proj_df <- bind_rows(
  filter(yld_proj, scen_def == "Baseline", year == 2010) %>% 
    mutate(option = "2010"), 
  filter(yld_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
  group_by(option) %>%
  mutate(min_val = min(value),
         max_val = max(value)) %>%
  filter(gcm == "nocc") %>%
  ungroup()

fig_option_yld <- ggplot(data = yld_proj_df) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "maize YIRF") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

fig_option_yld2 <- yld_proj_df %>%
  ungroup() %>%
  mutate(dif = growth-growth[option == "baseline"]) %>%
  filter(option != "baseline", year != 2010) %>%
  ggplot(data = .) +
    geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
    #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
    guides(fill=F, colour = F) +
    labs(x = "", title = "maize YIRF", y = "pp dif with BAU") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### EMISSIONS
emis_proj <- globiom %>%
  filter(variable == "EMIS") %>%
  filter(year %in% c(2010, 2050)) 

emis_proj_df <- bind_rows(
  filter(emis_proj, scen_def == "Baseline", year == 2010) %>% 
    mutate(option = "2010"), 
  filter(emis_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
  group_by(option, item) %>%
  mutate(min_val = min(value),
         max_val = max(value)) %>%
  filter(gcm == "nocc") %>%
  ungroup()

fig_option_emis <- ggplot(data = emis_proj_df) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "EMIS") +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


fig_option_emis2 <- emis_proj_df %>%
  ungroup() %>%
  group_by(item) %>%
  mutate(dif = growth-growth[option == "baseline"]) %>%
  filter(option != "baseline", year != 2010) %>%
  ggplot(data = .) +
    geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
    #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "EMIS") +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### LAND
land_proj <- globiom %>%
  filter(variable == "LAND") %>%
  filter(year %in% c(2010, 2050)) 

land_proj_df <- bind_rows(
  filter(land_proj, scen_def == "Baseline", year == 2010) %>% 
    mutate(option = "2010"), 
  filter(land_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
  group_by(option, item) %>%
  mutate(min_val = min(value),
         max_val = max(value)) %>%
  filter(gcm == "nocc") %>%
  ungroup() %>%
  filter(item %in% c("CrpLnd", "GrsLnd", "Forest"))

fig_option_land <- ggplot(data = land_proj_df) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "LAND") +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

fig_option_land2 <- land_proj_df %>%
  ungroup() %>%
  group_by(item) %>%
  mutate(dif = growth-growth[option == "baseline"]) %>%
  filter(option != "baseline", year != 2010) %>%
  ggplot(data = .) +
  geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "LAND") +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### PRICE
price_proj <- globiom %>%
  filter(variable == "XPRP") %>%
  filter(year %in% c(2010, 2050)) 

price_proj_df <- bind_rows(
  filter(price_proj, scen_def == "Baseline", year == 2010) %>% 
    mutate(option = "2010"), 
  filter(price_proj, option %in% c("baseline", "no_till_100", "no_till_50", "residues_100", "residues_50", "irrigation"), 
         SSP == "SSP2", trade == "none", rcp != "2p6", year == 2050)) %>%
  group_by(option, item) %>%
  mutate(min_val = min(value),
         max_val = max(value)) %>%
  filter(gcm == "nocc") %>%
  ungroup() %>%
  filter(item %in% c("Corn", "Cass"))

fig_option_price <- ggplot(data = price_proj_df) +
  geom_col(aes(x = option, y = value, fill = scenario, colour = scenario)) +
  geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "XPRP") +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

fig_option_price2 <- price_proj_df %>%
  ungroup() %>%
  group_by(item) %>%
  mutate(dif = growth-growth[option == "baseline"]) %>%
  filter(option != "baseline", year != 2010) %>%
  ggplot(data = .) +
  geom_col(aes(x = option, y = dif, fill = scenario, colour = scenario)) +
  #geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val)) +
  guides(fill=F, colour = F) +
  labs(x = "", title = "XPRP") +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


pdf(file = file.path(root, "Graphs/workshop_options.pdf"), paper = "a4r")
fig_option_yld
fig_option_yld2
fig_option_emis
fig_option_emis2
fig_option_land
fig_option_land2
fig_option_price
fig_option_price2
dev.off()


### BASELINE TRENDS
# Source normative scenarios
source(file.path(root, "Scripts/normative_scenario.r"))

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
  geom_col(data = prod_hist, aes(x = year, y = value, fill = crop)) +
  geom_col(data = prod_proj, aes(x = year, y = value, fill = item)) +
  scale_x_continuous(limits = c(1955, 2055), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 20000))  +
  annotate("text", x = 1980, y = 18000, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 18000, label = "GLOBIOM") +
  theme_bw() +
  labs(x = "", y = "Production (1000 tons)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))


### YIELD
# Set crops
yld_crops_sel <- c("Corn", "Cass", "Gnut", "Mill")

# Historical
yld_hist <- fao_hist_raw %>%
  filter(country == "Zambia", variable == "YILD",
         crop %in% yld_crops_sel) %>%
  mutate(legend = "Historical (FAOSTAT)") %>%
  rename(item = crop)

# projections
yld_proj <- globiom %>%
  filter(item %in% yld_crops_sel, variable %in% c("YILM"), unit == "fm t/ha",  scenario == "output_CSIP_ZMB_1") %>%
  ungroup() %>%
  dplyr::select(year, item, value, variable, scenario) %>%
  group_by(item, variable) %>%
  mutate(index = value/value[year == 2000],
         legend = "GLOBIOM")

# base year
yld_base_2000 <- yld_hist %>%
  filter(year == 2000) %>%
  rename(base2000 = value) %>%
  dplyr::select(base2000, item)

# Create t/ha series
yld_proj <- yld_proj %>%
  left_join(yld_base_2000) %>%
  mutate(value = base2000*index) %>%
  dplyr::select(-base2000)

yld_target <- filter(yld_hist, year == 2000) %>%
  mutate(value = value*2,
         label = "Normative scenario",
         year = 2050)

# Plot
ggplot() +
  geom_line(data = filter(yld_hist, year <= 2000), aes(x = year, y = value, colour = item, linetype = legend), size = 1) +
  geom_line(data = yld_proj, aes(x = year, y = value, colour = item, linetype = legend), size = 1) +
  geom_point(data = yld_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = yld_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10)) +
  #scale_y_continuous(limits = c(0, 7.5))  +
  scale_colour_discrete(breaks = crops_sel) +
  scale_linetype_manual(values = c("dashed", "solid")) + 
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~item, scales = "free") +
  guides(colour = F)


### LIVESTOCK PRODUCTION
lvst_globiom <- c("BOVO", "BOVD", "BOVF")

# Historical
lvst_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_lvst_ZMB.csv")) 

lvst_hist_2 <- lvst_raw %>%
  mutate(legend = "Historical (FAOSTAT)",
         value = value/1000) %>%
  rename(item = short_name) %>%
  filter(item == "catt",
         year %in% c(1961, 1970, 1980, 1990))

# Projections
lvst_proj <- globiom %>% 
  filter(item %in% lvst_globiom,  
         scenario == "output_CSIP_ZMB_1", year %in% c(2000:2050)) %>%
  group_by(scenario, scenario, year) %>%
  summarize(value = sum(value)*2) %>%
  mutate(item = "catt",
         legend = "GLOBIOM")

lvst_target <- data.frame(year = 2050, value = 6000, label = "Normative scenario")

col_bau <- c("black", "blue")
names(col_bau) <- c("Historical (FAOSTAT)", "GLOBIOM")

ggplot() +
  geom_col(data = lvst_hist_2, aes(x = year, y = value, fill = legend)) +
  geom_col(data = lvst_proj, aes(x = year, y = value, fill = legend)) +
  geom_point(data = lvst_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = lvst_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_fill_manual(values = col_bau) +
  scale_x_continuous(limits = c(1955, 2055), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 6500))  +
  theme_bw() +
  labs(x = "", y = "1000 Heads", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))



### GHG
ghg_proj <- globiom_raw %>% 
  mutate(year = as.integer(as.character(year))) %>%
  filter(variable == "EMIS", scenario == "output_CSIP_ZMB-1") 

fig_emis <- ggplot() +
  geom_line(data = ghg_proj, aes(x = year, y = value, colour = item)) +
  #scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  #scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "Emissions", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none") +
  facet_wrap(~item, scales = "free")


### CALORIE CONSUMPTION
# Load historical data
calo_hist <- read.csv(file.path(dataPath, "Data/Historical/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c"),
         legend = "Historical (FAOSTAT)") %>%
  filter(iso3c %in% "ZMB") %>%
  dplyr::select(iso3c, year = Year, value = Value, legend) 

# calo_hist_base <- filter(calo_hist, year == 2000) %>%
#   dplyr::rename(base_2000 = value) %>%
#   ungroup() %>%
#   dplyr::select(-year)

# Projected data
calo_proj <- globiom %>% 
  filter(variable == "CALO",
         scenario == "output_CSIP_ZMB_1",item == "TOT") %>%
  mutate(legend = "GLOBIOM")

calo_df <- bind_rows(calo_proj, calo_hist)

calo_target <- data.frame(year = 2050, value = 2400, label = "Caloric norm")
# # Rebase simulations 2000 to historical data (2000=100)
#  calo_proj <- calo_proj %>%
#    left_join(., calo_hist_base) %>%
#    mutate(value = base_2000*index)

ggplot() +
  geom_line(data = calo_df, aes(x = year, y = value, colour = legend, linetype = legend), size = 2) +
  geom_point(data = calo_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = calo_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10))  +
  scale_colour_manual(values = c("blue", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  labs(x = "", y = "kcal/cap/day", linetype = "", colour = "") +
  geom_vline(xintercept = 2000) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  #guides(linetype = "none", colour = "none") +
  theme(legend.position = "bottom")


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
price_proj <- globiom %>% 
  filter(variable == "XPRP", 
         scenario == "output_CSIP_ZMB_1", year %in% c(2000:2050), item %in% crop_globiom)

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


### CROP LAND USE
# Historical crop land per crop
crp_area_hist <- fao_hist_raw %>%
  filter(iso3c == "ZMB") %>%
  filter(variable == "AREA", crop %in% crop_globiom) %>%
  filter(year %in% c(1961, 1970, 1980, 1990))

crp_area_proj <- globiom %>%
  filter(variable == "Area", 
         scenario == "output_CSIP_ZMB_1", year %in% c(2000:2050),
         item %in% crop_globiom) %>%
  group_by(year) %>%
  mutate(share = value/sum(value)*100)

ggplot() +
  geom_col(data = crp_area_hist, aes(x = year, y = value, fill = crop)) +
  geom_col(data = crp_area_proj, aes(x = year, y = value, fill = item)) +
  scale_x_continuous(limits = c(1955, 2055), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 2100))  +
  annotate("text", x = 1980, y = 2000, label = "Historical (FAOSTAT)") +
  annotate("text", x = 2030, y = 2000, label = "GLOBIOM") +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(""))


### LAND COVER
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
  bind_rows(crplnd_hist) %>%
  filter(year %in% c(1961, 1970:2001)) %>%
  mutate(legend = "Historical (GLOBIOM)")

# Projected data
land_proj <- globiom %>%
  filter(variable == "LAND", 
         scenario == "output_CSIP_ZMB_1", year %in% c(2000:2050)) %>%
  rename(LC_TYPE = item) %>%
  left_join(lc_type_map) %>%
  group_by(lc_class, year, scenario) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  filter(!lc_class %in% c("PltFor, PriFor")) %>%
  mutate(legend = "GLOBIOM")

land_target <- data.frame(year = 2050, value = 1900, label = "Normative scenario")

ggplot() +
  geom_line(data = filter(land_hist, lc_class == "CrpLnd"), aes(x = year, y = value, colour = legend, linetype = legend), size = 1) +
  geom_line(data = filter(land_proj, lc_class == "CrpLnd"), aes(x = year, y = value, colour = legend, linetype = legend), size = 1) +
  geom_point(data = land_target, aes(x = year, y = value), colour = "yellow", shape = 8, size = 5) +
  geom_text(data = land_target, aes(x = year, y = value, label = label), hjust = 1, nudge_x = -5) +
  scale_colour_manual(values = c("blue", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  scale_x_continuous(limits = c(1955, 2059), breaks = c(1961, seq(1970, 2050, 10)), expand = c(0.0,0.0))  +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Area (1000 ha)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  
  guides(linetype = "none") +
  theme(legend.position = "bottom")
+
  facet_wrap(~lc_class, scales = "free")

pdf(file = file.path(root, "Graphs/workshop_BAU.pdf"), paper = "a4r")
fig_yld
fig_prod
fig_price
fig_lvst
fig_lc
fig_emis
fig_calo
dev.off()
    
    
  