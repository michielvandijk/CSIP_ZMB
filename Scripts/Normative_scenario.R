#'========================================================================================================================================
#' Project:  CSIP
#' Subject:  Script to develop normative scenario projections
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "gdxrrw", "ggthemes", "viridis", "gridExtra", "imputeTS")


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

modeldataPath <- "P:/globiom/Projects/ISWEL/Zambezi/gdx/a6_SSPs_Water_EFR-8_full.gdx"


### SET COUNTRY
source(file.path(root, "Scripts/Set_country.R"))


### SET FILE, SCENARIOS AND COLOURS
# Select scenarios
scen <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

# set colours
#scen_col <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "red")
scen_col <- c("green" ,"blue", "red", "yellow", "brown")
names(scen_col) <- scen 

iso3c_sel <- "ZambiaReg"


### LOAD MAPPINGS
# short_name to GLOBIOM crops
crop_lvst2ALLPRODUCT <- read_excel(file.path(dataPath, "Data/mappings/GLOBIOM_mappings.xlsx"), sheet = "crop_lvst2globiom")


### LOAD DATA
# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(dataPath, "Data/Historical/OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# Faostat & aquastat
ir_crop_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/aquastat_ir_crops_ZMB.csv")) 
ir_raw <- read_excel(file.path(dataPath, "Data/ZMB/Raw/Agricultural_statistics/Other/AQUASTAT/20171113_irrigation_ZMB.xlsx")) %>%
  rename(variable = `Variable Name`, year = Year, value = Value)
trade_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_trade_ZMB.csv")) 
land_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_land_ZMB.csv")) 
lvst_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_lvst_ZMB.csv")) 

# Emissions
unfcc_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/unfcc_emis_ZMB.csv")) 
ciat_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/ciat_emis_ZMB.csv")) 

# National emissions
nat_emis_hist <- read_excel(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/nat_emis_ZMB.xlsx"), sheet = "historical") 
indc_raw <- read_excel(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/nat_emis_ZMB.xlsx"), sheet = "indc") 
nc2_raw <- read_excel(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/nat_emis_ZMB.xlsx"), sheet = "nc2") 


### YIELD PROJECTIONS
## Scenario assumptions
yld_fact = 2

# Historical
yld_hist <- fao_hist_raw %>%
  filter(country == "Zambia", 
         variable == "YILD") %>%
  mutate(scenario = "Historical") %>%
  filter(year >= 2000)

# Double yield in 2050
yld_ns <- yld_hist %>%
  filter(year %in% c(2010, 2011, 2013)) %>%
  group_by(crop) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(value = yld_fact * value,
          scenario = "Normative scenario",
         year = 2050,
         variable = "YILD")

yld_df <- bind_rows(yld_hist, yld_ns, 
                    expand.grid(year = 2040, scenario = "Historical", value = 0, crop = unique(yld_hist$crop), stringsAsFactors = F)) %>%
  filter(!crop %in% c("Cereals_Crops", "Roots_Crops", "Pulses_Crops", "Oil_Crops")) 

# Plot
fig_yld_ns <- ggplot() + 
  geom_point(data = filter(yld_df, year <= 2013 | year == 2050), aes(x = factor(year), y = value, colour = crop, alpha = scenario, shape = scenario)) +
  geom_line(data = filter(yld_df, year <= 2013), aes(x = factor(year), y = value, colour = crop, alpha = scenario, group = scenario)) +
  #geom_col(data = yld_df, aes(x = factor(year), y = value, colour = crop, alpha = scenario, shape = scenario), colour = "black") +
  facet_wrap(~crop, scales = "free") +
  #geom_smooth(data = filter(yld_df, year <= 2010), aes(x = year, y = value)) +
  labs(x = "", y = "tons/ha", shape = "", colour = "", fill = "", alpha = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks = c(2000, 2010, 2050))  +
  scale_alpha_discrete(range = c(0.5, 1)) +
  scale_shape_manual(values = c(20,8)) +
  guides(fill = "none", colour = "none") +
  theme(legend.position = "bottom")
rm(yld_df)


### INCREASE LAND UNDER IRRIGATION 
## Total irrigation by type
# Historical
ir_type_hist <- ir_raw %>%
  filter(variable %in% c("Area equipped for full control irrigation: total",
           "Area equipped for irrigation: equipped lowland areas")) %>%
  mutate(value = value * 1000,
         type = dplyr::recode(variable, 
                           "Area equipped for full control irrigation: total"= "Full control",
                           "Area equipped for irrigation: equipped lowland areas" = "Lowland"),
         variable = factor(variable, levels = c("Lowland", "Full control")))

## Scenario assumptions for 2050
ir_fact = 3

# Projections
ir_type_proj <- ir_type_hist %>%
  filter(year == 2002) %>%
  mutate(year = 2050,
         value = value * ir_fact,
         variable = "IR_all")
y_ul2 <- 1.05*sum(ir_type_proj$value)

# Combine data and plot
fig_ir_type <- bind_rows(ir_type_hist, ir_type_proj, data.frame(year = 2010, value = 0, type = "Lowland")) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = value, fill = type), colour = "black") +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, y_ul2)) +
  #scale_fill_manual(labels = c("Full control", "Lowland")) +
  labs(x = "", y ="Irrigated area (ha)", fill = "Irrigation type",
       title = "Irrigated area by type (ha)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = c(1992, 2002, 2050))


## Total controlled irrigation per crop
# Historical
ir_crop_hist <- ir_crop_raw %>%
  filter(short_name != "total")

# Projections
ir_crop_proj <- ir_crop_hist %>%
  filter(year == 2002) %>%
  mutate(year = 2050,
         value = value * ir_fact,
         variable = "IR_con")
y_ul <- 1.05*sum(ir_crop_proj$value)

# Combine data and plot
fig_ir_crop <- bind_rows(ir_crop_hist, ir_crop_proj, data.frame(year = 2010, value = 0, short_name = "bana")) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = value, fill = short_name), colour = "black", width = 0.8) +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, y_ul)) +
  labs(x = "", y ="Irrigated area (ha)", fill = "Crops",
       title = "Irrigated area under full control per crop (ha)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = c(1991, 2002, 2050))

grid.arrange(fig_ir_type, fig_ir_crop, ncol=2)

# Clean up
rm(ir_crop_raw, ir_raw)


### EXPORT VALUE
## Scenario assumptions
expo_fact = 2

# Historical: Limited to export value of primary agriculture
expo_hist <- trade_raw %>%
  left_join(crop_lvst2ALLPRODUCT) %>%
  na.omit %>%
  group_by(year, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "Historical") %>%
  filter(variable == "expo_v",
         year >= 2000) %>%
  ungroup()

# Projections
expo_proj <- expo_hist %>%
  filter(year %in% c(2011, 2012, 2013)) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(scenario = "projection",
         year = 2050,
         value = value * expo_fact,
         variable = "EXPO")

expo_df <- bind_rows(expo_proj, data.frame(year = 2014, value = 0, scenario = "Historical"))

# Combine data and plot
y_ul_expo <- max(expo_proj$value/1000) * 1.05

fig_expo_ns <- bind_rows(expo_hist, expo_df) %>%
  mutate(value = value/1000) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = value, fill = scenario), colour = "black") +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, y_ul_expo)) +
  labs(x = "", y ="Export value (million)", fill = "Scenario",
       title = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none") +
  scale_x_discrete(breaks = c(2000:2013, 2050))

# Clean up
rm(expo_df, trade_raw)


### LAND USE
## Scenario assumptions
land_fact = 1

# Historical
land_hist <- land_raw %>%
  na.omit %>%
  filter(item %in% c("permanent crops", "arable land", "permanent meadows and pastures")) %>%
  mutate(scenario = "Historical") %>%
  filter(year >= 2000) 

# Projections
land_proj <- land_hist %>%
  filter(year %in% c(2012, 2013, 2014)) %>%
  group_by(unit, item) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(scenario = "projection",
         year = 2050,
         value = value * land_fact,
         variable = "AREA")

land_df <- bind_rows(land_proj, data.frame(year = 2015, value = 0, scenario = "Historical", item = "arable land")) %>%
  mutate(year = ifelse(year == 2050, 2016, year)) # Fix using 2050 creates a large gap between hist and proj series

# Combine data and plot
y_ul_land <- sum(land_proj$value) * 1.05

fig_land_ns <- ggplot() +
  geom_area(data = land_hist, aes(x = year, y = value, fill = item), colour = "black", position = "stack") +
  geom_col(data = land_df, aes(x = year, y = value, fill = item), colour = "black") +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, y_ul_land)) +
  labs(x = "", y ="agricultural area (1000 ha)", fill = "",
       title = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2016), label = c(2000, 2005, 2010, 2050), expand = c(0,0))

# Clean up
rm(land_df, land_raw)


### LIVESTOCK TARGETS
# Historical
lvst_hist <- lvst_raw %>%
  na.omit %>%
  filter(short_name %in% c("catt", "smru")) %>%
  mutate(scenario = "Historical") %>%
  filter(year >= 2000) 

## Scenario assumptions
catt_fact = 1
smru_fact = lvst_hist$value[lvst_hist$short_name == "smru" & lvst_hist$year == 2014] * 2

# Projections
lvst_proj <- data.frame(value = c(6000000 * catt_fact, smru_fact), short_name = c("catt", "smru"), year = 2050, scenario = "projections", variable = "LVST")

lvst_df <- bind_rows(lvst_proj, data.frame(year = c(2015,2015), value = c(0,0), scenario = c("Historical", "Historical"), short_name = c("catt", "smru")))

# Combine data and plot
y_ul_lvst <- max(lvst_proj$value/1000) * 1.05

fig_lvst_ns <- bind_rows(lvst_hist, lvst_df) %>%
  mutate(value = value/1000) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = value, fill = scenario), colour = "black") +
  theme_bw() +
  facet_wrap(~ short_name) +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, y_ul_lvst)) +
  labs(x = "", y ="1000 heads", fill = "Scenario",
       title = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none") +
  scale_x_discrete(breaks = c(2000:2014, 2050))

# Clean up
rm(lvst_df, lvst_raw)


### INCREASE CROP DIVERSITY
# Area share per crop over last three years

# Historical
div_hist <- fao_hist_raw %>%
  na.omit %>%
  filter(variable == "AREA", iso3c == "ZMB",
         year >= 2000,
         !crop %in% c("Cereals_Crops", "Roots_Crops", "Pulses_Crops", "Oil_Crops", "ALL", "FRS","LVS", "CRP")) %>% 
           mutate(scenario = "Historical",
                  variable = "AREA_SH") %>%
  group_by(year) %>%
  mutate(share = 100 * value/sum(value, na.rm = T)) %>%
  ungroup()

## Scenario assumptions
# Corn area constant, rest doubles
# Projections  
div_proj <- div_hist %>%
  filter(year %in% c(2011, 2012, 2013)) %>%
  group_by(crop) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_target = ifelse(crop == "Corn", value, value * 2),
         share = 100*area_target/sum(area_target, na.rm = T), 
         scenario = "Normative scenario",
         year = 2050,
         variable = "AREA_SH")

div_df <- bind_rows(div_hist, div_proj, 
                    expand.grid(year = 2040, scenario = "Historical", value = 0, crop = unique(div_hist$crop), stringsAsFactors = F)) 

# Plot
fig_div_ns <- ggplot() + 
  geom_point(data = filter(div_df, year <= 2013 | year == 2050), aes(x = factor(year), y = share, colour = crop, alpha = scenario, shape = scenario)) +
  geom_line(data = filter(div_df, year <= 2013), aes(x = factor(year), y = share, colour = crop, alpha = scenario, group = scenario)) +
  #geom_col(data = yld_df, aes(x = factor(year), y = value, colour = crop, alpha = scenario, shape = scenario), colour = "black") +
  facet_wrap(~crop, scales = "free") +
  #geom_smooth(data = filter(yld_df, year <= 2010), aes(x = year, y = value)) +
  labs(x = "", y = "area share (%)", shape = "", colour = "", fill = "", alpha = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks = c(2000, 2010, 2050))  +
  scale_alpha_discrete(range = c(0.5, 1)) +
  scale_shape_manual(values = c(20,8)) +
  guides(fill = "none", colour = "none") +
  theme(legend.position = "bottom")



### EMISSIONS TARGETS
# Function to impute values
impute_f <- function(df){
  title = unique(paste(df$variable, df$scenario, sep = "_"))
  print(title)
  df <- arrange(df, year)
  #Impute
  #imp <- na.kalman(df$value)
  imp <- na.interpolation(df$value) # use simple interpolation
  plotNA.imputations(df$value, imp, main = title)
  # Combine
  df$value <- imp
  return(df)
}

# Historical: we use nat_emis_proj as 1994 values have been revised
base <- expand.grid(year = c(1994:2000), variable = c("agriculture", "LULUCF"), 
                    scenario = "historical", unit = "MtCO2e", source = "NC2", stringsAsFactors = F)

nat_emis_hist <- left_join(base, nat_emis_hist) %>%
  group_by(variable) %>%
  do(impute_f(.))
rm(base)

ciat <- ciat_raw %>%
  filter(variable %in% c("agriculture (mtco2e)", "land-use change and forestry (mtco2)")) %>%
  mutate(scenario = "historical",
         variable = dplyr::recode(variable, "agriculture (mtco2e)" = "agriculture",
                           "land-use change and forestry (mtco2)" = "LULUCF"))

# Combine national Projections with unfcc and impute
# base
base <- expand.grid(year = c(2000:2030), variable = c("agriculture", "LULUCF"), scenario = c("BAU","mitigation", "substantial support"), stringsAsFactors = F)

# Combine
nat_emis_proj <- bind_rows(
  filter(nat_emis_hist, year == 2000) %>%
    mutate(scenario = "BAU"),
  filter(nat_emis_hist, year == 2000) %>%
    mutate(scenario = "mitigation"),
  filter(nat_emis_hist, year == 2000) %>%
    mutate(scenario = "substantial support"),
  filter(indc_raw, scenario == "substantial support"),
  nc2_raw) %>%
  dplyr::select(year, variable, scenario, value) %>%
  left_join(base,.)

# Impute NA values
nat_emis_proj <- nat_emis_proj %>%
  group_by(variable, scenario) %>%
  do(impute_f(.))

# Combine data and plot
nat_emis <- bind_rows(nat_emis_hist,
                      filter(nat_emis_proj))

fig_nat_emis_ns <- ggplot() +
  geom_line(data = nat_emis, aes(x = year, y = value, colour = scenario)) +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = c(0,0)) +
  labs(x = "", y ="GHG emissions (MtCO2e)",
       title = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  facet_wrap(~variable)


# Chain link CIAT/FAOSTAT series
nat_emis_proj_index <- nat_emis_proj %>%
  group_by(scenario, variable) %>%
  mutate(index = value/value[year == 2014]) %>%
  dplyr::select(-value) %>%
  filter(year >= 2014)

# ciat_2010
ciat_2010 <- filter(ciat, year == 2014) %>%
  dplyr::select(value, variable)

# Splice
nat_emis_proj_ciat <- left_join(nat_emis_proj_index, ciat_2010) %>%
  mutate(value = value * index) %>%
  dplyr::select(-index)

# Combine
nat_emis_ciat <- bind_rows(ciat, nat_emis_proj_ciat) %>%
  mutate(class = "Official reports")

# 2030 Values
nat_emis_ciat_2030 <- filter(nat_emis_ciat, year == 2030) %>%
  dplyr::select(value, variable, scenario, year) %>%
  mutate(class = "Extrapolation")

# Project 2050 values: Assume same increase as between 2014 and 2030
nat_emis_ciat_2050 <- nat_emis_proj_ciat %>%
  group_by(scenario, variable) %>%
  mutate(index = value/value[year == 2014]) %>%
  dplyr::select(-value) %>%
  filter(year == 2030) %>%
  left_join(
    nat_emis_ciat_2030 %>%
      dplyr::select(-year, -class)) %>%
  mutate(value = value * index, 
         year = 2050,
         class = "Extrapolation") %>%
  dplyr::select(-index)

# Combine 
nat_emis_ciat_df <- bind_rows(nat_emis_ciat, nat_emis_ciat_2030, nat_emis_ciat_2050) %>%
  mutate(scenario = factor(scenario, levels = c("historical", "mitigation", "substantial support", "BAU")))
  

lt_emis <- c("dotted", "solid")
names(lt_emis) <- c("Extrapolation", "Official reports")
col_emis <- c("red", "black", "blue", "green")
names(col_emis) <- c("BAU", "historical", "mitigation", "substantial support")

fig_nat_emis_ciat_ns <- ggplot() +
  geom_line(data = nat_emis_ciat_df, aes(x = year, y = value, colour = scenario, linetype = class), size = 1.5) +
  theme_bw() +
  #scale_y_continuous(labels = comma, expand = c(0,0)) +
  labs(x = "", y ="GHG emissions (MtCO2e)",
       title = "", linetype ="", colour = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical") +
  scale_linetype_manual(values = lt_emis) +
  scale_colour_manual(values = col_emis) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050))

# clean up
#rm(base, ciat, ciat_2010, ciat_raw, indc_raw, nc2_raw, unfcc_raw, nat_emis_proj_index, nat_emis_ciat)







