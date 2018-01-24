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

modeldataPath <- "P:/globiom/Projects/ISWEL/Zambezi/gdx/a6_SSPs_Water_EFR-8_full.gdx"


### SET COUNTRY
source(file.path(root, "Scripts/Set_country.R"))


### SET FILE, SCENARIOS AND COLOURS
# File
globiom_file <- "a6_SSPs_Water_EFR-8_full"

# Select scenarios
scen <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

# set colours
#scen_col <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "red")
scen_col <- c("green" ,"blue", "red", "yellow", "brown")
names(scen_col) <- scen 

iso3c_sel <- "ZambeziReg"


### LOAD MAPPINGS
# Regional mapping
reg_map <- rgdx.set(file.path(modelPath, globiom_file), "REGION_MAP") %>%
  rename(region = ANYREGION, country = ALLCOUNTRY)

# Aggregate regional mapping
reg_ag_map <- rgdx.set(file.path(modelPath, globiom_file), "REGION_AG_MAP") %>%
  rename(region = ANYREGION, ag_region = REGION_AG)

# short_name to GLOBIOM crops
crop_lvst2ALLPRODUCT <- read_excel(file.path(dataPath, "Data/mappings/GLOBIOM_mappings.xlsx"), sheet = "crop_lvst2globiom")


### LOAD DATA
# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(GLOBIOMPath, "/Data/FAOSTAT/Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# Aquastat
ir_crop_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/aquastat_ir_crops_ZMB.csv")) 
ir_raw <- read_excel(file.path(dataPath, "Data/ZMB/Raw/Agricultural_statistics/Other/AQUASTAT/20171113_irrigation_ZMB.xlsx")) %>%
  rename(variable = `Variable Name`, year = Year, value = Value)
trade_raw <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/faostat_trade_ZMB.csv")) 


### YIELD PROJECTIONS
## Scenario assumptions
yld_fact = 2

# Historical
yld_hist <- fao_hist_raw %>%
  filter(country == "Zambia", 
         variable == "YILD") %>%
  mutate(scenario = "Historical") %>%
  filter(year >= 1990)

# Double yield in 2050
yld_ns <- yld_hist %>%
  filter(year %in% c(2010, 2011, 2013)) %>%
  group_by(crop) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(value = yld_fact * value,
          scenario = "Normative scenario",
         year = 2050)

yld_df <- bind_rows(yld_hist, yld_ns, 
                    expand.grid(year = 2040, scenario = "Historical", value = 0, crop = unique(yld_hist$crop), stringsAsFactors = F)) %>%
  filter(!crop %in% c("Cereals_Crops", "Roots_Crops", "Pulses_Crops", "Oil_Crops")) 

# Plot
fig_yld_ns <- ggplot() + 
  geom_col(data = yld_df, aes(x = factor(year), y = value, fill = crop, alpha = scenario), colour = "black") +
  facet_wrap(~crop, scales = "free") +
  guides(fill = "none") +
  labs(x = "", y = "tons/ha") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks = c(1990:2013, 2050))  
rm(yld_df, yld_ns, yld_hist)


### INCREASE LAND UNDER IRRIGATION 
## Scenario assumptions
ir_fact = 3

## Total irrigation by type
# Historical
ir_type_hist <- ir_raw %>%
  filter(variable %in% c("Area equipped for full control irrigation: total",
           "Area equipped for irrigation: equipped lowland areas")) %>%
  mutate(value = value * 1000,
         variable = recode(variable, 
                           "Area equipped for full control irrigation: total"= "Full control",
                           "Area equipped for irrigation: equipped lowland areas" = "Lowland"),
         variable = factor(variable, levels = c("Lowland", "Full control")))

# Projections
ir_type_proj <- ir_type_hist %>%
  filter(year == 2002) %>%
  mutate(year = 2050,
         value = value * ir_fact)
y_ul2 <- 1.05*sum(ir_type_proj$value)

# Combine data and plot
fig_ir_type <- bind_rows(ir_type_hist, ir_type_proj, data.frame(year = 2010, value = 0, variable = "Lowland")) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = value, fill = variable), colour = "black") +
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
         value = value * ir_fact)
y_ul <- 1.05*sum(ir_crop_proj$value)

# Combine data and plot
fig_ir_crop <- bind_rows(ir_crop_hist, ir_crop_proj, data.frame(year = 2010, value = 0)) %>%
  ggplot() +
  geom_col(aes(x = factor(year), y = value, fill = short_name), colour = "black") +
  theme_bw() +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, y_ul)) +
  labs(x = "", y ="Irrigated area (ha)", fill = "Crops",
       title = "Irrigated area under full control per crop (ha)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = c(1991, 2002, 2050))

grid.arrange(fig_ir_type, fig_ir_crop, ncol=2)

### EXPORT VALUE
## Scenario assumptions
expo_fact = 3

# Historical: Limited to export value of primary agriculture
expo_hist <- trade_raw %>%
  left_join(crop_lvst2ALLPRODUCT) %>%
  na.omit %>%
  group_by(year, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "Historical") %>%
  filter(variable == "expo_v",
         year > 2000) %>%
  ungroup()

# Projections
expo_proj <- expo_hist %>%
  filter(year %in% c(2011, 2012, 2013)) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(scenario = "projection",
         year = 2050,
         value = value * expo_fact)

expo_proj <- bind_rows(expo_proj, data.frame(year = 2014, value = 0, scenario = "Historical"))

# Combine data and plot
y_ul_expo <- max(expo_proj$value/1000) * 1.05

fig_expo <- bind_rows(expo_hist, expo_proj) %>%
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
  scale_x_discrete(breaks = c(2001:2013, 2050))
  



### INCREASE CEREAL PRODUCTION
# Somewhat redundant as doubling exogenous yield is likely to result in higher production
prod_hist <- fao_hist_raw %>%
  filter(country == "Zambia", 
         variable == "PROD", 
         crop == "Cereals_Crops",
         year %in% c(2010)) %>%
  mutate(scenario = "Historical")









