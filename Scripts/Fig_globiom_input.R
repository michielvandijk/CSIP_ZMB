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
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)

dataPath2 <- "P:/globiom" 
modeldataPath <- "P:/globiom/Projects/ISWEL/Zambezi/gdx/a6_SSPs_Water_EFR-8_full.gdx"


### SET COUNTRY
source("Scripts/Set_country.R")


### SET FILE, SCENARIOS AND COLOURS
# File
globiom_file <- "a6_SSPs_Water_EFR-8_full"

# Select scenarios
scen <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

# set colours
scen_col <- c("blue", "red", "green", "yellow", "grey")
names(scen_col) <- scen 

iso3c_sel <- "ZambeziBasin"


### LOAD MAPPINGS
# Regional mapping
reg_map <- rgdx.set(file.path(modelPath, globiom_file), "REGION_MAP") %>%
  rename(region = ANYREGION, country = ALLCOUNTRY)

# Aggregate regional mapping
reg_ag_map <- rgdx.set(file.path(modelPath, globiom_file), "REGION_AG_MAP") %>%
  rename(region = ANYREGION, ag_region = REGION_AG)


### LOAD DATA
# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(dataPath2, "Data\\FAOSTAT\\Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# Macro scenario driver data
macro_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "MACROSCEN_DATA") %>%
  setNames(c("scenario", "region", "variable", "year", "value")) %>%
  mutate(year = as.integer(as.character(year))) %>%
  dplyr::filter(variable %in% c("GDP", "POP", "GDPpCAP"),
         scenario %in% scen)



# Yield projections
yld_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "YLD_SSP_STAT") %>%
  setNames(c("scenario", "region", "crop", "year", "growth")) %>%
  mutate(year = as.integer(as.character(year))) 

# Climate change yield shocks
#cc_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "ISIMIP_CC_IMPACT_LUId2")

# Crop data
#crop_raw <- rgdx.param(file.path(modelPath, globiom_file), "CROP_DATA_COMPARE")


### REGIONAL GDP AND POP PLOTS
macro_proj <- macro_proj_raw %>%
  filter(region %in% reg_map$region, variable %in% c("GDP", "POP")) %>%
  left_join(reg_ag_map) %>%
  group_by(scenario, ag_region, variable, year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  group_by(scenario, ag_region, variable) %>%
  mutate(index = (value/value[year == 2000]-1)*100) %>%
  filter(year == 2050)

# Plots
fig_gdp = ggplot(data = filter(macro_proj, variable == "GDP"), aes(x = scenario, y = index, fill = scenario)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2", "red")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~ag_region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  #coord_cartesian(ylim = c(0,16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black")) 

fig_gdp

# pop
fig_pop = ggplot(data = filter(macro_proj, variable == "POP"), aes(x = scenario, y = index, fill = scenario)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2", "red")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~ag_region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  #coord_cartesian(ylim = c(0,16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black")) 

fig_pop


### REGION SPECIFIC GDP AND POP PLOTS
# GDP Plot
gdp_reg_proj <- macro_proj_raw %>%
  filter(region %in% iso3c_sel, variable == "GDP") %>%
  mutate(value = value/1000)

ggplot() +
  #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = gdp_reg_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "billion USD", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  guides(linetype = "none")


# GDPCap Plot
gdpcap_reg_proj <- macro_proj_raw %>%
  filter(region %in% iso3c_sel, variable == "GDPpCAP") %>%
  mutate(value = value/1000)

ggplot() +
  #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = gdpcap_reg_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "USD per capita (CHECK)", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none")


# POP plot
pop_reg_proj <- macro_proj_raw %>%
  filter(region %in% iso3c_sel, variable == "POP") %>%
  mutate(value = value/1000)

ggplot() +
  #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = pop_reg_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "million people", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none")


### YIELD PLOTS
# Filter out relevant variables (crop_code > 1000 are aggregates)
FAO_prod <- FAO_raw %>%
  filter(iso3c %in% iso3c_sel)

# aggregate at zam level
prod_hist <- FAO_prod %>%
  group_by(variable, unit, year, crop) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(scenario = "Historical")


### YIELD PROJECTIONS
## Load data
# Hist
faostat_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/FAOSTAT_", iso3c_sel, ".csv")))

# Projections

## Base year data
base <- faostat_raw %>%
  filter(short_name %in% c("maiz", "grou", "cott"),
         variable == "yield",
         year == 2000) %>%
  transmute(short_name, base = value)

base_2000 <- bind_rows(
  faostat_raw %>%
  filter(short_name %in% c("maiz", "grou", "cott"),
         variable == "yield",
         year == 2000) %>%
  mutate(scenario = "SSP1"),
  faostat_raw %>%
    filter(short_name %in% c("maiz", "grou", "cott"),
           variable == "yield",
           year == 2000) %>%
    mutate(scenario = "SSP2"),
  faostat_raw %>%
    filter(short_name %in% c("maiz", "grou", "cott"),
           variable == "yield",
           year == 2000) %>%
  mutate(scenario = "SSP3"))

hist <- faostat_raw %>%
  filter(short_name %in% c("maiz", "grou", "cott"),
         variable == "yield")

yld_proj <- yld_proj_raw %>% 
  filter(crop %in% c("Corn", "Gnut", "Cott")) %>%
  mutate(short_name = dplyr::recode(crop, 
                                    "Corn" = "maiz",
                                    "Gnut" = "grou",
                                    "Cott" = "cott")) %>%
  left_join(base) %>%
  mutate(value = base * growth) %>%
  filter(year <= 2050) %>%
  bind_rows(base_2000)


# Plot
ggplot() +
  geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = proj, aes(x = year, y = value, colour = short_name, linetype = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10)) +
  scale_colour_discrete(breaks=c("cott", "grou", "maiz"),
                        labels=c("Cotton", "Groundnuts", "Maize")) +
  scale_linetype_manual(values = c("dashed", "dotted", "dotdash")) + 
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none")



### MACRO PROJECTIONS



