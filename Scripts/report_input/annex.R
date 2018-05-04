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
source(file.path(root, "Scripts/support/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### PREPARE GAMS LINK
igdx(GAMSPath)


### SET FILE, SCENARIOS AND COLOURS
scen <- c("output_CSIP_ZMB-0", "output_CSIP_ZMB-1", "output_CSIP_ZMB-2")

# Select scenarios


# set colours
#scen_col <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "red")
scen_col <- c("green" ,"blue", "red", "yellow", "brown")
names(scen_col) <- c("SSP1", "SSP2","SSP3")


### LOAD MAPPINGS
# Regional mapping
reg_map <- rgdx.set(file.path(modelPath, globiom_file), "REGION_MAP") %>%
  rename(region = ANYREGION, country = ALLCOUNTRY)

# Aggregate regional mapping
reg_ag_map <- rgdx.set(file.path(modelPath, globiom_file), "REGION_AG_MAP") %>%
  rename(region = ANYREGION, ag_region = REGION_AG)


### LOAD DATA

# Zambia GLOBIOM OUTPUT Data
output_raw <- rgdx.param(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "OUTPUT") %>%
  setNames(c("scenario", "variable", "unit", "ANYREGION", "item", "ssp", "bioscen", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year)))

# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(GLOBIOMPath, "/Data/FAOSTAT/Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# ssp pop historical
pop_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/SSPs/ssp_pop_hist.csv")) 

# ssp gdp historical
gdp_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/SSPs/ssp_gdp_hist.csv")) 


### REGIONAL GDP AND POP PLOTS
macro_proj <- macro_proj_raw %>%
  filter(region %in% reg_map$region, variable %in% c("GDP", "POP")) %>%
  left_join(reg_ag_map) %>%
  group_by(scenario, ag_region, variable, year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  group_by(scenario, ag_region, variable) %>%
  mutate(index = (value/value[year == 2000]-1)*100) %>%
  filter(year == 2050, scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  na.omit

# Plots
fig_gdp = ggplot(data = filter(macro_proj, variable == "GDP"), aes(x = scenario, y = index, fill = scenario)) +
  scale_fill_manual(values = scen_col) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~ag_region, switch = "x") +
  labs(title = "GDP projections:2010-2050", y = "2010-2050 growth (%)", x = "", fill = "") +
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
  theme(axis.line.y = element_line(color="black")) +
  theme(plot.title = element_text(hjust = 0.5))

fig_gdp

# pop
fig_pop = ggplot(data = filter(macro_proj, variable == "POP"), aes(x = scenario, y = index, fill = scenario)) +
  scale_fill_manual(values = scen_col) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~ag_region, switch = "x") +
  labs(title = "Population projections: 2010-2050", y = "2010-2050 growth (%)", x = "", fill = "") +
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
  theme(axis.line.y = element_line(color="black")) +
  theme(plot.title = element_text(hjust = 0.5))

fig_pop





