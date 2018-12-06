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


# set colours
scen_col <- c("green" ,"blue", "red", "yellow", "brown")
names(scen_col) <- c("SSP1", "SSP2","SSP3")


### LOAD MAPPINGS
# Aggregate regional mapping
reg_ag_map <- rgdx.set(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "REGION_AG_MAP") %>%
  setNames(c("scenarios", "ANYREGION", "region")) %>%
  dplyr::select(-scenarios) %>%
  unique %>%
  mutate(ag_region = recode(ANYREGION, "World"= "World",
                         "MidEastNorthAfr" = "Middle East and North Africa",
                         "LatinAmericaCarib" = "Latin Americ",
                         "NorthAmerica" = "North America",
                         "SouthAsia" = "South Asia",
                         "WesternEurope" = "Europe",
                         "Africa" = "Sub-Saharan Africa",
                         "PacificDev" = "Oceania",
                         "EurCentAsia" = "Central Asia",
                         "EastAsiaPac" = "East Asia"))
                    
# GDP and POP projections
macro_proj_raw <- read_excel(file.path(projectPath, "GLOBIOM/results/SSP_GDP_POP.xlsx"))

### REGIONAL GDP AND POP PLOTS
macro_proj <- macro_proj_raw %>%
  filter(item %in% c("GDP", "Population")) %>%
  left_join(reg_ag_map) %>%
  group_by(SSP, ag_region, item, year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  group_by(SSP, ag_region, item) %>%
  mutate(index = (value/value[year == 2000]-1)*100) %>%
  filter(year == 2050, SSP %in% c("SSP1", "SSP2", "SSP3")) %>%
  na.omit %>%
  rename(variable = item, scenario = SSP)

# Plots
fig_gdp = ggplot(data = filter(macro_proj, variable == "GDP"), aes(x = scenario, y = index, fill = scenario)) +
  scale_fill_manual(values = scen_col) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~ag_region, switch = "x") +
  labs(title = "GDP projections:2010-2050", y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0), labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  #coord_cartesian(ylim = c(0,16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(axis.line.y = element_line(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text = element_text(angle = 90))



# pop
fig_pop = ggplot(data = filter(macro_proj, variable == "Population"), aes(x = scenario, y = index, fill = scenario)) +
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
  theme(strip.background = element_blank()) +
  theme(axis.line.y = element_line(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text = element_text(angle = 90))






