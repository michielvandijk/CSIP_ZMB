#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to make maps using GLOBIOM output
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
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### PREPARE GAMS LINK
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### LOAD SIMU MAP
simu <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/simu_", iso3c_sel, ".rds")))

### LOAD AGRICULTURAL SYSTEM DATA FROM GLOBIOM
# Read gdx files
sys_raw <- rgdx.param(file.path(modelPath, "crop_areas"), "cps_area") %>%
  filter(COUNTRY == "Zambia")

# Calculate share per ag_sys
sys <- sys_raw %>% 
  rename(ag_sys = .i4) %>%
  filter(ag_sys != "Total") %>%
  mutate(SimUID = as.character(SimUID)) %>%
  group_by(SimUID) %>%
  top_n(1, cps_area) %>%
  dplyr::select(SimUID, ag_sys) %>%
  ungroup() %>%
  mutate(SimUID = as.character(SimUID)) 

# Link with map data
simu_df <- simu@data %>%
  mutate(id = row.names(.),
         SimUID = as.character(SimUID))

simu_df <- fortify(simu, SimUID ="id") %>%
  left_join(simu_df) %>%
  left_join(sys)

# plot 
fig_ag_sys <- ggplot() + 
  geom_polygon(data = simu_df, aes(x = long, y = lat, group = group, fill = factor(ag_sys)), colour = "black") +
  scale_fill_manual(values = c('#d7191c','#fdae61','#a6d96a','#1a9641'), na.value = "light grey", name = "", 
                    breaks = c("HI", "LI", "IR", "SS", NA),
                    labels = c("High input", "Low input", "Irrigated", "Subsistence", "No data")) +
  theme_map() +
  theme(legend.position = c(0.1, 0.7)) 
  #coord_map(projection="mercator", xlim = c(66, 82.45), ylim = c(24, 37.2)) + # NB Needed so that ggmap and polygons have same projection
  #theme(panel.border = element_rect(colour = "black", fill=NA, size=2))

