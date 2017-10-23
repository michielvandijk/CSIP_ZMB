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


### SET COUNTRY
source("Scripts/Set_country.R")


### PREPARE GAMS LINK
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### LOAD FAOSTAT DATA
faostat_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/FAOSTAT_", iso3c_sel, ".csv")))

### LOAD AGRICULTURAL SYSTEM DATA FROM GLOBIOM
proj_raw <- rgdx.param(file.path(modelPath, "a6_SSPs_Water-0_full"), "YLD_SSP_STAT") %>%
  setNames(c("scenario", "region", "crop", "year", "growth")) %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(region == "ZambeziBasin") 


### PREPARE PLOT
base <- faostat_raw %>%
  filter(short_name %in% c("maiz", "grou", "cott"),
         variable == "yield",
         year == 2000) %>%
  transmute(short_name, base = value)

base_2010 <- bind_rows(
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

proj <- proj_raw %>% 
  #filter(year >2010) %>%
  filter(crop %in% c("Corn", "Gnut", "Cott")) %>%
  mutate(short_name = dplyr::recode(crop, 
                                    "Corn" = "maiz",
                                    "Gnut" = "grou",
                                    "Cott" = "cott")) %>%
  left_join(base) %>%
  mutate(value = base * growth) %>%
  filter(year <= 2050) %>%
  bind_rows(base_2010)


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


# plot 
ggplot() + 
  geom_polygon(data = simu_df, aes(x = long, y = lat, group = group, fill = factor(ag_sys)), colour = "black") +
  scale_fill_manual(values = c('#d7191c','#fdae61','#a6d96a','#1a9641'), na.value = "light grey", name = "", 
                    breaks = c("HI", "LI", "IR", "SS", NA),
                    labels = c("High input", "Low input", "Irrigated", "Subsistence", "No data")) +
  theme_map() +
  theme(legend.position = c(0.1, 0.7)) 
  #coord_map(projection="mercator", xlim = c(66, 82.45), ylim = c(24, 37.2)) + # NB Needed so that ggmap and polygons have same projection
  #theme(panel.border = element_rect(colour = "black", fill=NA, size=2))

