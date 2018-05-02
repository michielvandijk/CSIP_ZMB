#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to process SSP scenario data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load()


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY CODE
iso3c <- "ZMB"

### LOAD SSP DATA
# Population
pop_ssp_raw <- read_excel(file.path(dataPath, paste0("Data/", iso3c, "/Raw/Scenarios/ssp_pop_",iso3c, "_raw.xlsx")), sheet = "ssp")
pop_hist_raw <- read_excel(file.path(dataPath, paste0("Data/", iso3c, "/Raw/Scenarios/ssp_pop_",iso3c, "_raw.xlsx")), sheet = "historic")

# gdp
gdp_ssp_raw <- read_excel(file.path(dataPath, paste0("Data/", iso3c, "/Raw/Scenarios/ssp_gdp_",iso3c, "_raw.xlsx")), sheet = "ssp")
gdp_hist_raw <- read_excel(file.path(dataPath, paste0("Data/", iso3c, "/Raw/Scenarios/ssp_gdp_",iso3c, "_raw.xlsx")), sheet = "historic")


### PROCESS POP DATA
# ssp
pop_ssp <- pop_ssp_raw %>%
  gather(Year, value, -Model, -Scenario, -Variable, -Region, -Unit, -Notes) %>%
  dplyr::select(-Notes) %>%
  mutate(Scenario2 = Scenario)

# historical
pop_hist <- pop_hist_raw %>%
  gather(Year, value, -Model, -Scenario, -Variable, -Region, -Unit) %>%
  mutate(Scenario2 = "Observed")

# Combine
pop <- bind_rows(pop_hist, pop_ssp) %>%
  mutate(id = paste(Scenario, Model, sep = "_"),
         Year = as.integer(Year)) %>%
  filter(Year %in% c(1960:2050),
         Model %in% c("IIASA-WiC POP", "History"),
         Scenario %in% c("World Bank (WDI)", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) %>%
  na.omit()


# Plot
fig_col <- c("black","orange", "green", "red", "blue", "grey")
names(fig_col) <- c("Observed", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
fig_type <- c("dashed", "solid", "solid", "solid", "Solid", "solid")
names(fig_type) <- c("Observed", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

ggplot(data = pop) +
  geom_line(aes(x = Year, y = value, colour = Scenario2, linetype = Scenario2), size = 1.5) +
  scale_colour_manual(values = fig_col) +
  #scale_linetype_manual(values = fig_type) +
  scale_x_continuous(breaks = seq(1960, 2050, 10)) +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  theme_bw() +
  labs(x = "", y = "Population (Million)", colour = "", linetype = "") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  theme(legend.position = c(.2,.7)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank())
   

### PROCESS GDP DATA
# ssp
gdp_ssp <- gdp_ssp_raw %>%
  gather(Year, value, -Model, -Scenario, -Variable, -Region, -Unit, -Notes) %>%
  dplyr::select(-Notes) %>%
  mutate(Scenario2 = Scenario)

# historical
gdp_hist <- gdp_hist_raw %>%
  gather(Year, value, -Model, -Scenario, -Variable, -Region, -Unit) %>%
  mutate(Scenario2 = "Observed")

# Combine
gdp <- bind_rows(gdp_hist, gdp_ssp) %>%
  mutate(id = paste(Scenario, Model, sep = "_"),
         Year = as.integer(Year)) %>%
  filter(Year %in% c(1980:2050),
         (Model == "OECD Env-Growth" & Year >=2010 | Model == "History"))


# Plot
ggplot(data = gdp) +
  geom_line(aes(x = Year, y = value, colour = Scenario2, linetype = Scenario2), size = 1.5) +
  scale_colour_manual(values = fig_col) +
  scale_x_continuous(breaks = seq(1960, 2050, 10)) +
  theme_bw() +
  labs(x = "", y = "PPP Billion USD 2005", colour = "", linetype = "") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  theme(legend.position = c(.2,.7)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank())


### PROCESS GDP PER CAPITA DATA
# Combine gdp and pop
gdp_cap <- bind_rows(gdp, pop) %>%
  dplyr::select(-Model, -id, -Unit) %>%
  spread(Variable, value) %>%
  na.omit %>%
  mutate(value = `GDP|PPP`/Population*1000)
  
# Plot
ggplot(data = gdp_cap) +
  geom_line(aes(x = Year, y = value, colour = Scenario2, linetype = Scenario2), size = 1.5) +
  scale_colour_manual(values = fig_col) +
  scale_x_continuous(breaks = seq(1960, 2050, 10)) +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "GDP per capita (PPP USD 2005 per capita)", colour = "", linetype = "") +
  geom_vline(xintercept = 2010, linetype = "dashed") +
  theme(legend.position = c(.2,.7)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank())

