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
scen_col <- c("blue", "red", "green", "yellow", "grey")
names(macro_col) <- scen 


### LOAD RAW DATA
# Historical FAO data
FAO_hist <- rgdx.param(file.path(dataPath2, "Data\\FAOSTAT\\Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c")) %>%
  filter(iso3 == iso3c_sel)

# Macro scenario driver data
macro_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "MACROSCEN_DATA") %>%
  setNames(c("scenario", "region", "variable", "year", "value")) %>%
  droplevels(.) %>%
  mutate(year = as.integer(as.character(year)),
         region = as.character(region)) %>%
  dplyr::filter(variable %in% c("GDP"),
         scenario %in% scen) %>%
  #arrange(scenario, region, variable, year) %>%
  group_by(scenario, region, variable) %>%
  summarize(n = n())
  dplyr::mutate(index = value/value[year == 2000])


xtabs(~region + year, data = macro_proj_raw)
unique(macro_proj_raw$region)
levels(macro_proj_raw$region) == "Austral_ILRI"

# Climate change yield shocks
cc_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "ISIMIP_CC_IMPACT_LUId2")


# Crop data
crop_raw <- rgdx.param(file.path(modelPath, globiom_file), "CROP_DATA_COMPARE")

# Emissions
ghg_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "GHG_Compare")

# Calories
calo_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "CALORIECONS2")

# Prices
price_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "PRICE_COMPARE2")

# Land use
land_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "LAND_COMPARE2")


### GLOBAL PLOTS
### GDP AND POP PLOTS
# gdp
gdp <- TOTAL %>%
  filter(variable %in% c("GDPT"),
         year == 2050,
         model == "MAGNET",
         scenario2 == "NoCC") 

# Plots
fig_gdp = ggplot(data = gdp, aes(x = ssp, y = index, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "Index (2010=1)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,12)) +
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

fig_gdp2 = ggplot(data = gdp, aes(x = ssp, y = diff, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  #coord_cartesian(ylim = c(0,12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_gdp2

# pop
pop <- TOTAL %>%
  filter(variable %in% c("POPT"),
         year == 2050,
         model == "MAGNET",
         scenario2 == "NoCC") 

fig_pop = ggplot(data = pop, aes(x = ssp, y = index, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "Index (2010=1)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,2.5)) +
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

fig_pop2 = ggplot(data = pop, aes(x = ssp, y = diff, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(-10,150)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_pop2


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

# GDP Plot
gdp_proj <- filter(macro_proj_raw, variable == "GDP") %>%
  mutate(value = value/1000)

ggplot() +
  #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = gdp_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "billion USD", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none")


# GDPCap Plot
gdpcap_proj <- filter(macro_proj_raw, variable == "GDPpCAP") %>%
  mutate(value = value*100)

ggplot() +
  #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = gdpcap_proj, aes(x = year, y = value, colour = scenario), size = 1) +
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
pop_proj <- filter(macro_proj_raw, variable == "POP") %>%
  mutate(value = value/1000)

ggplot() +
  #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
  geom_line(data = pop_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
  scale_colour_manual(values = scen_col, name = "SSPs") +
  theme_bw() +
  labs(x = "", y = "million people", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none")



