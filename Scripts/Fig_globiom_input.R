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


### LOAD HISTORICAL DATA
# Historical FAO data
#yexo_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/yield/yield_hist.csv"))

# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(GLOBIOMPath, "/Data/FAOSTAT/Almost_Final_01dec2014\\Outputs_GDX_CSVs\\OUTPUT_FAO_DATA_GLOBIOM_2000.gdx"), "OUTPUT_Country", compress = T) %>%
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# ssp pop historical
pop_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/SSPs/ssp_pop_hist.csv")) 

# ssp gdp historical
gdp_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/SSPs/ssp_gdp_hist.csv")) 

# cc shocks
#cc_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "ISIMIP_CC_IMPACT_LUId2")
#cc_proj_ZMB_raw <- filter(cc_proj_raw, ANYREGION == "Zambia")
#saveRDS(cc_proj_ZMB_raw, file.path(root, "cache/cc_proj_ZMB_raw"))
cc_proj_raw <- readRDS(file.path(root, "cache/cc_proj_ZMB_raw"))


### CLIMATE CHANGE SHOCKS
# Filter out relevant variables
cc_proj <- cc_proj_raw %>%
  filter(ANYCLIMATEMODEL %in% c("GCM1", "GCM2", "GCM3", "GCM4", "GCM5"),
         ANYRCP %in% c("rcp8p5", "noC8p5"), EPICOUTPUT == "YLDG") %>%
  dplyr::rename(value = ISIMIP_CC_Impact_LUId2)

# Compute cumulative yield shock for 2000-2050
cc_proj_shock <- cc_proj %>%
  group_by(ANYCLIMATEMODEL, AllColRow, CROP, InputSys, ANYRCP) %>%
  summarize(value = prod(value)) %>%
  #ungroup() %>%
  #group_by(ANYCLIMATEMODEL, CROP, InputSys) %>%
  #summarize(value = mean(value)) %>%
  ungroup() %>%
  mutate(index = (value-1)*100) %>%
  filter(InputSys == "SS", CROP %in% c("Corn", "Gnut", "Cass", "Mill", "Rice", "Soyb", "SwPo"))


# Plot
fig_cc_shock <- ggplot(data = cc_proj_shock, aes(x = CROP, y = index, fill = CROP)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "", y = "% change (2000-2050)") +
  guides(fill = F) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ANYRCP)


### LOAD GLOBIOM DATA
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


### REGION SPECIFIC GDP AND POP PLOTS
# GDP Plot
gdp_reg_hist <- gdp_hist %>%
  filter(iso3c == "ZMB") %>%
  mutate(value = value/1000)

gdp2000 <- gdp_reg_hist %>%
  filter(year == 2000) %>%
  dplyr::select(base2000 = value)

gdp_reg_proj <- macro_proj_raw %>%
  filter(region %in% country_sel, variable == "GDP", scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  group_by(scenario) %>%
  mutate(index = value/value[year == 2000],
         value = gdp2000$base2000*index)

fig_reg_gdp <- ggplot() +
  geom_line(data = gdp_reg_hist, aes(x = year, y = value), colour = "black", size = 1) +
  geom_line(data = gdp_reg_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1980, 2050), breaks = seq(1980, 2050, 10))  +
  scale_y_continuous(limits = c(0, 350))  +
  scale_colour_manual(values = scen_col, name = "") +
  theme_bw() +
  labs(title = "GDP projections: 2000-2050",x = "", y = "billion USD", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))


# # GDPCap Plot
# gdpcap_reg_proj <- macro_proj_raw %>%
#   filter(region %in% iso3c_sel, variable == "GDPpCAP") %>%
#   mutate(value = value/1000)
# 
# ggplot() +
#   #geom_line(data = hist, aes(x = year, y = value, colour = short_name), size = 1) +
#   geom_line(data = gdpcap_reg_proj, aes(x = year, y = value, colour = scenario), size = 1) +
#   scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10), expand = c(0.0,0.0))  +
#   scale_colour_manual(values = scen_col, name = "SSPs") +
#   theme_bw() +
#   labs(x = "", y = "USD per capita (CHECK)", colour = "", linetype = "") +
#   geom_vline(xintercept = 2000, linetype = "dashed") +
#   theme(legend.position = c(.15,.8)) +
#   theme(legend.background = element_rect(colour = "black")) +
#   theme(panel.grid.minor = element_blank()) +
#   guides(linetype = "none") +
#   theme(legend.position="bottom")


# POP plot
pop_reg_hist <- pop_hist %>%
  filter(iso3c == "ZMB") 

pop_reg_proj <- macro_proj_raw %>%
  filter(region %in% "Zambia", variable == "POP", scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  mutate(value = value/1000)

fig_reg_pop <- ggplot() +
  geom_line(data = pop_reg_hist, aes(x = year, y = value), colour = "black", size = 1) +
  geom_line(data = pop_reg_proj, aes(x = year, y = value, colour = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10))  +
  scale_y_continuous(limits = c(0, 40))  +
  scale_colour_manual(values = scen_col, name = "") +
  theme_bw() +
  labs(title = "Population projections; 2000-2050", x = "", y = "million people", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  #theme(legend.position = c(.15,.8)) +
  #theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank()) +
  guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))



### YIELD PLOTS
# Selected crops
crops_sel <- c("Corn", "Gnut", "Cott")
crops_label <- c("Corn", "Groundnut", "Cotton")

# Historical
yld_reg_hist <- fao_hist_raw %>%
  filter(country == "Zambia", 
         variable == "YILD",
         crop %in% crops_sel) %>%
  mutate(scenario = "Historical") 

# projections
yld_reg_proj <- yld_proj_raw %>%
  filter(region == "SouthernAf", 
         crop %in% crops_sel) 

# base year
yld_base_2000 <- yld_reg_hist %>%
  filter(year == 2000) %>%
  rename(base2000 = value) %>%
  dplyr::select(-year, -scenario)

# Add index = 1 for 2000
yld_reg_proj_2000 <- expand.grid(scenario = unique(yld_reg_proj$scenario), crop = unique(yld_reg_proj$crop), year = 2000) %>%
  mutate(growth = 1,
         year = as.integer(as.character(year)))

# Create t/ha series
yld_reg_proj <- bind_rows(yld_reg_proj_2000, yld_reg_proj) %>%
  left_join(yld_base_2000) %>%
  mutate(value = base2000*growth) %>%
  dplyr::select(-base2000) %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3"))

# Plot
fig_reg_yld <- ggplot() +
  geom_line(data = filter(yld_reg_hist, year <= 2000), aes(x = year, y = value, colour = crop), size = 1) +
  geom_line(data = yld_reg_proj, aes(x = year, y = value, colour = crop, linetype = scenario), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10)) +
  scale_y_continuous(limits = c(0, 7.5))  +
  scale_colour_discrete(breaks = crops_sel,
                        labels= crops_label) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash")) + 
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))







