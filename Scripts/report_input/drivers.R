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
# Select scenarios
scen <- c("output_CSIP_ZMB-0", "output_CSIP_ZMB-1", "output_CSIP_ZMB-2")

# set colours
#scen_col <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "red")
scen_col <- c("green" ,"blue", "red", "yellow", "brown")
names(scen_col) <- c("SSP1", "SSP2","SSP3")


### LOAD DATA
# Zambia GLOBIOM OUTPUT Data
zmb_raw <- rgdx.param(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "OUTPUT_ZMB") %>%
  setNames(c("scenario2", "variable", "unit", "ANYREGION", "item", "ssp", "scenario", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year)),
         item = as.character(item)) %>%
  filter(ANYREGION == "ZambiaReg") %>%
  droplevels

# Historical FAO data
fao_hist_raw <- rgdx.param(file.path(projectPath, "Data/Historical/FAOSTAT_GLOBIOM_CTY_since1961.gdx"), "OUTPUT_Country", compress = T) %>% 
  transmute(variable = factor(toupper(VAR_ID)), unit = VAR_UNIT, country = ANYREGION, crop = .i4, 
            year = as.integer(as.character(ALLYEAR)), value = OUTPUT_Country, 
            iso3c = countrycode(country, "country.name", "iso3c"))

# ssp pop historical
pop_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/SSPs/ssp_pop_hist.csv")) 

# ssp gdp historical
gdp_hist <- read_csv(file.path(GLOBIOMPath, "Data/Historical/Processed/SSPs/ssp_gdp_hist.csv")) 

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v3.xlsx")) 


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year)),
         variable = toupper(variable)) %>%
  dplyr::filter(variable %in% c("YEXO", "EMIS", "LAND", "XPRP", "ANIM", "CONS", "PROD", "CALO", "AREA", "YILD",
                                "NTMS", "NTMS2", "NETT", "IMPO", "EXPO", "XPRP")) %>%
  dplyr::filter(!item %in% c("W_Elect", "W_Heat")) # Items with NAN values that give problems

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  dplyr::filter(scen_type == "none", ssp %in% c("SSP1", "SSP2", "SSP3"), gcm == "noCC", rcp == "noCC")

# # cc shocks
# #cc_proj_raw <- rgdx.param(file.path(modelPath, globiom_file), "ISIMIP_CC_IMPACT_LUId2")
# #cc_proj_ZMB_raw <- filter(cc_proj_raw, ANYREGION == "Zambia")
# #saveRDS(cc_proj_ZMB_raw, file.path(root, "cache/cc_proj_ZMB_raw"))
# cc_proj_raw <- readRDS(file.path(root, "cache/cc_proj_ZMB_raw"))
# 
# 
# ### CLIMATE CHANGE SHOCKS
# # Filter out relevant variables
# cc_proj <- cc_proj_raw %>%
#   filter(ANYCLIMATEMODEL %in% c("GCM1", "GCM2", "GCM3", "GCM4", "GCM5"),
#          ANYRCP %in% c("rcp8p5", "noC8p5"), EPICOUTPUT == "YLDG") %>%
#   dplyr::rename(value = ISIMIP_CC_Impact_LUId2)
# 
# # Compute cumulative yield shock for 2000-2050
# cc_proj_shock <- cc_proj %>%
#   group_by(ANYCLIMATEMODEL, AllColRow, CROP, InputSys, ANYRCP) %>%
#   summarize(value = prod(value)) %>%
#   #ungroup() %>%
#   #group_by(ANYCLIMATEMODEL, CROP, InputSys) %>%
#   #summarize(value = mean(value)) %>%
#   ungroup() %>%
#   mutate(index = (value-1)*100) %>%
#   filter(InputSys == "SS", CROP %in% c("Corn", "Gnut", "Cass", "Mill", "Rice", "Soyb", "SwPo"))
# 
# 
# # Plot
# fig_cc_shock <- ggplot(data = cc_proj_shock, aes(x = CROP, y = index, fill = CROP)) +
#   geom_boxplot() +
#   stat_boxplot(geom = "errorbar") +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw() +
#   labs(x = "", y = "% change (2000-2050)") +
#   guides(fill = F) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   facet_wrap(~ANYRCP)


### REGION SPECIFIC GDP AND POP PLOTS
# GDP Plot
gdp_reg_hist <- gdp_hist %>%
  filter(iso3c == "ZMB") %>%
  mutate(value = value/1000)

gdp2000 <- gdp_reg_hist %>%
  filter(year == 2000) %>%
  dplyr::select(base2000 = value)

gdp_reg_proj <- zmb_raw %>%
  filter(variable == "GDPT", ssp %in% c("SSP1", "SSP2", "SSP3")) %>%
  group_by(scenario) %>%
  mutate(index = value/value[year == 2000],
         value = gdp2000$base2000*index)

fig_reg_gdp <- ggplot() +
  geom_line(data = gdp_reg_hist, aes(x = year, y = value), colour = "black", size = 1) +
  geom_line(data = gdp_reg_proj, aes(x = year, y = value, colour = ssp), size = 1) +
  scale_x_continuous(limits = c(1980, 2050), breaks = seq(1980, 2050, 10))  +
  scale_y_continuous(limits = c(0, 350), breaks = scales::pretty_breaks(n = 10))  +
  scale_colour_manual(values = scen_col, name = "") +
  theme_bw(base_size = 13) +
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

pop_reg_proj <- zmb_raw %>%
  filter(variable == "POPT", ssp %in% c("SSP1", "SSP2", "SSP3")) %>%
  mutate(value = value)

fig_reg_pop <- ggplot() +
  geom_line(data = pop_reg_hist, aes(x = year, y = value), colour = "black", size = 1) +
  geom_line(data = pop_reg_proj, aes(x = year, y = value, colour = ssp), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10))  +
  scale_y_continuous(limits = c(0, 40), breaks = scales::pretty_breaks(n = 10))  +
  scale_colour_manual(values = scen_col, name = "") +
  theme_bw(base_size = 13) +
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
  mutate(ssp = "Historical") %>%
  rename(item = crop) %>%
  dplyr::select(-variable) %>%
  mutate(item = recode(item, 
                                         "Barl" = "Barley",
                                         "BeaD" = "Dry beans",
                                         "Cass" = "Cassava",
                                         "ChkP" = "Chick peas",
                                         "Corn" = "Maize",
                                         "Cott" = "Cotton",
                                         "Gnut" = "Groundnuts",
                                         "Mill" = "Millet",
                                         "Pota" = "Potatoes",
                                         "Rape" = "Rapeseed",
                                         "Rice" = "Rice",
                                         "Soya" = "Soybeans",
                                         "Srgh" = "Sorghum",
                                         "SugC" = "Sugarcane",
                                         "sunf" = "Sunflowers",
                                         "SwPo" = "Sweet potatoes", 
                                         "Whea" = "Wheat"))

# projections
yld_reg_proj <- zmb %>%
  filter(variable == "YEXO",
         item %in% crops_sel,
         unit == "fm t/ha") %>%
  group_by(variable, item, ssp) %>%
  mutate(growth = value/value[year == 2000]) %>%
  ungroup() %>%
  mutate(item = recode(item, 
                       "Barl" = "Barley",
                       "BeaD" = "Dry beans",
                       "Cass" = "Cassava",
                       "ChkP" = "Chick peas",
                       "Corn" = "Maize",
                       "Cott" = "Cotton",
                       "Gnut" = "Groundnuts",
                       "Mill" = "Millet",
                       "Pota" = "Potatoes",
                       "Rape" = "Rapeseed",
                       "Rice" = "Rice",
                       "Soya" = "Soybeans",
                       "Srgh" = "Sorghum",
                       "SugC" = "Sugarcane",
                       "sunf" = "Sunflowers",
                       "SwPo" = "Sweet potatoes", 
                       "Whea" = "Wheat"))


# base year
yld_base_2000 <- yld_reg_hist %>%
  filter(year == 2000) %>%
  rename(base2000 = value) %>%
  dplyr::select(-year, -ssp, -unit)

# Create t/ha series
yld_reg_proj <- yld_reg_proj %>%
  left_join(yld_base_2000) %>%
  mutate(value = base2000*growth) %>%
  dplyr::select(-base2000) %>%
  filter(ssp %in% c("SSP1", "SSP2", "SSP3"))

# Plot
fig_reg_yld <- ggplot() +
  geom_line(data = filter(yld_reg_hist, year <= 2000), aes(x = year, y = value, colour = item), size = 1) +
  geom_line(data = yld_reg_proj, aes(x = year, y = value, colour = item, linetype = ssp), size = 1) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10)) +
  scale_y_continuous(limits = c(0, 6), breaks = scales::pretty_breaks(n = 10))  +
  scale_colour_discrete(breaks = crops_sel,
                        labels= crops_label) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash")) + 
  theme_bw(base_size = 13) +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  theme(panel.grid.minor = element_blank()) +
  #guides(linetype = "none") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))



