#'========================================================================================================================================
#' Project:  CSIP Zambia
#' Subject:  Script to make process yecc data
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


### GET DATA
# Read LPJML CC shifters
yecc_lpjml_raw <- read_delim(file.path("P:/globiom/Projects/ILRI-FAO-LivestockFeedCCpaper/Biophys_CC_shifters/Shifter_ISIMIP_LUIDCountry_cropland_heterogeneous_LPJmL_v4.gms"),
                            delim = ",", col_names = T, skip = 13)

# Read EPIC CC shifters
yecc_epic_raw <- read_delim(file.path("P:/globiom/Projects/ILRI-FAO-LivestockFeedCCpaper/Biophys_CC_shifters/Shifter_ISIMIP_LUIDCountry_cropland_heterogeneous_EPIC_v4.gms"),
                         delim = ",", col_names = T, skip = 13)

# Read luid map
luid_p <- readOGR("P:/globiom/Data/simu_luid_region_maps/LUId/LUID_CTY.shp")

# Read area data
area <- rgdx.param("P:/globiom/Data/simu_luid_region_maps/data_CropAreaInit/data_CropAreaInit", "CROPSAREA_INIT") 


### PROCESS DATA
# LPJML
yecc_lpjml_zmb <- filter(yecc_lpjml_raw, Country == "Zambia",
                         EPICOUTPUT == "YLDG") %>%
  gather(year, value, -ANYClimateModel:-EPICOUTPUT)

# EPIC
yecc_epic_zmb <- filter(yecc_epic_raw, Country == "Zambia",
                        EPICOUTPUT == "YLDG") %>%
  gather(year, value, -ANYClimateModel:-EPICOUTPUT)

# luid
luid_zmb_p <- luid_p[luid_p$Field2 == "Zambia",]
plot(luid_zmb_p)
saveRDS(luid_zmb_p, file.path(projectPath, "Data/ZMB/Processed/Maps/luid_p_ZMB.rds"))

# Area
luid_zmb <- unique(yecc$LUId)
area_zmb <- filter(area, ANYREGION == "Zambia",
                   AllColRow %in% luid_zmb) %>%
  setNames(c("Country","LUId",  "AltiClass", "SlpClass", "SoilClass", "Crop", "InputSys", "area")) %>%
  group_by(Country, LUId, Crop, InputSys) %>%
  summarize(area = sum(area)) %>%
  droplevels()

# Combine
yecc <- bind_rows(yecc_epic_zmb, yecc_lpjml_zmb) %>%
  mutate(ANYClimateModel = recode(ANYClimateModel, "GCM1" = "HadGEM2-ES",
                                                   "GCM2" = "IPSL-CM5A-LR",
                                                   "GCM3" = "GFDL-ESM2M",
                                                   "GCM4" = "MIROC-ESM-CHEM",
                                                   "GCM5" = "NorESM1-M")) %>%
  left_join(., area_zmb) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(area))

# save
write_csv(yecc, file.path(projectPath, "Data/ZMB/Processed/Yecc/yecc.csv"))


### AGGREGATE
yecc_ag <-  yecc %>%
  ungroup() %>%
  group_by(ANYClimateModel, ANYCropModel, ANYRCP, Country, Crop, InputSys, year) %>%
  summarize(yecc = sum(value * area, na.rm = T)/sum(area, na.rm = T)) %>%
  filter(!is.nan(yecc), ANYRCP != "noC8p5")

summary(yecc_ag)
lapply(yecc_ag[sapply(yecc_ag, class) == "character"], unique)
xtabs(~ ANYCropModel + ANYClimateModel + ANYRCP, data = yecc)

# save
write_csv(yecc_ag, file.path(projectPath, "Data/ZMB/Processed/Yecc/yecc_ag.csv"))


