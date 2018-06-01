#'========================================================================================================================================
#' Project:  CSIP Zambia
#' Subject:  Script to make maps of yecc
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
luid_r <- readOGR("P:/globiom/Data/simu_luid_region_maps/LUId/LUID_CTY.shp")


### PROCESS DATA
# LPJML
yecc_lpjml_zmb <- filter(yecc_lpjml_raw, Country == "Zambia", 
                         ANYRCP %in% c("rcp8p5", "noC8p5"),
                         EPICOUTPUT == "YLDG", InputSys == "SS") %>%
  gather(year, value, -ANYClimateModel:-EPICOUTPUT) %>%
  filter(year == 2050) %>%
  mutate(growth = (value - 1)*100)

# EPIC
yecc_epic_zmb <- filter(yecc_epic_raw, Country == "Zambia", 
                        ANYRCP %in% c("rcp8p5", "noC8p5"),
                        EPICOUTPUT == "YLDG", InputSys == "SS") %>%
  gather(year, value, -ANYClimateModel:-EPICOUTPUT) %>%
  filter(year == 2050) %>%
  mutate(growth = (value - 1)*100)

df<- bind_rows(yecc_epic_zmb, yecc_lpjml_zmb)

GCM1 . HadGEM2-ES
GCM2 . IPSL-CM5A-LR
GCM3 . GFDL-ESM2M
GCM4 . MIROC-ESM-CHEM
GCM5 . NorESM1-M
GCMM . MeanGCM

### GRAPHS
ggplot(df) +
  geom_boxplot(aes(x = Crop, y = growth)) +
  facet_wrap(ANYCropModel~ANYRCP)



### GRAPHS
# ZMB map
zmb_map <- fortify(luid_r[luid_r$Field2 == "Zambia",], region = "Field1_1") %>%
  rename(LUId= id)
zmb_df <- luid_r[luid_r$Field2 == "Zambia",]@data %>%
  rename(id = Field1_1)

model_df <- left_join(zmb_map, df)

model_df <- yecc_lpjml_zmb
crp <- "Corn"

yecc_p_f <- function(crp, model_df){
  df <- filter(model_df, Crop == crp, InputSys == "SS", year == 2050, ANYRCP != "rcp8p5")
  ggplot() +
    geom_polygon(data = model_df, aes(x = long, y = lat, group = group, fill = value)) +
    facet_grid(ANYClimateModel ~ ANYCropModel + ANYRCP) +
    scale_fill_gradient(low = "yellow", high = "red")
  
}


### PLOT MAPS