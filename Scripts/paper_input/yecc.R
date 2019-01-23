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


### GET DATA
# yecc
yecc <- read_csv(file.path(projectPath, "Data/ZMB/Processed/Yecc/yecc.csv"), guess_max = 20000) # add guess_max because otherwise it makes it integer

# Read luid map
luid_p_zmb <- readRDS(file.path(projectPath, "Data/ZMB/Processed/Maps/luid_p_ZMB.rds"))


### PLOTS
# Recode crop
yecc <- yecc %>% 
  ungroup() %>%
  mutate(Crop = recode(Crop, 
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
                       "Whea" = "Wheat",
                       "BVMEAT" = "Bovine meat",
                       "PGMEAT" = "Pig meat"))



# Combine
yecc_ag <-  yecc %>%
  ungroup() %>%
  group_by(ANYClimateModel, ANYCropModel, ANYRCP, Country, Crop, InputSys, year) %>%
  summarize(yecc = sum(value * area, na.rm = T)/sum(area, na.rm = T)) %>%
  filter(!is.nan(yecc), ANYRCP != "noC8p5") 
  
summary(yecc_ag)
lapply(yecc_ag[sapply(yecc_ag, class) == "character"], unique)
xtabs(~ ANYCropModel + ANYClimateModel + ANYRCP, data = yecc)

# Plots
yecc_ag_df <- yecc_ag %>%
  filter(ANYRCP %in% c("rcp8p5")) %>%
  filter(year == 2050, InputSys == "SS") %>%
  mutate(growth = (yecc - 1)*100) 

fig_yecc_ag <- ggplot(yecc_ag_df, aes(x = Crop, y = growth, fill = Crop)) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  scale_y_continuous(breaks = seq(-150, 150, 25)) +
  labs(x = "", y = "2010-2050 yield growth (%)", fill = "") +
  theme_bw(base_size = 15) +
  theme() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = "none")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### MAPS
# ZMB map
zmb_map <- fortify(luid_p_zmb, region = "Field1_1") %>%
  rename(LUId= id)

# Function to create yecc maps
yecc_p_f <- function(crp, sys){
  crop_df <- filter(yecc, Crop %in% crp, InputSys %in% sys, year == 2050, ANYRCP == "rcp8p5") %>%
    group_by(LUId, Crop) %>%
    summarize(value = mean(value, na.rm =T))

  df <- left_join(zmb_map, crop_df) %>%
    na.omit() %>%
    mutate(growth = (value - 1)*100) %>%
    mutate(range = cut(growth, c(-Inf, -25, -5, 5, 25, Inf), labels = F),
           range = factor(range, levels = c(1:5), 
                          labels = c("yield loss > 25", "yield loss 5-25%", "yield change within 5%",
                                     "yield gain 5-25%", "yield gain >25%")))
  
  cols <- c("#CA0020", "#F4A582", "#F7F7F7", "#92C5DE", "#0571B0")
  names(cols) <- c("yield loss > 25", "yield loss 5-25%", "yield change within 5%",
  "yield gain 5-25%", "yield gain >25%")
  
  p = ggplot() +
    geom_polygon(data = luid_p_zmb, aes (x = long, y = lat, group = group), colour = "black", fill = "grey50") +
    geom_polygon(data = df, aes(x = long, y = lat, group = group, fill = range)) +
    geom_path(data = luid_p_zmb, aes (x = long, y = lat, group = group), colour = "black") +
    labs(fill = "") +
    scale_fill_manual(values = cols) +
    coord_map() +
    theme_void(base_size = 15) +
    facet_wrap(~Crop, nrow = 2) +
    theme(strip.text = element_text(face = "bold"))
  p

}

fig_yecc_crop <- yecc_p_f(c("Maize", "Cassava", "Groundnuts", "Soybeans"), "SS")

