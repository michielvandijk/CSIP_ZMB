#'========================================================================================================================================
#' Project:  CSIP_ZMB
#' Subject:  Plots for GCM precipitation and temp 
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot","labelled", "formattable")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
# Additional packages
p_load("WDI", "countrycode", "grid", "gridExtra", "ncdf4", "velox", "tictoc")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### SET DATAPATH
source(file.path(root, "Scripts/support/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD DATA
# fig_aez
aez <- readOGR(file.path(projectPath, "/Data/ZMB/Raw/Spatial_data/aez/aez.shp"))

# temperature and precipitation
tas_df <- readRDS(file.path(projectPath, "Data/ZMB/Processed/gcm/tas_df.rds"))
pr_df <- readRDS(file.path(projectPath, "Data/ZMB/Processed/gcm/pr_df.rds"))


### PLOT
# Change name of gcms
tas_df <- tas_df %>%
  mutate(gcm = dplyr::recode(gcm, "gfdl.esm2m" = "GFDL-ESM2M",
         "hadgem2.es" = "HadGEM2-ES",
         "ipsl.cm5a.lr" = "IPSL-CM5A-LR",
         "miroc.esm.chem" = "MIROC-ESM-CHEM",
         "noresm1.m" = "NorESM1-M"))

# Prepare AEZ
aez_df <- aez@data %>%
  mutate(id = rownames(.))

aez_for <- fortify(aez) %>%
  left_join(aez_df)

# tas
# Set number of colors
n_col <- length(unique(tas_df$bin))
col <- c(brewer.pal(6,"Reds"))

fig_tas = ggplot() +
  geom_raster(data = tas_df, aes(x = x, y = y, fill = bin)) +
  geom_path(data = aez_for, aes(x = long, y = lat, group = group)) +
  coord_cartesian() +
  facet_wrap(~gcm) +
  scale_fill_manual(values = brewer.pal(n = n_col, name = "Reds")) +
  labs(x="", y="", fill = "Absolute change (K)") +
  theme_void(base_size = 15) +
  theme(legend.position = "bottom", legend.title.align=0.5) +
  guides(fill = guide_legend(label.position = "bottom", title.position = "top", nrow = 1, colour = "black")) 

# plot PR
pr_df <- pr_df %>%
  mutate(gcm = dplyr::recode(gcm, "gfdl.esm2m" = "GFDL-ESM2M",
         "hadgem2.es" = "HadGEM2-ES",
         "ipsl.cm5a.lr" = "IPSL-CM5A-LR",
         "miroc.esm.chem" = "MIROC-ESM-CHEM",
         "noresm1.m" = "NorESM1-M"))
# Plot
# Set number of colors
n_col <- length(unique(pr_df$bin))
col <- c(brewer.pal(7,"RdBu"))

fig_pr = ggplot() +
  geom_raster(data = pr_df, aes(x = x, y = y, fill = bin)) +
  geom_path(data = aez_for, aes(x = long, y = lat, group = group)) +
  coord_cartesian() +
  facet_wrap(~gcm) +
  scale_fill_manual(values = col) +
  labs(x="", y="", fill = "% change") +
  theme_void(base_size = 15) +
  theme(legend.position = "bottom", legend.title.align=0.5) +
  guides(fill = guide_legend(label.position = "bottom", title.position = "top", nrow = 1, colour = "black", keywidth = 3)) 
