#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to make maps using downscaled GLOBIOM output
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
p_load("WDI", "countrycode", "gdxrrw", "ggthemes", "viridis", "ncdf4", "ncdf4.helpers")


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


### LOAD DATA
# Data per simu
zmb_simu_lc <- rgdx.param(file.path(projectPath, paste0("Data/Downscaling/downscaled_Zambia_SSP2_RCPbase.gdx")), "ds_lu_results") %>%
  droplevels %>%
  dplyr::rename(item = LC_TYPE_DS, year = Time, value = ds_lu_results) %>%
  dplyr::select(-quantiles, -ANYREGION) %>%
  mutate(SimUID = as.integer(as.character(SimUID))) 
  
# Simu map
simu_r <- raster(file.path(GLOBIOMPath, "data/simu_luid_region_maps/Simu/Simu_raster_5min/rasti_simu_gr.tif"))
names(simu_r) <- "SimUID"

# Simu info
simu_info <- read_csv(file.path(GLOBIOMPath, "data/simu_luid_region_maps/Simu/Simu_info/simu_info.csv"))

# Simu area
simu_area <- read_csv(file.path(GLOBIOMPath, "data/simu_luid_region_maps/Simu/Simu_info/simu_area.csv")) %>%
  mutate(simu_area = simu_area/1000)

# Zambia map
zmb_map <- readRDS(file.path(projectPath,"data/maps/gaul/adm_2010_ZMB.rds"))
plot(zmb_map)

# Zambia AEZ
zmb_aez <- readOGR(file.path(projectPath, "/Data/ZMB/Raw/Spatial_data/aez/aez.shp"))


### SIMU
# Raster to df
simu_df_r <- as.data.frame(rasterToPoints(simu_r)) 

# Get simu id for zmb, link data, clean data and calculate simu share
simu_zmb <- filter(simu_info, country == "Zambia") %>%
  left_join(.,zmb_simu_lc) %>%
  left_join(simu_area) %>%
  mutate(value = ifelse(value < 10^-6, NA, value),
         share = value/simu_area*100,
         share = ifelse(share >100, 100, share),
         bin = cut(share, seq(0, 100, 20), labels = seq(10, 100, 20))) # because of rounding etc, some shares are slightly larger than 100

# Combine with raster data
simu_zmb_r <- left_join(simu_zmb, simu_df_r) 

# Calculate difference between 2010 and 2050
simu_zmb_dif <- simu_zmb %>%
  dplyr::select(-simu_area, -value, -bin) %>%
  filter(year %in% c(2010, 2050)) %>%
  spread(year, share) %>%
  mutate(dif = ((`2050`/`2010`)-1)*100,
         dif2 = `2050`-`2010`)

# Combine with raster data
simu_zmb_dif_r <- left_join(simu_zmb_dif, simu_df_r) 


### PLOT
# https://stackoverflow.com/questions/21537782/how-to-set-fixed-continuous-colour-values-in-ggplot2
# Function for annual plot values
plot_val_f <- function(lc, yr, col){
  df <- filter(simu_zmb_r, SPAs == "0_ref", year == yr, item == lc)
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = share)) +
    #geom_path(data = zmb_map, aes(x = long, y = lat, group = group), colour = "black") +
    geom_path(data = zmb_aez, aes(x = long, y = lat, group = group), colour = "black") +
    scale_fill_gradientn(colours = col, breaks = c(0, 20, 40, 60, 80, 100), 
                         labels = c(0, 20, 40, 60, 80, 100), 
                         limits=c(0,100), na.value = "white") +
    labs(x="", y="", fill = "", title = yr) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  p
}

# Function for annual plot, discrete scale
plot_val2_f <- function(lc, yr, col){
  df <- filter(simu_zmb_r, SPAs == "0_ref", year == yr, item == lc, !is.na(bin))
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = bin)) +
    geom_path(data = zmb_aez, aes(x = long, y = lat, group = group), colour = "black") +
    scale_fill_manual(values = c(col),
                      labels = c(20, 40, 60, 80, 100), drop = F,
                      #limits = c(0,100), 
                      na.value = "white") +
    labs(x="", y="", fill = "", title = yr) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  p
}


# Function for difference in percentage
plot_dif_f <- function(lc, col){
  df <- filter(simu_zmb_dif_r, SPAs == "0_ref", item == lc)
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = dif2)) +
    geom_path(data = zmb_aez, aes(x = long, y = lat, group = group), colour = "black") +
    #scale_fill_gradient2(low = muted(col_l), mid = "white", high = muted(col_h), na.value = "white") +
    scale_fill_gradientn(colours = col, na.value = "white") +
    labs(x="", y="", fill = "", title = "2010-2050 difference in pp") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  p
}

# Function for difference in percentage - discrete scale
plot_dif2_f <- function(lc, col){
  df <- filter(simu_zmb_dif_r, SPAs == "0_ref", item == lc) %>%
    mutate(bin = cut(dif2, breaks = 5))
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = bin)) +
    geom_path(data = zmb_aez, aes(x = long, y = lat, group = group), colour = "black") +
    #scale_fill_gradient2(low = muted(col_l), mid = "white", high = muted(col_h), na.value = "white") +
    scale_fill_manual(values = c(col),
                      na.value = "white") +
    labs(x="", y="", fill = "", title = "2010-2050 difference in pp") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  p
}


# Set colors
orange_scale <- c(brewer.pal(5,"Oranges"))
green_scale <- c(brewer.pal(5, "Greens"))
blue_scale <- c(brewer.pal(5,"Blues"))
grey_scale <- c(brewer.pal(5,"Greys"))

# Plot
library(cowplot)
map_crplnd <- plot_grid(plot_val2_f("CrpLnd", 2010, orange_scale),
                        plot_val2_f("CrpLnd", 2050, orange_scale),
                        plot_dif_f("CrpLnd", orange_scale))

map_grslnd <- plot_grid(plot_val2_f("GrsLnd", 2010, blue_scale),
                        plot_val2_f("GrsLnd", 2050, blue_scale),
                        plot_dif_f("GrsLnd", blue_scale))

map_for <- plot_grid(plot_val2_f("PriFor", 2010, green_scale),
                        plot_val2_f("PriFor", 2050, green_scale),
                        plot_dif_f("PriFor", rev(green_scale)))

map_natlnd <- plot_grid(plot_val2_f("NatLnd", 2010, grey_scale),
                     plot_val2_f("NatLnd", 2050, grey_scale),
                     plot_dif_f("NatLnd", rev(grey_scale)))
