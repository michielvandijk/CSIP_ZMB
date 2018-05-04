#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to prepare analyse conservation agriculture in Zambia
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "haven")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode")



### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### SET COUNTRY AND YEAR
source(file.path(root, "code/ZMB/2010/set_country_year.r"))

### LOAD DATA
# hh_id
source(file.path(root, "code/ZMB/2010/household_surveys/id.r"))

# location
source(file.path(root, "code/ZMB/2010/household_surveys/location.r"))

# Conservation agriculture data
source(file.path(root, "code/ZMB/2010/household_surveys/field.r"))

#adm
adm <- readRDS(file.path(proc_path, paste0("maps/gaul/adm_", iso3c_sel, ".rds")))

# aez
aez <- readOGR(file.path(raw_path, "maps/aez/AEZ.shp"))


### COMBINE DATA
# Combine data
df <- left_join(id, ca1_raw) %>%
  filter(!is.na(field), !is.na(crop_hs)) %>%
  left_join(., ca2_raw) %>%
  filter(land_use %in% c("Own cultivated field", "Rented in cultivated field (paid in cash or in-kind)", "Borrowed in cultivated field (without payment)"))


### IDENTIFY CA FARMERS
# We follow the approach of Zulu-Mbata et al. (2015), IAPRI working paper 114.
#' Select hh that:
#' Full CA: minumum tillage (planting basins, ripping and zero tillage), crop rotation and residue retention
#' Partial CA: minimum tillage and one other component
#' 
#' Crop rotation is defined as cereal-legume rotation: maize-legume or legume_maize
table(df$tillage)

# Select maize-legume systems
# df <- df %>%
#    filter(crop_hs %in% c("Maize", "Velvet beans", "Mixed beans", "Cowpeas", "Soyabeans")) %>%
#    filter(crop_hs_2010 %in% c("Maize", "Velvet beans", "Mixed beans", "Cowpeas", "Soyabeans", NA))

# Classify fields
df <- df %>%
  mutate(min_til = ifelse(tillage %in% c("Planting basins (potholes)", "Ripping", "Zero tillage"), 1, 0),
         min_til = ifelse(is.na(tillage), NA, min_til),
         res_ret = ifelse(residue == "Left in the field then plowed / incorporated into the field", 1, 0),
         res_ret = ifelse(is.na(residue), NA, res_ret),
         rot = ifelse(crop_hs != crop_hs_2010, 1, 0),
         rot = ifelse(is.na(crop_hs_2010), NA, rot),
         ca_f = ifelse(min_til == 1 & res_ret == 1 & rot == 1, 1, 0),
         ca_p = ifelse((min_til == 1 & res_ret ==1) | (min_til == 1 & rot ==1), 1, 0))

table(df$min_til, useNA = "always")
table(df$res_ret, useNA = "always")
table(df$rot, useNA = "always")
table(df$ca_p, useNA = "always")
table(df$ca_f, useNA = "always")

# Calculate values per hh
hh_df <- df %>%
  group_by(cluster, hh_code) %>%
  summarize(weight = unique(weight),
            min_til = max(min_til),
            res_ret = max(res_ret),
            rot = max(rot),
            ca_f = max(ca_f),
            ca_p = max(ca_p)) %>%
  ungroup

# Summary statistics  
sum_df <- hh_df %>%
  summarize(hh = sum(weight, na.rm = T),
            min_til = sum(weight*min_til, na.rm = T)/sum(weight, na.rm = T)*100,
            rot = sum(weight*rot, na.rm = T)/sum(weight, na.rm = T)*100,
            res_ret= sum(weight*res_ret, na.rm = T)/sum(weight, na.rm = T)*100,
            ca_p= sum(weight*ca_p, na.rm = T)/sum(weight, na.rm = T)*100,
            ca_f= sum(weight*ca_f, na.rm = T)/sum(weight, na.rm = T)*100)

### MAP 
# Add location
hh_df <- hh_df %>%
  left_join(location)

# plot ca_f
ggplot() + 
  geom_path(data = aez, aes(x = long, y = lat, group = group)) +
  geom_point(data = filter(hh_df, ca_f ==1), aes(x = long, y = lat, colour = factor(ca_f))) +
  coord_map() +
  theme_bw()

# plot ca_p
ggplot() + 
  geom_path(data = aez, aes(x = long, y = lat, group = group)) +
  geom_point(data = filter(hh_df, ca_p ==1), aes(x = long, y = lat, colour = factor(ca_p))) +
  coord_map() +
  theme_bw()

