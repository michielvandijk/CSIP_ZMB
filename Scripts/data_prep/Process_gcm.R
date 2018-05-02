#'========================================================================================================================================
#' Project:  CSIP_ZMB
#' Subject:  Process GCM precipitation and temp 
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
p_load("WDI", "countrycode", "grid", "gridExtra", "ncdf4", "velox")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Scripts/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD DATA
# http://neondataskills.org/R/Multi-Band-Rasters-In-R/

isimip_path <- "P:/watxene/ISIMIP/ISIMIP2a_FT/input/HadGEM2-ES/rcp8p5"


# Rainfed 
test2 <- nc_open(file.path(isimip_path, "rhs_hadgem2-es_rcp8p5_2005-2010.nc4"))
test <- stack(file.path(isimip_path, "rhs_hadgem2-es_rcp8p5_2005-2010.nc4"))
test
plot(test[[c(1:3)]])
nbands(test)

test[[c(1:2)]]@layers


, var = "Maize")
names(lu_rf_mai) <- "lu"
lu_rf_wht <- raster(file.path(dataPath, "WP10/Data/Raw_data/climate/happi/landuse.rf.nc4"), var = "Wheat")
names(lu_rf_wht) <- "lu"

# Same result as
# library(ncdf4)
# lu_rf_file <- nc_open(file.path(dataPath, "WP10/Data/Raw_data/climate/landuse.rf.nc4"))
# lu_var_names <- names(lu_rf_file$var)
# lu_var <- lu_rf_file$var[[which(lu_var_names== "Maize")]]
# lu_rf <- ncvar_get(lu_rf_file, lu_var)
# colnames(lu_rf)<-as.character(rev(seq(-89.75, 89.75, 0.5))) # NB REVERSE Y COORD!!
# rownames(lu_rf)<-seq(-179.75, 179.75, 0.5)
# lu_rf_m <- reshape2::melt(lu_rf) %>% setNames(c("x", "y", "value"))
# nc_close(lu_rf_file)


# Irrigated
lu_ir_mai <- raster(file.path(dataPath, "WP10/Data/Raw_data/climate/happi/landuse.ir.nc4"), var = "Maize")
names(lu_ir_mai) <- "lu"
lu_ir_wht <- raster(file.path(dataPath, "WP10/Data/Raw_data/climate/happi/landuse.ir.nc4"), var = "Wheat")
names(lu_ir_wht) <- "lu"


### RASTERIZE GLOBAL MAP TO 0.5 degree
map_wrld_r <- rasterize(wrld_simpl, lu_rf_mai)
names(map_wrld_r) <- "ID"


### CREATE LIST OF ALL FILES
# Create lookup file for loading data HAPPI_OUTv2
gcm <- list.files(file.path(happiPath, "HAPPI_OUTv2"))
variable <- list.files(file.path(happiPath, "HAPPI_OUTv2/CAM4-2degree"))
clim_scen <- list.files(file.path(happiPath, "HAPPI_OUTv2/CAM4-2degree/yield"))

lookup <- expand.grid(gcm = gcm, variable = variable, clim_scen = clim_scen)

# Load all filenames and split
filenames_f <- function(i){
  filenames = data.frame(
    full_filename = list.files(file.path(happiPath, paste0("HAPPI_OUTv2/", lookup$gcm[i], "/", lookup$variable[i], "/", lookup$clim_scen[i])), full.names = T),
    filename = list.files(file.path(happiPath, paste0("HAPPI_OUTv2/", lookup$gcm[i], "/", lookup$variable[i], "/", lookup$clim_scen[i])), full.names = F))
  return(filenames)
}

filenames <- bind_rows(lapply(c(1:nrow(lookup)), filenames_f)) %>%
    separate(filename, c("model", "gcm", "clim_scen", "sim_scen", "system", "ens", "variable", "crop", "scale", "time_step", "sy", "ey") , sep = "_", remove = F) %>%
    separate(ey, c("ey", "ext")) %>%
  filter(variable %in% c("yield"), gcm != "echam6")

# ### CREATE LIST OF ALL FILES
# # Create lookup file for loading data HAPPI_CO2_OUTv2
# gcm_co2 <- list.files(file.path(happiPath, "HAPPI_CO2_OUTv2"))
# variable_co2 <- list.files(file.path(happiPath, "HAPPI_CO2_OUTv2/CAM4-2degree"))
# clim_scen_co2 <- list.files(file.path(happiPath, "HAPPI_CO2_OUTv2/CAM4-2degree/yield"))
# 
# lookup_co2 <- expand.grid(gcm = gcm_co2, variable = variable_co2, clim_scen = clim_scen_co2)
# 
# 
# # Load all filenames and split
# filenames2_f <- function(i){
#   filenames = data.frame(
#     full_filename = list.files(file.path(happiPath, paste0("HAPPI_CO2_OUTv2/", lookup_co2$gcm[i], "/", lookup_co2$variable[i], "/", lookup_co2$clim_scen[i])), full.names = T),
#     filename = list.files(file.path(happiPath, paste0("HAPPI_CO2_OUTv2/", lookup_co2$gcm[i], "/", lookup_co2$variable[i], "/", lookup_co2$clim_scen[i])), full.names = F))
#   return(filenames)
# }
# 
# filenames_co2 <- bind_rows(lapply(c(1:nrow(lookup)), filenames2_f)) %>%
#   separate(filename, c("model", "gcm", "clim_scen", "sim_scen", "system", "ens", "variable", "crop", "scale", "time_step", "sy", "ey") , sep = "_", remove = F) %>%
#   separate(ey, c("ey", "ext")) %>%
#   filter(variable %in% c("yield"), gcm != "echam6")
# 



### EXTRACT DATA PER CROP, SYSTEM AND CLIMATE SCENARIO
# Create lookup table
lookup2 <- expand.grid(crop = c("wht", "mai"), system = c("firr", "noirr"), clim_scen = tolower(clim_scen))

# function to extract data from nc files for Europe
get_yld_f <- function(i, mask = map_eu28, lu_cut = 0.0){
  
  crop_sel = lookup2$crop[i]
  system_sel = lookup2$system[i]
  clim_scen_sel = lookup2$clim_scen[i]
  
  print(paste(crop_sel, system_sel, clim_scen_sel, sep = "_"))
  crp_files <- filenames %>%
  filter(crop == crop_sel, system == system_sel, clim_scen == clim_scen_sel)
  
  if(crop_sel == "wht" & system_sel == "noirr") {lu_map <- lu_rf_wht}
  if(crop_sel == "wht" & system_sel == "firr") {lu_map <- lu_ir_wht}
  if(crop_sel == "mai" & system_sel == "noirr") {lu_map <- lu_rf_mai}
  if(crop_sel == "mai" & system_sel == "firr") {lu_map <- lu_ir_mai}

  stack <- stack(crp_files$full_filename)
  stack <- stack(lu_map, map_wrld_r, stack)
  stack <- crop(stack, mask)
  
  crop_df <- as.data.frame(rasterToPoints(stack)) %>%
    gather(variable, value, -x, -y, -lu, -ID) %>%
    na.omit %>%
    left_join(.,wrld_simpl_df[,c("ID", "iso3c")]) %>%
    filter(lu >= lu_cut) %>%
    mutate(crop = crop_sel, system = system_sel, clim_scen = clim_scen_sel)
  
  return(crop_df)  
}

# Extract and save data
#yld_eu28 <- bind_rows(lapply(c(1:nrow(lookup2)), get_yld_f, map_eu28))
#saveRDS(yld_eu28, file.path(dataPath, "WP10/Data/Raw_data/climate/happi/yld_eu28_happi_no_lu_mask.rds"))
yld_eu28 <- readRDS(file.path(dataPath, "WP10/Data/Raw_data/climate/happi/yld_eu28_happi_no_lu_mask.rds"))

#yld_ssa <- bind_rows(lapply(c(1:nrow(lookup2)), get_yld_f, map_ssa))
#saveRDS(yld_ssa, file.path(dataPath, "WP10/Data/Raw_data/climate/happi/yld_ssa_happi_no_lu_mask.rds"))
yld_ssa <- readRDS(file.path(dataPath, "WP10/Data/Raw_data/climate/happi/yld_ssa_happi_no_lu_mask.rds"))


### EU28 PLOT OF ABSOLUTE VALUES
eu28 <- iso3c2region %>%
  filter(region2 == "EU28")
yld_eu28 <- filter(yld_eu28, iso3c %in% eu28$iso3c)

sum_yld_eu28 <- yld_eu28 %>%
  group_by(crop, system, clim_scen, iso3c) %>%
  summarize(mean = mean(value, na.rm = T),
            min = min(value, na.rm = T),
            max = max(value, na.rm = T))

# Plots
yld_eu28_noirr_mai <- filter(yld_eu28, system == "noirr", crop == "mai") 

ggplot(data = yld_eu28_noirr_mai, aes(x = clim_scen, y = value), outlier.colour = NA) +
  #geom_boxplot() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), aes(fill = clim_scen)) +
  labs(title = paste(unique(yld_eu28_noirr_mai$system), unique(yld_eu28_noirr_mai$crop), sep = "-")) +
  #geom_jitter(height = 0, width = 0.1) +
  facet_wrap(~iso3c, scales = "free") +
  theme(legend.position = "bottom")


### SSA PLOT OF ABSOLUTE VALUES
ssa <- iso3c2region %>%
  filter(region2 == "SSA")
yld_ssa <- filter(yld_ssa, iso3c %in% ssa$iso3c)

sum_yld_ssa <- yld_ssa %>%
  group_by(crop, system, clim_scen, iso3c) %>%
  summarize(mean = mean(value, na.rm = T),
            min = min(value, na.rm = T),
            max = max(value, na.rm = T))

# Plots
yld_ssa_noirr_mai <- filter(yld_ssa, system == "noirr", crop == "mai") 

ggplot(data = yld_ssa_noirr_mai, aes(x = clim_scen, y = value), outlier.colour = NA) +
  #geom_boxplot() +
  labs(title = paste(unique(yld_ssa_noirr_mai$system), unique(yld_ssa_noirr_mai$crop), sep = "-")) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), aes(fill = clim_scen)) + 
  facet_wrap(~ iso3c, scales = "free")


### EU PLOT OF GROWTH
# Calculate average historical yld per grid cell
yld_eu28_av <- yld_eu28 %>%
  filter(clim_scen == "all-hist") %>%
  group_by(x, y, iso3c, crop, system, clim_scen) %>%
  summarize(average = mean(value, na.rm = T)) %>%
  dplyr::select(-clim_scen)

# Calculate change relative to all-hist average
yld_eu28_rel <- left_join(yld_eu28, yld_eu28_av) %>%
  mutate(value = value/average) %>%
  filter(!is.na(value))
summary(yld_eu28_rel)


# Plots
yld_eu28_noirr_mai_rel <- filter(yld_eu28_rel, system == "noirr", crop == "mai") 

ggplot(data = yld_eu28_noirr_mai_rel, aes(x = clim_scen, y = value), outlier.colour = NA) +
  #geom_boxplot() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), aes(fill = clim_scen)) + 
  labs(title = paste(unique(yld_eu28_noirr_mai_rel$system), unique(yld_eu28_noirr_mai_rel$crop), sep = "-")) +
  #geom_jitter(height = 0, width = 0.1) +
  facet_wrap(~iso3c, scales = "free") +
  theme(legend.position = "bottom")

