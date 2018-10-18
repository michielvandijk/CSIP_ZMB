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
# Zambia polygon
luid_p_zmb <- readRDS(file.path(projectPath, "Data/ZMB/Processed/Maps/luid_p_ZMB.rds"))

zmb_map <- fortify(luid_p_zmb, region = "Field1_1") %>%
  rename(LUId= id)

# http://neondataskills.org/R/Multi-Band-Rasters-In-R/

# Set ISIMIP path
isimip_path <- "P:/watxene/Wat-Data/ISI-MIP1/multi-GCM_input"


### FUNCTIONS
## Functions for processing TAS, which are available per year and for which average can be taken
# Select gcm files
gcm_files_f <- function(gcm, sy, ey, var, folder){
  files <- data.frame(full_file_name = list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = TRUE),
                             file_name = list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = F)) %>%
    separate(file_name, into = c("variable", "bced", "ref_sy", "ref_ey", "gcm", "type", "year_ext"), sep = "_", remove = T) %>%
    separate(year_ext, into = c("year", "extension"), sep = "\\.") %>%
    filter(year >= sy, year <= ey, variable %in% var)
  return(files)
}

# Stack nc files, crop, mask and average
process_cc_f <- function(gcm, sy, ey, var, folder, poly){
  files_df <- gcm_files_f(gcm, sy, ey, var, folder)
  files <- files_df$full_file_name
  type <- unique(files_df$type)
  df <- stack(lapply(files, clip_nc_f, poly))
  mean_var <- mean(df, na.rm = T)
  names(mean_var) <- paste(gcm, type, var, sep = "_")
  return(mean_var)
}

# clip nc file
clip_nc_f <- function(files_nc, poly){
  print(basename(files_nc))
  r <- stack(files_nc)
  r <- crop(r, poly)
  r <- mask(r, poly)
  return(r)
}

# Select gcm files - bundle
gcm_files2_f <- function(gcm, sy, ey, var, folder){
  files <- data.frame(full_file_name = list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = TRUE),
                      file_name = list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = F)) %>%
    separate(file_name, into = c("variable", "bced", "ref_sy", "ref_ey", "gcm", "type", "period_ext"), sep = "_", remove = T) %>%
    separate(period_ext, into = c("start", "end_ext"), sep = "-") %>%
    separate(end_ext, into = c("end", "ext"), sep = "\\.") %>%
    filter(start >= sy, end <= ey+9, variable %in% var)
  return(files)
}


# Stack nc files - bundle, crop, mask and average
process_cc2_f <- function(gcm, sy, ey, var, folder, poly){
  files_df <- gcm_files2_f(gcm, sy, ey, var, folder)
  files <- files_df$full_file_name
  type <- unique(files_df$type)
  df <- stack(lapply(files, clip_nc2_f, poly, sy, ey))
  mean_var <- mean(df, na.rm = T)
  names(mean_var) <- paste(gcm, type, var, sep = "_")
  return(mean_var)
}

# clip nc file - bundle

# clip nc file - bundle
clip_nc2_f <- function(files_nc, poly, sy, ey){
  print(basename(files_nc))
  r <- stack(files_nc)
  
  # Set period
  start <- as.Date(paste0(sy, "/01/01"), "%Y/%m/%d")
  end <- as.Date(paste0(ey, "/12/31"), "%Y/%m/%d")
  
  # Select period layers in nc files
  period_df <- data.frame(layer = names(r)) %>%
    mutate(date_raw  = gsub("X", "", layer),
           date = as.Date(date_raw, "%Y.%m.%d"),
           year = format(date,"%Y")) %>%
    filter(date >= start, date <= end)
  
  # sum all values in the same year and multiply with 60x60x24 as values are in kg m-2 s-1 which is equal to mm/second 
  # function to sum over year, crop, mask and multiply with 60*60*20
  sum_y_f <- function(y, st){
    print(y)
    ry <- st[[period_df$layer[period_df$year == y]]]
    ry <- sum(ry, na.rm = T)
    ry <- crop(ry, poly)
    ry <- mask(ry, poly)
    ry <- ry*60*60*24 
  }
  
  # select process over year and stack
  r_sum <- stack(lapply(c(sy:ey), sum_y_f, r))
  return(r_sum)
}

# Function to create df from raster with mean var info
raster2df_f <- function(gcm, sy, ey, var, folder, poly){
  r <- process_cc_f(gcm, sy, ey, var, folder, poly)
  df <- as.data.frame(rasterToPoints(r)) %>%
  setNames(c("x", "y", "value")) %>%
  mutate(variable = var, 
         gcm = gcm,
         year = paste(sy, ey, sep = "_"))
  return(df)
}

### DOWNLOAD DATA
# For some GCMs-variable combinations the nc files are available per year and for others in a bundle. 
# We use different functions to handle this

## TAS
tic()
cc_tas <- list()

# gfdl_esm2m
cc_tas[[1]] <- process_cc_f("gfdl-esm2m", 1981, 2000, "tas", "hist/tas", luid_p_zmb)
cc_tas[[2]] <- process_cc_f("gfdl-esm2m", 2041, 2060, "tas", "rcp8p5/tas", luid_p_zmb)

# hadhem2-es
cc_tas[[3]] <- process_cc_f("hadgem2-es", 1981, 2000, "tas", "hist/tas", luid_p_zmb)
cc_tas[[4]] <- process_cc_f("hadgem2-es", 2041, 2060, "tas", "rcp8p5/tas", luid_p_zmb)

# ipsl-cm5a-lr
cc_tas[[5]] <- process_cc_f("ipsl-cm5a-lr", 1981, 2000, "tas", "hist/tas", luid_p_zmb)
cc_tas[[6]] <- process_cc_f("ipsl-cm5a-lr", 2041, 2060, "tas", "rcp8p5/tas", luid_p_zmb)

# miroc-esm-chem
cc_tas[[7]] <- process_cc_f("miroc-esm-chem", 1981, 2000, "tas", "hist/tas", luid_p_zmb)
cc_tas[[8]] <- process_cc_f("miroc-esm-chem", 2041, 2060, "tas", "rcp8p5/tas", luid_p_zmb)

# noresm1-m
cc_tas[[9]] <- process_cc_f("noresm1-m", 1981, 2000, "tas", "hist/tas", luid_p_zmb)
cc_tas[[10]] <- process_cc_f("noresm1-m", 2041, 2060, "tas", "rcp8p5/tas", luid_p_zmb)
toc()

## PR
cc_pr <- list()

# gfdl_esm2m
cc_pr[[1]] <- process_cc2_f("gfdl-esm2m", 1981, 2000, "pr", "hist/pr", luid_p_zmb) 
cc_pr[[2]] <- process_cc2_f("gfdl-esm2m", 2041, 2060, "pr", "rcp8p5/pr", luid_p_zmb)

# hadhem2-es
cc_pr[[3]] <- process_cc2_f("hadgem2-es", 1981, 2000, "pr", "hist/pr", luid_p_zmb)
cc_pr[[4]] <- process_cc2_f("hadgem2-es", 2041, 2060, "pr", "rcp8p5/pr", luid_p_zmb)

# ipsl-cm5a-lr
cc_pr[[5]] <- process_cc2_f("ipsl-cm5a-lr", 1981, 2000, "pr", "hist/pr", luid_p_zmb)
cc_pr[[6]] <- process_cc2_f("ipsl-cm5a-lr", 2041, 2060, "pr", "rcp8p5/pr", luid_p_zmb)

# miroc-esm-chem
cc_pr[[7]] <- process_cc2_f("miroc-esm-chem", 1981, 2000, "pr", "hist/pr", luid_p_zmb)
cc_pr[[8]] <- process_cc2_f("miroc-esm-chem", 2041, 2060, "pr", "rcp8p5/pr", luid_p_zmb)

# noresm1-m
cc_pr[[9]] <- process_cc2_f("noresm1-m", 1981, 2000, "pr", "hist/pr", luid_p_zmb)
cc_pr[[10]] <- process_cc2_f("noresm1-m", 2041, 2060, "pr", "rcp8p5/pr", luid_p_zmb)



# Plot difference between base year and projection

y <- disaggregate(tas_1980, 6, method='bilinear') # to 5 arcmin
plot(y)
m <- mask(y, luid_p_zmb)
plot(m)
plot(luid_p_zmb, add = T)
plot(tas_1980)
test <- raster2df_f("gfdl-esm2m", 1980, 1981, "tas", "hist", luid_p_zmb)

ggplot() +
  geom_raster(data = test, aes(x = x, y = y, fill = value)) +
  coord_cartesian() +
  theme_void()

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

