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

# fig_aez
aez <- readOGR(file.path(projectPath, "/Data/ZMB/Raw/Spatial_data/aez/aez.shp"))

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
    filter(year >= sy, year <= ey, variable %in% var)  %>%
    mutate(start = as.integer(gsub("[a-zA-Z]", "", start)),
           end = as.integer(gsub("[a-zA-Z]", "", end)))
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
clip_nc_f <- function(files, poly){
  print(basename(files))
  r <- stack(files)
  r <- crop(r, poly)
  r <- mask(r, poly)
  return(r)
}

## Functions for processing PR, which are available in bundles and require summing per year
# Select gcm files - bundle
gcm_files2_f <- function(gcm, sy, ey, var, folder){
  files <- data.frame(full_file_name = list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = TRUE),
                      file_name = list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = F)) %>%
    separate(file_name, into = c("variable", "bced", "ref_sy", "ref_ey", "gcm", "type", "period_ext"), sep = "_", remove = T) %>%
    separate(period_ext, into = c("start", "end_ext"), sep = "-") %>%
    separate(end_ext, into = c("end", "ext"), sep = "\\.") %>%
    mutate(start = as.integer(gsub("[a-zA-Z]", "", start)),
           end = as.integer(gsub("[a-zA-Z]", "", end)))
  
  # Set ceiling so we select right decades
    sy <- floor(sy/10)*10
    ey <- ceiling(ey/10)*10

    files <- files %>% 
      filter(start >= sy, end <= ey, variable %in% var) 
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
clip_nc2_f <- function(files, poly, sy, ey){
  print(basename(files))
  r <- stack(files)
  
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
    ry <- crop(ry, poly)
    ry <- mask(ry, poly)
    ry <- sum(ry, na.rm = T)
    ry <- ry*60*60*24 
  }
  
  # select process over year and stack
  sy_r <- unique(min(period_df$year))
  ey_r <- unique(max(period_df$year))
  r_sum <- stack(lapply(c(sy_r:ey_r), sum_y_f, r))
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
cc_tas[[1]] <- process_cc_f("gfdl-esm2m", 1981, 2000, "tas", "hist/tas", aez)
cc_tas[[2]] <- process_cc_f("gfdl-esm2m", 2041, 2060, "tas", "rcp8p5/tas", aez)

# hadhem2-es
cc_tas[[3]] <- process_cc_f("hadgem2-es", 1981, 2000, "tas", "hist/tas", aez)
cc_tas[[4]] <- process_cc_f("hadgem2-es", 2041, 2060, "tas", "rcp8p5/tas", aez)

# ipsl-cm5a-lr
cc_tas[[5]] <- process_cc_f("ipsl-cm5a-lr", 1981, 2000, "tas", "hist/tas", aez)
cc_tas[[6]] <- process_cc_f("ipsl-cm5a-lr", 2041, 2060, "tas", "rcp8p5/tas", aez)

# miroc-esm-chem
cc_tas[[7]] <- process_cc_f("miroc-esm-chem", 1981, 2000, "tas", "hist/tas", aez) # GIVES WARNING CHECK
cc_tas[[8]] <- process_cc_f("miroc-esm-chem", 2041, 2060, "tas", "rcp8p5/tas", aez)

# noresm1-m
cc_tas[[9]] <- process_cc_f("noresm1-m", 1981, 2000, "tas", "hist/tas", aez)
cc_tas[[10]] <- process_cc_f("noresm1-m", 2041, 2060, "tas", "rcp8p5/tas", aez)
toc()

# Save
saveRDS(cc_tas, file.path(projectPath, "Data/ZMB/Processed/gcm/temperature.rds"))

## PR
tic()
cc_pr <- list()

# gfdl_esm2m
cc_pr[[1]] <- process_cc2_f("gfdl-esm2m", 1981, 2000, "pr", "hist/pr", aez) 
cc_pr[[2]] <- process_cc2_f("gfdl-esm2m", 2041, 2060, "pr", "rcp8p5/pr", aez)

# hadgem2-es
cc_pr[[3]] <- process_cc2_f("hadgem2-es", 1981, 2000, "pr", "hist/pr", aez)
cc_pr[[4]] <- process_cc2_f("hadgem2-es", 2041, 2060, "pr", "rcp8p5/pr", aez)

# ipsl-cm5a-lr
cc_pr[[5]] <- process_cc2_f("ipsl-cm5a-lr", 1981, 2000, "pr", "hist/pr", aez)
cc_pr[[6]] <- process_cc2_f("ipsl-cm5a-lr", 2041, 2060, "pr", "rcp8p5/pr", aez)

# miroc-esm-chem
cc_pr[[7]] <- process_cc2_f("miroc-esm-chem", 1981, 2000, "pr", "hist/pr", aez)
cc_pr[[8]] <- process_cc2_f("miroc-esm-chem", 2041, 2060, "pr", "rcp8p5/pr", aez)

# noresm1-m
cc_pr[[9]] <- process_cc2_f("noresm1-m", 1981, 2000, "pr", "hist/pr_v2", aez)
cc_pr[[10]] <- process_cc2_f("noresm1-m", 2041, 2060, "pr", "rcp8p5/pr", aez)
toc()

# Save
saveRDS(cc_pr, file.path(projectPath, "Data/ZMB/Processed/gcm/precipitation.rds"))


### PLOT TAS
# smooth and stack
tas <- stack(lapply(cc_tas, function(x) disaggregate(x, 6, method='bilinear')))
tas <- disaggregate(tas, 10)
tas <- mask(tas, aez)

plot(tas)

# convert to df and calculate difference
tas_df <- as.data.frame(rasterToPoints(tas)) %>%
  gather(file, value, -x, -y) %>%
  separate(file, c("gcm", "type", "variable"), sep = "_") %>%
  spread(type, value) %>%
  mutate(value = rcp8p5 - hist,
         bin = cut(value, seq(1, 4, 0.5), labels = c(seq(1.5, 4, 0.5))))
saveRDS(tas_df, file.path(projectPath, "Data/ZMB/Processed/gcm/tas_df.rds"))
summary(tas_df)

# Prepare AEZ
aez_df <- aez@data %>%
  mutate(id = rownames(.))

aez_for <- fortify(aez) %>%
  left_join(aez_df)

# Plot
# Set number of colors
n_col <- length(unique(tas_df$bin))
col <- c(brewer.pal(6,"Reds"))

p = ggplot() +
  geom_raster(data = tas_df, aes(x = x, y = y, fill = bin)) +
  geom_path(data = aez_for, aes(x = long, y = lat, group = group)) +
  coord_cartesian() +
  facet_wrap(~gcm) +
  scale_fill_manual(values = brewer.pal(n = n_col, name = "Reds")) +
  labs(x="", y="", fill = "Absolute change (K)") +
  theme_void() +
  theme(legend.position = "bottom", legend.title.align=0.5) +
  guides(fill = guide_legend(label.position = "bottom", title.position = "top", nrow = 1, colour = "black")) 

p


### PLOT PR
# smooth and stack
pr <- stack(cc_pr)
pr[pr==0] <- NA
pr <- disaggregate(pr, 6, method='bilinear')
pr <- disaggregate(pr, 10)
pr <- mask(pr, aez)

plot(pr)

# convert to df and calculate difference
pr_df <- as.data.frame(rasterToPoints(pr)) %>%
  gather(file, value, -x, -y) %>%
  separate(file, c("gcm", "type", "variable"), sep = "_") %>%
  spread(type, value) %>%
  mutate(value = (rcp8p5 - historical)/historical*100,
                   #bin = cut(value, c(-100, -7.5, -2.5, 2.5, 7.5, 100), labels = c("< 7.5%", "-7.5% to -2.5%", "-2.5% to 2.5%", "2.5% to 7.5%", ">7.5%")))
                   bin = cut(value, c(-100, -6, -4, -2, 2, 4, 6, 100), labels = c("< -6%", "-6% to -4%", "-4% to -2%", "-2% to 2%", "2% to 4%", "4% to 6%", ">6%")))
saveRDS(pr_df, file.path(projectPath, "Data/ZMB/Processed/gcm/pr_df.rds"))
summary(pr_df)

# Prepare AEZ
aez_df <- aez@data %>%
  mutate(id = rownames(.))

aez_for <- fortify(aez) %>%
  left_join(aez_df)

# Plot
# Set number of colors
n_col <- length(unique(pr_df$bin))
col <- c(brewer.pal(7,"RdBu"))

p = ggplot() +
  geom_raster(data = pr_df, aes(x = x, y = y, fill = bin)) +
  geom_path(data = aez_for, aes(x = long, y = lat, group = group)) +
  coord_cartesian() +
  facet_wrap(~gcm) +
  scale_fill_manual(values = col) +
  labs(x="", y="", fill = "% change") +
  theme_void() +
  theme(legend.position = "bottom", legend.title.align=0.5) +
  guides(fill = guide_legend(label.position = "bottom", title.position = "top", nrow = 1, colour = "black")) 

p
