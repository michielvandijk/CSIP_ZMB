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
# Set ISIMIP path
isimip_path <- "P:/watxene/Wat-Data/ISI-MIP1/multi-GCM_input"


### FUNCTIONS
## Function to slice bundled netcdf into single year files

gcm <- "hadgem2-es"
sy <- 1981
ey <- 1991
folder <- "hist/pr"
poly <- aez
var <- "pr"


gcm_files_f <- function(gcm, sy, ey, var, folder){
  files <-  list.files(file.path(isimip_path, paste0(gcm, "/", folder)), pattern = "^.*\\.nc$|^.*\\.nc4$", full.names = TRUE)
  r <- brick(files[1])
  names(r)
  
  # Select period layers in nc files
  period_df <- data.frame(layer = names(r)) %>%
    mutate(date_raw  = gsub("X", "", layer),
           date = as.Date(date_raw, "%Y.%m.%d"),
           year = format(date,"%Y"))
  # save per year
  y <- 1955
  st <- r
  save_netcdf_f <- function(st, y){
    ry <- r[[period_df$layer[period_df$year == y]]]
    outfile <- file.path(isimip_path, paste0(gcm, "/", folder, "/test2.nc"))
    writeRaster(ry, outfile, overwrite=TRUE, format="CDF", varname="tmp", varunit="z-scores", 
                longname="test variable -- raster layer to netCDF", xname="lon", yname="lat")
  }
  
  
  return(files)
}

writeRaster(tmpin, outfile, overwrite=TRUE, format="CDF", varname="tmp", varunit="z-scores", 
            longname="test variable -- raster layer to netCDF", xname="lon", yname="lat")