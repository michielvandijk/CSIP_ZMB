#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot", "ggthemes")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "maps")
# Additional packages
p_load("quickPlot", "classInt", "countrycode", "viridis")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH
source(file.path(root, "Scripts/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### SET COUNTRY
source(file.path(root, "Scripts/Set_country.R"))

### LOAD DATA
# Simu
simu <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/simu_", iso3c_sel, ".rds")))

# GAUL
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))
adm1_df <- adm1@data %>%
  mutate(id = row.names(.))

# land cover
esa <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/ESA_", iso3c_sel, "_2000.rds")))

# Irrigation
gmia <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gmia_", iso3c_sel, ".rds")))

# world map
wld <- map_data("world") 

### ADM MAP
# Capital
data(world.cities)
capital <- filter(world.cities, country.etc == country_sel, capital == 1)

# Fortify polygon
adm1_for <- fortify(adm1) %>%
  left_join(adm1_df)

# Set colours
cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))

# Set distric labels
districts <- as.data.frame(coordinates(adm1)) %>%
  mutate(name = adm1_df$ADM1_NAME) %>%
  set_names(c("long", "lat", "name")) 

# Draw map
fig_adm1 <- ggplot() +
  geom_polygon(data = wld, aes(x=long, y=lat, group=group), colour = "black", fill = "grey", alpha = 0.5) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  geom_polygon(data = adm1_for, aes(x = long, y = lat, fill = ADM1_NAME, group = group), colour = "black") +
  geom_text(data = districts, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(length(adm1_df$ADM1_NAME))) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank()) +
  coord_map(xlim = c(21, 34), ylim = c(-18.5, -7.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))


### SIMU
fig_simu <- ggplot() +
  geom_polygon(data = wld, aes(x=long, y=lat, group=group), colour = "black", fill = "grey", alpha = 0.5) +
  geom_polygon(data = simu, aes(x = long, y = lat, group = group), colour = "black", fill = "light blue") +
  coord_map() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank()) +
  coord_map(xlim = c(21, 34), ylim = c(-18.5, -7.5)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.5))


### ESA LAND COVER MAP VERSION 1
# Load ESA legend
ESA_legend <- read_csv(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-Legend.csv")) %>%
  mutate(ID = land_cover_code)

# Add attributes
# http://stackoverflow.com/questions/19586945/how-to-legend-a-raster-using-directly-the-raster-attribute-table-and-displaying
esa <- ratify(esa)
rat <- levels(esa)[[1]] #get the values of the unique cell frot the attribute table
rat <- left_join(rat, ESA_legend)

# Create colours for legend and sort in right order
ESA_colour <- rat
ESA_colour <- ESA_colour %>%
  filter(ID %in% seq(0, 220, 10)) %>%
  mutate(colour= rgb(R, G, B, max = 255)) %>%
  unique()
ESA_colour <- ESA_colour[order(ESA_colour$land_cover_short, decreasing = F),]

# Links levels
levels(esa) <- rat
levels(esa)
rm(rat)

# Visualise 
fig_land_cover <- levelplot(esa, att='land_cover_short', col.regions = ESA_colour$colour, margin = F) +
    layer(sp.polygons(adm1, col = "black", lwd = 2))


### ESA LAND COVER MAP VERSION 2
# Load ESA legend and add colours
ESA_legend <- read_csv(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-Legend.csv")) %>%
  mutate(ID = land_cover_code, 
         colour= rgb(R, G, B, max = 255)) %>%
  unique()

# Create named vector for colours 
lc_colors <- ESA_legend$colour
names(lc_colors) <- ESA_legend$land_cover_code

# Create dataframe
lc_df <- as.data.frame(rasterToPoints(esa)) %>%
  setNames(c("x", "y","land_cover_code")) %>% 
  left_join(ESA_legend)

# Plot
fig_esa = ggplot()+
  geom_raster(data = lc_df, aes(x = x, y = y, fill = as.factor(land_cover_code))) +
  geom_path(data = adm1, aes(x = long, y = lat, group = group)) +
  scale_fill_manual(values = lc_colors) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())


### GMIA 
# Load GMIA
gmia <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GMIA_", iso3c_sel, ".rds")))

gmia_df <- as.data.frame(rasterToPoints(gmia)) %>%
  setNames(c("x", "y", "gmia")) %>%
  mutate(gmia_class = cut(gmia, breaks = c(5000, 1000, 10, -1), labels = c("L", "M", "H")))
summary(gmia_df)  

# Map
ggplot() + 
  geom_raster(data = gmia_df, aes(x = x, y = y, fill = as.factor(gmia))) +
  #scale_fill_viridismanual(values = c("H" = "blue", "M" = "red", "L" = "green", na.value = "white")) +
  geom_path(data = adm1, aes(x = long, y = lat, group = group)) +
  scale_fill_viridis(discrete = T) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())


# POPULATION
# Load population map
pop <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/pop_", iso3c_sel, ".rds")))

pop_df <- as.data.frame(rasterToPoints(pop)) %>%
  setNames(c("x", "y", "pop")) %>%
  mutate(pop_class = cut(pop, breaks = c(800000, 100, 50, -1), labels = c("L", "M", "H")))
summary(pop_df)  

# Map
ggplot() + 
  geom_raster(data = pop_df, aes(x = x, y = y, fill = pop_class)) +
  #scale_fill_viridismanual(values = c("H" = "blue", "M" = "red", "L" = "green", na.value = "white")) +
  geom_path(data = adm1, aes(x = long, y = lat, group = group)) +
  scale_fill_viridis(discrete=TRUE) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())


# Travel time
access <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/access_", iso3c_sel, ".rds")))
access_df <- as.data.frame(rasterToPoints(access)) %>%
  setNames(c("x", "y", "access"))

# Map
rev(heat.colors(5))
ggplot() + 
  geom_raster(data = access_df, aes(x = x, y = y, fill = access)) +
  geom_path(data = adm1, aes(x = long, y = lat, group = group)) +
  #scale_fill_viridis(direction = -1) +
  #scale_fill_gradient(low = "orange", high = "red4") +
  scale_fill_gradientn(colors = c("#FFFF00FF", "#FFAA00FF", "#FF5500FF", "#FF0000FF")) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())



### LSMS MAP
# Get location data
# source(file.path(root, "Code/ZMB/Household_surveys/location_ZMB_2010.r"))
# location <- location %>%
#   group_by(lat, lon) %>%
#   summarize(n = n()) %>%
#   mutate(size = 1)
# 
# fig_LSMS <- ggplot()+
#   #geom_raster(data = ZMB_GAEZ, aes(x = x, y = y, fill = layer)) +
#   geom_polygon(data = adm2_for, aes(x = long, y = lat, group = group), fill = "grey", colour = "black") +
#   geom_polygon(data =filter(adm2_for, id == 662), aes(x = long, y = lat), colour = "black", fill = "white") +
#   geom_point(data = location, aes(x = lon, y = lat, size = size), colour = "red") +
#   scale_size_continuous(name = "", range = c(1, 1), labels = "Enumeration areas") +
#   #scale_fill_gradientn(colours = terrain.colors(10)) +
#   #scale_fill_gradient(low = "light green", high = "dark green") +
#   #scale_fill_distiller(palette = "Spectral", name = "Potential\nyield (tons/ha)") +
#   coord_equal() +
#   labs(x="", y="") +
#   theme_classic() +
#   theme(legend.position="none",
#         line = element_blank(),
#         axis.text = element_blank())
# 
# 
# #ggsave("FigTabMap/fig_LSMS.png")

# Create random enumeration areas
ZMB_ext <- extent(adm1)
x_range <- seq(ZMB_ext@xmin, ZMB_ext@xmax, 0.1)
y_range <- seq(ZMB_ext@ymin, ZMB_ext@ymax, 0.1)

x_sample <- sample(x_range, 250, replace = T)
y_sample <- sample(y_range, 250, replace = T)

lsms_sample <- data.frame(x = x_sample, y = y_sample)
coordinates(lsms_sample) <- ~x+y
crs(lsms_sample) <- crs(adm1)
lsms_sample <- gIntersection(lsms_sample, adm1, byid=TRUE)
lsms_sample <- data.frame(lsms_sample)

fig_lsms <- ggplot() +
  geom_polygon(data = wld, aes(x=long, y=lat, group=group), colour = "black", fill = "grey", alpha = 0.5) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  geom_polygon(data = adm1_for, aes(x = long, y = lat, fill = ADM1_NAME, group = group), colour = "black") +
  geom_text(data = districts, aes(x = long, y = lat, label = name), size = 2) +
  geom_point(data = lsms_sample, aes(x = x, y = y), colour = "black") +
  scale_fill_manual(values = cols(length(adm1_df$ADM1_NAME))) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank()) +
  coord_map(xlim = c(21, 34), ylim = c(-18.5, -7.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))

fig_lsms
