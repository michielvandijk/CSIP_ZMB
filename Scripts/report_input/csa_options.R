#'========================================================================================================================================
#' Project:  CSIP
#' Subject:  Script to describe and project CSA options
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
p_load("WDI", "countrycode", "gdxrrw", "ggthemes", "viridis", "gridExtra", "ggalt", "openxlsx")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)


### SET DATAPATH
source(file.path(root, "Scripts/support/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source(file.path(root, "Scripts/Set_country.R"))


### AEZ
# fig_aez
aez <- readOGR(file.path(projectPath, "/Data/ZMB/Raw/Spatial_data/aez/aez.shp"))

aez_df <- aez@data %>%
  mutate(id = rownames(.))

aez_for <- fortify(aez) %>%
  left_join(aez_df)

fig_aez <- ggplot() +
  geom_polygon(data = aez_for, aes(x = long, y = lat, group = group, fill = ZONES)) +
  scale_fill_manual(values = terrain.colors(5)) +
  coord_quickmap() +
  theme_void() +
  labs(x = "", y = "", fill = "")  +
  theme(legend.position = "bottom")


### CONSERVATION AGRICULTURE

# ca projection
# https://stats.stackexchange.com/questions/30255/fitting-logistic-to-small-number-of-points-in-r
# http://kyrcha.info/2012/07/08/tutorials-fitting-a-sigmoid-function-in-r/
# https://en.wikipedia.org/wiki/Generalised_logistic_function
# fitmodel2 <- nls(y ~ SSlogis(x, Asym, xmid, scal), df)
# params2 <- coef(fitmodel2)

# Sigmoid function
sigmoid = function(g, a, xmid, k, x) {
  a + ((k-a) / (1 + exp(-g * (x - xmid))))
}

# Read data
ca_adop_raw <- read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "ca_adop")

# ca adoption
ca_adop <- ca_adop_raw %>%
  filter(Indicator == "ca", source %in% c("RALS2015", "stakeholders")) %>%
  mutate(value = value/100)

# Assume 2050 rate for ca = 30%
ca_proj <- data.frame(year = c(2010:2050), value = sigmoid(g = .3, 
            a = ca_adop$value[ca_adop$source == "RALS2015"], xmid = 2030, 
            k = ca_adop$value[ca_adop$source == "stakeholders"], 
            x = 2010:2050), option = "ca")

fig_ca_proj <- ggplot(data = ca_proj, aes(x = year, y = value*100)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(0, 40)) +
  labs( x = "", y = "maize area/maize farmers (%)") +
  theme_bw() +
  geom_point(data = ca_adop, aes(x = year, y = value*100), col = "red", size = 4) +
  geom_text(data = ca_adop, aes(x = year, y = value*100, label = value*100), vjust = 0, nudge_y = 2)
  
# msd
msd_adop <- ca_adop_raw %>%
  filter(Indicator == "msd", source %in% c("RALS2015", "stakeholders")) %>%
  mutate(value = value/100)

# Assume 2050 rate for msd = 50%
msd_proj <- data.frame(year = c(2010:2050), value = sigmoid(g = .3, 
                                                           a = msd_adop$value[msd_adop$source == "RALS2015"], xmid = 2030, 
                                                           k = msd_adop$value[msd_adop$source == "stakeholders"], 
                                                           x = 2010:2050), option = "msd")

fig_msd_proj <- ggplot(data = msd_proj, aes(x = year, y = value*100)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(0, 60)) +
  labs( x = "", y = "maize area/maize farmers (%)") +
  geom_point(data = msd_adop, aes(x = year, y = value*100), col = "red", size = 4) +
  theme_bw() +
  geom_text(data = msd_adop, aes(x = year, y = value*100, label = value*100), vjust = 0, nudge_y = 2)

# cr
cr_adop <- ca_adop_raw %>%
  filter(Indicator == "cr", source %in% c("RALS2015", "stakeholders")) %>%
  mutate(value = value/100)

# Assume 2050 rate for msd = 60%
cr_proj <- data.frame(year = c(2010:2050), value = sigmoid(g = .3, 
                                                            a = cr_adop$value[cr_adop$source == "RALS2015"], xmid = 2030,                                                             k = cr_adop$value[cr_adop$source == "stakeholders"], 
                                                            x = 2010:2050), option = "cr")


fig_cr_proj <- ggplot(data = cr_proj, aes(x = year, y = value*100)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(0, 70)) +
  labs( x = "", y = "maize area/maize farmers (%)") +
  geom_point(data = cr_adop, aes(x = year, y = value*100), col = "red", size = 4) +
  theme_bw() +
  geom_text(data = cr_adop, aes(x = year, y = value*100, label = value*100), vjust = 0, nudge_y = 2)

# rr
rr_adop <- ca_adop_raw %>%
  filter(Indicator == "rr", source %in% c("RALS2015", "stakeholders")) %>%
  mutate(value = value/100)

# Assume 2050 rate for msd = 80%
rr_proj <- data.frame(year = c(2010:2050), value = sigmoid(g = .3, 
                                                           a = rr_adop$value[rr_adop$source == "RALS2015"], xmid = 2030, 
                                                           k = rr_adop$value[rr_adop$source == "stakeholders"], 
                                                           x = 2010:2050), option = "rr")

fig_rr_proj <- ggplot(data = rr_proj, aes(x = year, y = value*100)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(0, 90)) +
  labs( x = "", y = "maize area/maize farmers (%)") +
  geom_point(data = rr_adop, aes(x = year, y = value*100), col = "red", size = 4) +
  theme_bw() +
  geom_text(data = rr_adop, aes(x = year, y = value*100, label = value*100), vjust = 0, nudge_y = 1)


### AGRO-FORESTRY
# af yield
af_yld <-  read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "af_yld") %>%
  gather(treatment, value, -crop, -year, - source) %>%
  group_by(crop, treatment) %>%
  summarize(max = max(value),
            min = min(value),
            value = mean(value))

fig_af_yld <- ggplot(data = af_yld, aes(x = treatment, y = value, fill = treatment)) +
  labs( x = "", y = "yield (kg/ha)") +
  guides(fill = "none") +
  geom_col() +
  facet_wrap(~crop, scales = "free") +
  geom_errorbar(aes(ymax = max, ymin = min), width = 0.2) +
  theme_bw()

af_yld_shock <- af_yld %>%
  dplyr::select(-max, -min) %>%
  spread(treatment, value) %>%
  mutate(shock = 1 + (under-outside)/outside)

# af labour
af_lab <- read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "af_lab") 

fig_af_lab <- ggplot(data = af_lab, aes(x = reorder(Management, -labour), y = labour, fill = Management)) +
  labs( x = "", y = "Labour (persons") +
  guides(fill = "none") +
  geom_col() +
  theme_bw()

# af projection
af_adop <- read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "af_adop") %>%
  filter(source %in% c("RALS2015", "stakeholders")) %>%
  mutate(value = value/100)

# Assume 2050 rate for af = 25%
af_proj <- data.frame(year = c(2010:2050), value = sigmoid(g = .3, 
                                                           a = af_adop$value[af_adop$source == "RALS2015"], xmid = 2030, 
                                                           k = af_adop$value[af_adop$source == "stakeholders"], 
                                                           x = 2010:2050), option = "af")

fig_af_proj <-  ggplot(data = af_proj, aes(x = year, y = value*100)) +
  geom_line(size = 2) +
  labs( x = "", y = "maize area/maize farmers (%)") +
  scale_y_continuous(limits = c(0, 30)) +
  geom_point(data = af_adop, aes(x = year, y = value*100), col = "red", size = 4) +
  theme_bw() +
  geom_text(data = af_adop, aes(x = year, y = value*100, label = value*100), vjust = 0, nudge_y = 2)


### DROUGHT-TOLERANT VARIETIES
# dtm yield
dtm_yld <-  read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "dtm_yld") %>%
  gather(reference, value, -yld_level, -source) %>%
  group_by(reference) %>%
  summarize(max = max(value),
            min = min(value),
            value = mean(value))

fig_dtm_yld <- ggplot(data = dtm_yld, aes(x = reference, y = value, fill = reference)) +
  labs( x = "", y = "% yield dtm in comparison to non-dtm") +
  guides(fill = "none") +
  geom_col() +
  geom_errorbar(aes(ymax = max, ymin = min), width = 0.2) +
  theme_bw()

# dtm projection
dtm_adop <- read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "dtm_adop") %>%
  mutate(impr = `non-dtm` + dtm) %>%
  gather(option, value, -year, -source) %>%
  filter(option != "non-dtm")



dtm_proj <- bind_rows(
  data.frame(year = c(1997:2050), value = sigmoid(g = 0.3, 0.23, xmid = 2005, k = 1, x = 1997:2050), option = "impr"),
  data.frame(year = c(1997:2050), value = sigmoid(g = 0.2, 0, xmid = 2020, k = 0.7, x = 1997:2050), option = "dtm")
)

# Assume 2050 rate of 100%
fig_dtm_proj <- ggplot() +
  geom_line(data = dtm_proj, aes(x = year, y = value*100, linetype = option, colour = option), size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_point(data = dtm_adop, aes(x = year, y = value, shape = option, colour = option), size = 4) +
  theme_bw() +
  labs( x = "", y = "maize area/maize farmers (%)", 
          shape = "Type of seeds", linetype = "Type of seeds", colour = "Type of seeds") +
  geom_text(data = dtm_adop, aes(x = year, y = value, label = value), vjust = 0, nudge_y = 5)



### POST-HARVEST LOSSES
# phl estimations
phl <- read_excel(file.path(projectPath, "Data/ZMB/Processed/Options/options_background.xlsx"), sheet = "phl_data") %>%
  mutate(crop = factor(crop, levels = as.character(crop)))

fig_phl <- ggplot(data = phl, aes(x = min, xend = max, y= crop, group=crop)) + 
  geom_dumbbell(colour = "blue",
                size = 2,
                size_x = 4,
                size_xend = 4) + 
  labs(x = "Post harvest loss range (%)", 
       y= "") +
  geom_point(data = filter(phl, is.na(min)), aes(x = max, y = crop), size = 4, col = "blue") +
  theme_bw()
  
# dtm parameters
# We assumem based on literature:
dtm_param <- phl %>%
  mutate(min = ifelse(is.na(min), 0, min),
         value = (max-min)/100,
         unit = "potential decrease in phl (%)",
         option = "dtm",
         source = "Affognon et al. (2015)",
         note = "difference between maximum and minimum of phl") %>%
  dplyr::select(-crop, -min, -max) %>%
  na.omit()

write_csv(dtm_param, file.path(projectPath, "Data/ZMB/Processed/Options/dtm_param.csv"))


### DATABASE WITH PROJECTIONS
# Combine projections
proj <- bind_rows(ca_proj, msd_proj, cr_proj, rr_proj, af_proj, dtm_proj)

# Save
write_csv(proj, file.path(projectPath, "Data/ZMB/Processed/Options/options_adoption.csv"))


