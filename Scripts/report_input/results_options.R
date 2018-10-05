#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to make figures using GLOBIOM output
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
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


### PREPARE GAMS LINK
igdx(GAMSPath)


### LOAD DATA
# Zambia GLOBIOM OUTPUT Data
zmb_raw <- rgdx.param(file.path(projectPath, paste0("GLOBIOM/results/", globiom_file)), "OUTPUT_ZMB") %>%
  setNames(c("scenario2", "variable", "unit", "ANYREGION", "item", "ssp", "scenario", "enscen", "year", "value")) %>%
  mutate(year = as.integer(as.character(year)),
         item = as.character(item)) %>%
  filter(ANYREGION == "ZambiaReg") %>%
  droplevels

# Scenario definitions
scen_def <- read_excel(file.path(projectPath, "/GLOBIOM/results/scenario_def_v3.xlsx")) 

# LC Map
#lc_type_map <- read_excel(file.path(dataPath, "Data/Mappings/GLOBIOM_mappings.xlsx"), sheet = "LC_TYPE")


### PROCESS RAW DATA
# Add scenario definitions
zmb <- zmb_raw %>%
  mutate(year = as.integer(as.character(year)),
         variable = toupper(variable)) %>%
  dplyr::filter(variable %in% c("YILM", "EMIS", "LAND", "XPRP", "ANIM", "CONS", "PROD", "NETT", "CALO", "EXPO", "IMPO", "NTMS", "NTMS2"))

# Check for missing 2010 values
check2010 <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  filter(!any(year==2010))

# # Remove series with missing values in 2010
zmb <- zmb %>%
  arrange(variable, scenario, item, unit, ssp, scenario2, year) %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  filter(any(year==2010))
xtabs(~item + variable, data = zmb)

# Add growth and index
zmb <- zmb %>%
  group_by(variable, scenario, item, unit, ssp, scenario2) %>%
  mutate(
    index = value/value[year == 2010],
    growth = (index-1)*100)

# Add scenario definition
zmb <- zmb %>% 
  left_join(., scen_def) %>%
  mutate(option = factor(option, levels = c("none", "af", "ca", "rr", "msd", "dtm", "ir", "phl", "div", "def")))

# # Remove additonal scenarios for now
 zmb <- zmb %>%
   filter(!option %in% c("def", "ir"))


### PLOT FUNCTIONS
# Colours
col_options <- brewer.pal(n = 9, name = "Set1")
names(col_options) = c("none", "af", "ca", "rr", "msd", "dtm", "phl", "div")

# Growth 2000-2050 with cc
plot_growth <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel) %>%
    ungroup %>%
    group_by(option, item) %>%
    mutate(max_val = max(growth, na.rm = T),
           min_val = min(growth, na.rm = T)) %>%
    filter(rcp == "noCC", year == 2050)
    
    p = ggplot(data = df) +
    geom_col(aes(x = option, y = growth, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "growth (2010-2050)", title = unique(df$variable)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
  p
}

# Difference in 2050 (%) with cc
plot_dif <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel, option != "none") %>%
    ungroup %>%
    group_by(option, item) %>%
    mutate(max_val = max(dif, na.rm = T),
           min_val = min(dif, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
           ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = dif, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "dif BAU-option in 2050 (%)") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  # +
  #   geom_text(aes(x = option, y = dif, label = round(dif, 0)), hjust = 0, vjust = 0) +
  #   geom_text(aes(x = option, y = min_val, label = round(min_val, 0)), hjust = 0, vjust = 1) +
  #   geom_text(aes(x = option, y = max_val, label = round(max_val, 0)), hjust = 0, vjust = -1) 
  p
}


# Difference in 2050 (%) with cc: no facet_wrap
plot_dif2 <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel, option != "none") %>%
    ungroup %>%
    group_by(option, item) %>%
    mutate(max_val = max(dif, na.rm = T),
           min_val = min(dif, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
    ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = dif, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = "dif BAU-option in 2050 (%)") + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  # +
  #   geom_text(aes(x = option, y = dif, label = round(dif, 2)), hjust = 0, vjust = 0) +
  #   geom_text(aes(x = option, y = min_val, label = round(min_val, 2)), hjust = 0, vjust = 1) +
  #   geom_text(aes(x = option, y = max_val, label = round(max_val, 2)), hjust = 0, vjust = -1) 
  p
}


# Plot absolute values in 2010 and 2050
plot_abs <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel) %>%
    ungroup %>%
    group_by(option, item, year) %>%
    mutate(max_val = max(value, na.rm = T),
           min_val = min(value, na.rm = T)) %>%
    filter(rcp == "noCC", year %in% c(2050))
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = value, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
    labs(x = "", y = unique(df$unit)) + 
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~ item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  p
}

# Plot difference in absolute values with BAU
plot_dif_abs <- function(df, ssp_sel){
  df <- df %>% 
    filter(ssp == ssp_sel, option != "none") %>%
    ungroup %>%
    group_by(option, item) %>%
    mutate(max_val = max(dif_abs, na.rm = T),
           min_val = min(dif_abs, na.rm = T)
           #l_qtl = quantile(dif, 1/4),
           #u_qtl = quantile(dif, 3/4)
    ) %>%
    filter(rcp == "noCC", year == 2050)
  
  p = ggplot(data = df) +
    geom_col(aes(x = option, y = dif_abs, fill = option), colour = "black") +
    geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
    guides(fill=F, colour = F) +
    scale_fill_manual(values = col_options) +
    labs(x = "", y = paste0("dif BAU-option in 2050 ", "(", unique(df$unit), ")")) + 
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~item, scales = "free") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  # +
  #   geom_text(aes(x = option, y = dif_abs, label = round(dif_abs, 0)), hjust = 0, vjust = 0) +
  #   geom_text(aes(x = option, y = min_val, label = round(min_val, 0)), hjust = 0, vjust = 1) +
  #   geom_text(aes(x = option, y = max_val, label = round(max_val, 0)), hjust = 0, vjust = -1) 
  p
}




  
# Baseline selection
# base_options <- zmb %>% 
#   ungroup() %>%
#   filter(scen_type %in% c("baseline", "CSA", "irrigation"), 
#          gcm == "noCC", ssp == "SSP2", year == 2050)

cc_options <- zmb  %>%
  ungroup() %>%
  filter(scen_type %in% c("none", "CSA")) %>%
  filter(year == 2050) 


### YIELD
# Selected crops
crop_globiom <- c("Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott", "Gnut", "Mill", "Pota", "Rape", 
                  "Rice", "Soya", "Srgh", "SugC", "sunf", "SwPo", "Whea")
crop_sel <- c("Corn", "Cass", "Gnut", "Mill", "SugC", "ChkP", "Cott", "Soya")


# projections
yld_opt <- cc_options %>%
  filter(item %in% crop_sel, variable == "YILM", unit == "fm t/ha") %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[option == "none"])/value[option == "none"]*100) %>%
  mutate(dif_abs = (value-value[scen_type == "none"])) 

fig_opt_yld <- plot_growth(yld_opt, "SSP2")

fig_opt_yld_dif <- plot_dif(yld_opt, "SSP2")

fig_opt_yld_abs <- plot_abs(yld_opt, "SSP2")


### PRODUCTION
# Total production
prod_opt_tot <- cc_options %>%
  filter(variable == "PROD", item %in% crop_globiom, unit == "1000 t dm") %>%
  ungroup() %>%
  group_by(year, variable, unit, gcm, crop_model, scen_type, rcp, ssp, scenario, option) %>%
  summarize(value = sum(value)) %>%
  mutate(item = "TOT")

prod_opt <- bind_rows(
  cc_options %>%
    filter(variable == "PROD", item %in% crop_sel, unit == "1000 t dm"),
  prod_opt_tot) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

#fig_opt_prod <- plot_growth(prod_opt, "SSP2")

fig_opt_prod_dif <- plot_dif(prod_opt, "SSP2")

fig_opt_prod_abs <- plot_abs(prod_opt, "SSP2")




### LAND
# Excluding diversification scenario
land_opt <- cc_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "NatLnd", "GrsLnd", "Forest")) %>%
  filter(option != "div") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif_abs = (value-value[scen_type == "none"])) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_land <- plot_growth(land_opt, "SSP2")

fig_opt_land_dif <- plot_dif(land_opt, "SSP2")

fig_opt_land_dif_abs <- plot_dif_abs(land_opt, "SSP2")

fig_opt_land_abs <- plot_abs(land_opt, "SSP2")

# Diversification scenario
land_opt_div <- cc_options %>%
  filter(variable == "LAND", item %in% c("CrpLnd", "NatLnd", "GrsLnd", "Forest")) %>%
  filter(option %in% c("none", "div")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif_abs = (value-value[scen_type == "none"])) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_land_dif_div <- plot_dif(land_opt_div, "SSP2")

fig_opt_land_dif_abs_div <- plot_dif_abs(land_opt_div, "SSP2")

### PRICE
price_opt <- cc_options %>%
  filter(variable == "XPRP", item %in% crop_sel) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)


fig_opt_price <- plot_growth(price_opt, "SSP2")

fig_opt_price_dif <- plot_dif(price_opt, "SSP2")

fig_opt_price_abs <- plot_abs(price_opt, "SSP2")


### TRADE
trade_opt <- cc_options %>%
  filter(variable %in% c("NETT"), unit %in% c("1000 t"),
         item %in% crop_sel, ssp == "SSP2") %>%
  dplyr::select(-scenario, -scenario2) %>%
  complete(item, variable, nesting(unit, ANYREGION, option, ssp, enscen, year, scen_type, gcm, rcp, crop_model), 
           fill = list(value = 0)) %>% # set missing EXPO and IMP not reported because zero to zero = no trade %>%
  filter(ssp == "SSP2", item %in% crop_sel) %>%
  filter(!item %in% c("ChkP", "SugC")) %>%
  ungroup %>%
  group_by(variable, option, item) %>%
  mutate(max_val = max(value, na.rm = T),
         min_val = min(value, na.rm = T)
              #l_qtl = quantile(dif, 1/4),
              #u_qtl = quantile(dif, 3/4)
       ) %>%
   filter(rcp == "noCC", year %in% 2050)

fig_opt_trade <- ggplot(data = trade_opt) +
       geom_col(aes(x = option, y = value, fill = option), colour = "black", position = "dodge") +
       geom_errorbar(aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
       guides(fill=F, colour = F) +
       scale_fill_manual(values = col_options) +
       labs(x = "", y = "Nett trade (1000 t)") + 
       theme_bw() +
       scale_y_continuous(expand = expand_scale(mult = c(0.5, .5))) +
       #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       facet_wrap(~item, scales = "free") +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank())
# +
#        geom_text(aes(x = option, y = value, label = round(value, 0)), hjust = 0, vjust = 0) +
#        geom_text(aes(x = option, y = value, label = round(min_val, 0)), hjust = 0, vjust = 1) +
#        geom_text(aes(x = option, y = value, label = round(max_val, 0)), hjust = 0, vjust = -1) 
     

### LIVESTOCK
lvst_opt1 <- cc_options %>%
  filter(variable == "ANIM", item %in% c("BVMEAT","SGMEAT", "PGMEAT", "PTMEAT","ALMILK")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)


fig_opt_lvst1 <- plot_growth(lvst_opt1, "SSP2")

fig_opt_lvst_dif1 <- plot_dif(lvst_opt1, "SSP2")

fig_opt_lvst_abs1 <- plot_abs(lvst_opt1, "SSP2")

lvst_opt2 <- cc_options %>%
  filter(variable == "ANIM", item %in% c("PIGS","BOVD", "BOVO", "BOVF","SGTO", "PTRB","PTRH")) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_lvst2 <- plot_growth(lvst_opt2, "SSP2")

fig_opt_lvst_dif <- plot_dif(lvst_opt2, "SSP2")

fig_opt_lvst_abs2 <- plot_abs(lvst_opt2, "SSP2")


### CALORIES
calo_opt <- cc_options %>%
  filter(variable == "CALO") %>%
  filter(item %in% "TOT") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

fig_opt_calo <- plot_growth(calo_opt, "SSP2")

fig_opt_calo_dif <- plot_dif2(calo_opt, "SSP2")

fig_opt_calo_abs <- plot_abs(calo_opt, "SSP2")

### EMISSIONS
# Aggregated
emis_opt <- cc_options %>%
  filter(variable == "EMIS") %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O", "TOT")) %>%
  mutate(item = recode(item, "Entferm_CH4" = "Enteric fermentation",
                       "ManmgtTot_N2O" = "Manure Management",
                       "ManmgtTot_CH4" = "Manure Management",
                       "Rice_CH4" = "Rice Cultivation",
                       "CropSoil_N2O" = "Synthetic Fertilizers",
                       "ManaplTot_N2O" = "Manure applied to Soils",
                       "ManprpTot_N2O"= "Manure left on Pasture",
                       "CropRes_N2O" = "Crop Residues",
                       "LUC" = "Land use change")) %>%
  group_by(year, variable, item, unit, gcm, crop_model, rcp, ssp, option, scenario, scen_type, crop_model) %>%
  summarize(value = sum(value)) 
  
emis_opt_ag <- emis_opt %>%
  #filter(item != "Land use change") %>%
  filter(item == "Synthetic Fertilizers") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

emis_opt_lulucf <- emis_opt %>%
  filter(item == "Land use change") %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value[option == "none"]*100)

#fig_opt_emis <- plot_growth(emis_opt, "SSP2")

fig_opt_ag_emis_dif <- plot_dif2(emis_opt_ag, "SSP2")

fig_opt_ag_emis_abs <- plot_abs(emis_opt_ag, "SSP2")

fig_opt_lulucf_emis_dif <- plot_dif2(emis_opt_lulucf, "SSP2")

fig_opt_lulucf_emis_abs <- plot_abs(emis_opt_lulucf, "SSP2")



# Disaggregated
emis_opt2 <- cc_options %>%
  filter(variable == "EMIS") %>%
  filter(!item %in% c("LUCF", "LUCP", "LUCC", "LUCG", "Net", "Soil_N2O")) %>%
  mutate(item = ifelse(item %in% c("ManmgtTot_N2O", "ManaplTot_N2O", "ManprpTot_N2O", "ManmgtTot_CH4"), "Manure", item)) %>%
  group_by(year, item, variable, unit, gcm, crop_model, rcp, ssp, scen_type, option) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, item, unit, gcm, crop_model, rcp, ssp) %>%
  mutate(dif = (value-value[scen_type == "none"])/value*100) %>%
  filter(ssp == "SSP2") %>%
  ungroup %>%
  group_by(option, item, year) %>%
  mutate(max_val = max(value, na.rm = T),
         min_val = min(value, na.rm = T)) %>%
  filter(rcp == "noCC", year %in% c(2050))

# Plot
fig_opt_emis_abs2 <- ggplot(data = emis_opt2) +
  geom_col(data = filter(emis_opt2, item != "TOT"), aes(x = option, y = value, fill = option, alpha = item), colour = "black") +
  geom_errorbar(data = filter(emis_opt2, item == "TOT"), aes(x = option, ymin = min_val, ymax = max_val), width = 0.5) +
  #scale_fill_manual("Subject", values=colours, guide = "none")
  guides(fill=F, colour = F) +
  scale_fill_manual(values = col_options) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  labs(x = "", y = unique(emis_opt2$unit), title = unique(emis_opt2$variable)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


### SUMMARY TABLE
# We rank each of the options on selected variables: Prod, kcal/cap/day, LUC, GHG
# We use difference with the BAU in 2050 for ranking

# Combine data
rank_df <- bind_rows(
  prod_opt %>%
    ungroup() %>%
    filter(item == "TOT", year == 2050, ssp == "SSP2", rcp != "noCC") %>%
    group_by(option, item, variable) %>%
    summarize(dif = mean(dif, na.rm = T))  %>%
    dplyr::select(option, dif, variable) %>%
    mutate(dif = dif*-1), # reverse order for ranking as more is better in contrast to the other two indicators 
  emis_opt_ag %>%
    ungroup() %>%
    filter(item == "Synthetic Fertilizers", year == 2050, ssp == "SSP2", rcp != "noCC") %>%
    group_by(option, item, variable) %>%
    summarize(dif = mean(dif, na.rm = T))  %>%
    dplyr::select(option, dif, variable) %>%
    mutate(dif = ifelse(option == "af", 0, dif)), # Restrict af emis, should be zero in update model version
  land_opt %>%
    ungroup() %>%
    filter(item == "CrpLnd", year == 2050, ssp == "SSP2", rcp != "noCC") %>%
    group_by(option, item, variable) %>%
    summarize(dif = mean(dif, na.rm = T))  %>%
    dplyr::select(option, dif, variable)) %>%
  filter(!option == "none") %>%
  ungroup() %>%
  dplyr::select(-item) %>%
  filter(variable != "LAND")

# Calculate ranking
rank <- rank_df %>%
  group_by(variable) %>%
  mutate(rank = dense_rank((dif))) %>%
  dplyr::select(-dif) %>%
  spread(variable, rank) %>%
  ungroup() %>%
  mutate(total = (PROD + EMIS)/3, # Remove land for now
         total = min_rank(total)) %>%
  gather(variable, value, -option)
  
# Figure
# Set number of colors
n_col <- length(unique(rank$option))

fig_rank <- ggplot(data = rank, aes(x = variable, y = reorder(option, desc(value)), fill = factor(value))) +
  geom_tile(colour = "black") +
  labs(x = "Indicator", y = "CSA option", fill = "rank") +
  scale_fill_manual(values = brewer.pal(n = n_col, name = "YlOrRd")) +
  #scale_fill_manual(values = rev(heat.colors(7))) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0))   # theme_bw() +
  # theme(legend.position = "bottom") +
  # guides(colour = guide_legend(nrow = 1))
