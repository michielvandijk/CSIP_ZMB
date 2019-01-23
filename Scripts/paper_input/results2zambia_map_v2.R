# save results to netcdf file
# for IMAP project
# for George C. Hurtt; NetCDF 3 format with dimensions of: time, lat, lon 
rm(list=ls())

#require(ncdf)
require(raster)
# LIBRARIES ---------------------------------------------------------------
library(ncdf4) 
library(ncdf4.helpers)
library(PCICt)
library(lattice)
library(ggplot2)
require(reshape2)
require(rgdal)
require(RColorBrewer)
require(magick)
require(gdxrrw)

#if in mac os x
if (Sys.info()[1]!="Windows") {
  igdx("/Applications/GAMS24.7/sysdir")
} else {
  igdx("C:/GAMS_23.3/win64/24.3")
}

p4s = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

source_path = "../downscale_maximumEntropy/data_gdx/";
#target_path = "I:/krisztin/4michiel/downscaled_zambia/";
target_path = "I:/krisztin/4amanda/ds_maps_zambia/";
# load from file
#settings = read.csv("_settings.csv",stringsAsFactors = FALSE)
#source_path = settings$temp_source_path
#target_path = settings$final_target_path
#date_tag = settings$date_tag

# load from file
#scenarios = read.csv("_scenarios.csv",stringsAsFactors = FALSE)
scenarios = data.frame(
  RCPs = c("RCP1p8"  ,"RCP1p9"  ,"RCP2p6"  ,"RCP3p7"  ,"RCP4p5"  ,"RCP6p0" ,"RCPref"  ,
           "RCP1p8"  ,"RCP1p9"  ,"RCP2p6"  ,"RCP3p7"  ,"RCP4p5"  ,"RCP6p0" ,"RCPref"  ,
           "RCP3p7"  ,"RCP4p5"  ,"RCP6p0"  ,"RCPref"  ),
  SPAs = c("SPA1"    ,"SPA1"    ,"SPA1"    ,"SPA1"    ,"SPA1"    ,"SPA1"   ,"SPA0"    ,
           "SPA2"    ,"SPA2"    ,"SPA2"    ,"SPA2"    ,"SPA2"    ,"SPA2"   ,"SPA0"    ,
           "SPA3"    ,"SPA3"    ,"SPA3"    ,"SPA0"    ),
  SSPs = c("SSP1"    ,"SSP1"    ,"SSP1"    ,"SSP1"    ,"SSP1"    ,"SSP1"   ,"SSP1"    ,
           "SSP2"    ,"SSP2"    ,"SSP2"    ,"SSP2"    ,"SSP2"    ,"SSP2"   ,"SSP2"    ,
           "SSP3"    ,"SSP3"    ,"SSP3"    ,"SSP3"    ),
  time = c("mar2016" ,"mar2016" ,"mar2016" ,"mar2016" ,"mar2016" ,"mar2016","mar2016" ,
           "mar2016" ,"oct2016" ,"mar2016" ,"mar2016" ,"mar2016" ,"mar2016","mar2016" ,
           "mar2016" ,"mar2016" ,"mar2016" ,"mar2016" ),
  stringsAsFactors = FALSE
)
scenarios = data.frame(
  RCPs = rep("scenBASE",10  ),
  SPAs = c("0_ref"  , "9_residues", "9_af_csip" ,
           "9_ca_csip","9_dtm_csip","9_phl_csip",
           "9_deforest_csip","9_diverse_csip" ,
           "0_Ref_Max_ZMB","9_notill" ),
  SSPs = rep("SSP2" ,10   ),
  stringsAsFactors = FALSE
)


full_simu_map = read.csv("full_simu_map_biodiv.csv",stringsAsFactors = FALSE)

#GLOBIOM's simulation units; we have to convert to this
geosims <- raster("shape_GLOBIOM/raster_simU/w001001.adf")
proj4string(geosims)<-p4s #apply projection manually
save_geovals <- data.frame(getValues(geosims)) #just the SimU IDs
colnames(save_geovals)<-"SimUID"

geosims_area <- raster::area(geosims)
ttemp = getValues(geosims_area) * 10000
geosims_area = setValues(geosims_area,ttemp)


  
#zambia_map = readRDS("./WMO/adm0_2010_ZMB.rds")
zambia_map = readOGR(dsn = "WMO/AEZ.shp",layer = "AEZ",stringsAsFactors = F)


zambiaExtent = extent(zambia_map)
zambiaExtent = zambiaExtent + c(-.5,.5,-.5,.5)
### plot in user specified extent (around zambia site) ### xmin, xmax, ymin, ymax
zambia_simus = full_simu_map$SimUID[full_simu_map$country == "Zambia"]

years = seq(2010,2100,by=10)
pretty_names = c("Cropland","Short rotation\nplantations","Grassland",
                 "Primary forest","Managed forest","Restored land",
                 "Other land","Urban")

doLU = TRUE
#doLUC = FALSE - currentltly defunct -- need to adapt it

ftarget = paste(source_path,"downscaled_Zambia_SSP2_RCPbase.gdx",sep="")
res_fileLU = rgdx.param(ftarget,
                        "ds_lu_results",squeeze = TRUE, compress = TRUE)


sss =1
for (sss in 1:nrow(scenarios)) {
  RCP = scenarios$RCPs[sss]
  SPA = scenarios$SPAs[sss]
  SSP = scenarios$SSPs[sss]
  cat(RCP,SPA,SSP,"\n")
  CLASSES = 8
  TT = 5
  
  res_file <- droplevels(res_fileLU[res_fileLU$SSPs == SSP &
                           res_fileLU$SPAs == SPA &
                           res_fileLU$RCPs == RCP &
                           res_fileLU$quantiles == "50%",])
  res_file = dcast(res_file, Time + SSPs + SPAs + RCPs + SimUID ~ LC_TYPE_DS,value.var = "ds_lu_results",
                  fill = 0,drop = FALSE)
  res_file = cbind(res_file,
                   RstLnd = 0,
                   Area = rowSums(res_file[,-c(1:5)]))
  
  # LU-Classes
  if (doLU) {
    # reorder classes to match template
    res_file_LU = res_file[,c("SimUID","Time","Area",
                                 "CrpLnd","PltFor","GrsLnd","PriFor","MngFor","RstLnd","NatLnd","UrbLnd")]
    colnames(res_file_LU)[-c(1:3)] = c("cropland","SRP","grassland","priforest","mngforest","restored","other","urban")
    # calculate pct of SimU area
    res_file_LU[,-c(1:3)] = res_file_LU[,-c(1:3)] / rowSums(res_file_LU[,-c(1:3)])
    
    
    ### for the LU case
    res_file_LU = melt(res_file_LU[,1:c(3 + CLASSES)],id.vars = c(1:3),variable.name = "LU")
    # convert to Mha
    #res_file_LU$Area = res_file_LU$Area / 10^6
    #res_file_LU$value = res_file_LU$value / 10^6
    res_file_LU$value[is.na(res_file_LU$value)] = 0
    res_file_LU$value[res_file_LU$value < 10^-6] = 0
    
    res_file_LU = dcast(res_file_LU, SimUID  ~   Time + LU,value.var = "value")
    res_file_LU = res_file_LU[res_file_LU$SimUID %in% zambia_simus,]
    
    oo = match(save_geovals$SimUID,res_file_LU$SimUID)
    
    # Define some colors
    nr_bins = 6
    color_matrix = data.frame(
      cropland = brewer.pal(nr_bins,"Reds"),
      SRP = brewer.pal(nr_bins,"Oranges"),
      grassland = brewer.pal(nr_bins,"Blues"),
      priforest = brewer.pal(nr_bins,"Greens"),
      mngforest = brewer.pal(nr_bins,"Purples"),
      restored = brewer.pal(nr_bins,"Greys"),
      other = brewer.pal(nr_bins,"Greys"),
      urban = brewer.pal(nr_bins,"Greys"),
      stringsAsFactors = F
    )
    
    #### pdf
    fsave = paste(target_path,"Zambia-GLOBIOM-",SSP,"-",SPA,"-",RCP,"-",format(Sys.Date(),"%d%b%y"),"_.pdf",sep="")
    step = 2
    pdf(fsave,paper="a4r",onefile = TRUE)
    for (t in 1:TT) {
      par(mfrow= c(2,3),oma=c(0,0,2,2))
      for (c in 1:CLASSES) {
        if (c %in% c(1:5,7)) {
          temp_map = setValues(geosims,res_file_LU[oo,step])
          temp_map =  crop(temp_map,zambiaExtent)
          
          plot(temp_map,
               breaks = c(seq(0,1,length.out = nr_bins)),
               col = as.character(color_matrix[,c]),
               axes = FALSE,
               main = pretty_names[c])
          plot(zambia_map,add = T)
        }
        step = step + 1
      }
      title(paste("Land use,",years[t],"(in pct. of pixel)"), outer=T,cex = 1.5)
    }
    dev.off()
    
    ### png
    step = 2
    for (t in 1:TT) {
      #png(fsave,width = 11.69,height = 8.27, units = "in",res = 100,type="cairo")
      png(paste("./temp/temp",t-1,".png",sep=""),
          width = 11.69,height = 8.27, units = "in",res = 100,type="cairo")
      par(mfrow= c(2,3),oma=c(0,0,2,2))
      for (c in 1:CLASSES) {
        if (c %in% c(1:5,7)) {
          temp_map = setValues(geosims,res_file_LU[oo,step])
          temp_map =  crop(temp_map,zambiaExtent)
          
          plot(temp_map,
               breaks = c(seq(0,1,length.out = nr_bins)),
               col = as.character(color_matrix[,c]),
               axes = FALSE,
               main = pretty_names[c])
          plot(zambia_map,add = T)
        }
        step = step + 1
      }
      title(paste("Land use,",years[t],"(in pct. of pixel)"), outer=T,cex = 1.5)
      dev.off()
    }
    maps_files <- list.files("./temp/", 
                             pattern = "temp.*.png$", full.names = T) 
    ssp2_animate <- image_read(maps_files)
    ssp2_animate <- image_animate(ssp2_animate, fps = 5)
    #ssp2_animate
    fsave = paste(target_path,"Zambia-GLOBIOM-",SSP,"-",SPA,"-",RCP,"-",years[t],"-",format(Sys.Date(),"%d%b%y"),"_.gif",sep="")
    image_write(ssp2_animate, fsave)
    
    unlink("./temp/*")
  }
}
