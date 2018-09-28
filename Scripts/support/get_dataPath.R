#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  get data path 
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# Use this file to set your path to the data
# check your computer username using
# Sys.info()["user"] and use this in the if
# statement. Then add your dataPath within 
# the {} brackets

# GLOBIOM File
globiom_file <- "output_CSIP_ZMB_all_23sept"

# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  projectPath <- "P:/Globiom/Projects/CSIP_ZMB"}

if(Sys.info()["user"] == "vandijkm") {
  dataPath <- "P:/Globiom/crop_map"}

if(Sys.info()["user"] == "vandijkm") {
  GLOBIOMPath <- "P:/Globiom"}

if(Sys.info()["user"] == "vandijkm") {
  GAMSPath <- "C:\\GAMS\\win64\\25.0"}


# Amanda IIASA:
if(Sys.info()["user"] == "palazzo") {
  projectPath <- "P:/Globiom/Projects/CSIP_ZMB"}

if(Sys.info()["user"] == "palazzo") {
  dataPath <- "P:/Globiom/crop_map"}

if(Sys.info()["user"] == "palazzo") {
  GAMSPath <- "C:\\GAMS\\win64\\24.4"}

if(Sys.info()["user"] == "palazzo") {
  GLOBIOMPath <- "P:/Globiom"}
