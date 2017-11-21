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

# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  dataPath <- "P:/globiom/Projects/CSIP_ZMB"}

if(Sys.info()["user"] == "vandijkm") {
  modelPath <- "P:/globiom/Projects/ISWEL/Zambezi/gdx"}

if(Sys.info()["user"] == "vandijkm") {
  GAMSPath <- "C:\\GAMS\\win64\\24.4"}


# Amanda IIASA:
if(Sys.info()["user"] == "palazzo") {
  dataPath <- "P:/globiom/Projects/CSIP_ZMB"}

if(Sys.info()["user"] == "vandijkm") {
  modelPath <- "P:/globiom/Projects/ISWEL/Zambezi/gdx"}

if(Sys.info()["user"] == "vandijkm") {
  GAMSPath <- "C:\\GAMS\\win64\\24.4"}


