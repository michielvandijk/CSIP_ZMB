#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Set country
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("countrycode")

### SET COUNTRY
iso3c_sel <- "ZMB"
country_sel <- countrycode("ZMB", "iso3c", "country.name")
