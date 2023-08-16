#Extract daily weather data based on locations

#load libraries
library(terra)
library(tidyverse)

#directories
root <- "/home/jovyan/common_data"
pr_his <- paste0(root, "/chirps_wrld")
sr_his <- paste0(root, "/ecmwf_agera5/solar_radiation_flux")
tn_his <- paste0(root, "/chirts/Tmin")
tx_his <- paste0(root, "/chirts/Tmax")

pr_fut <- paste0(root, "/chirps_cmip6_africa/Prec_") #, gcm, '_', ssp, '_', prd
sr_fut <- paste0(root, "/ecmwf_agera5_cmip6_africa/solar_radiation_flux_")
tn_fut <- paste0(root, "/chirts_cmip6_africa/Tmin_")
tx_fut <- paste0(root, "/chirts_cmip6_africa/Tmax_")

#weather station locations
ws_tab <- read.csv("~/Repositories/crop-yield/R/001_extract_weather/stations_75pc.csv", header=TRUE)

#years
yrs <- 1995:2014 #2021:2040


