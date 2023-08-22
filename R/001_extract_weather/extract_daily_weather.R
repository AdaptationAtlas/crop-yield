#Extract daily weather data based on locations

#load libraries
library(terra)
library(tidyverse)
library(furrr)

#base directory
root <- "/home/jovyan/common_data"

#input directories
pr_his <- paste0(root, "/chirps_wrld")
sr_his <- paste0(root, "/ecmwf_agera5/solar_radiation_flux")
tn_his <- paste0(root, "/chirts/Tmin")
tx_his <- paste0(root, "/chirts/Tmax")

pr_fut <- paste0(root, "/chirps_cmip6_africa/Prec_") #, gcm, '_', ssp, '_', prd
sr_fut <- paste0(root, "/ecmwf_agera5_cmip6_africa/solar_radiation_flux_")
tn_fut <- paste0(root, "/chirts_cmip6_africa/Tmin_")
tx_fut <- paste0(root, "/chirts_cmip6_africa/Tmax_")

#output directory
out_dir <- "~/work/atlas_yield/daily_gcm_extract" #paste0(root, "/atlas_yield")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#weather station locations
ws_tab <- read.csv("~/Repositories/crop-yield/R/001_extract_weather/stations_75pc.csv", header=TRUE)

#historical years
his_yrs <- 1995:2014

#first, extract historical data
#1. dates
his_dates <- seq(from = as.Date(paste0(min(his_yrs),'-01-01')),
                 to   = as.Date(paste0(max(his_yrs),'-12-31')),
                 by   = 'day')

#2. extract variables
for (varname in c("pr", "tmax", "tmin", "srad")) {
  ofile <- paste0(out_dir, "/historical/", varname, "_historical.csv")
  if (!file.exists(dirname(ofile))) {dir.create(dirname(ofile))}
  if (!file.exists(ofile)) {
    data_all <- list()
    for (yr in his_yrs) {
      cat("...extracting variable=", varname, "/ year=", yr, "\n")
      
      #filter year from dates
      yrdates <- data.frame(dates=his_dates) %>%
        dplyr::mutate(year=year(his_dates)) %>%
        dplyr::filter(year == yr)
      
      #input directory
      in_dir <- ifelse(varname == "pr", paste0(pr_his, "/chirps-v2.0."), 
                       ifelse(varname == "tmax", paste0(tx_his, "/", yr, "/Tmax."), 
                              ifelse(varname == "tmin", paste0(tn_his, "/", yr, "/Tmin."), 
                                     paste0(sr_his, "/Solar-Radiation-Flux_C3S-glob-agric_AgERA5_"))))
      
      #data files
      if (varname %in% c("pr", "tmax", "tmin")) {
        rs_data <- paste0(in_dir, gsub(pattern='-', replacement='.', x=yrdates$dates, fixed=T), '.tif')
      } else {
        rs_data <- paste0(in_dir, gsub(pattern='-', replacement='', x=yrdates$dates, fixed=T), '_final-v1.0.nc')
      }
      
      #load raster data
      rs_data <- rs_data[file.exists(rs_data)]
      rs_data <- terra::rast(rs_data)
      rs_tab <- terra::extract(rs_data, as.matrix(ws_tab[,c("lon","lat")]))
      
      #convert srad from J/m2/d to MJ/m2/d (i.e., crop model units)
      #note: conversion not needed for future data
      if (varname == "srad") {rs_tab <- rs_tab[,] * 1e-6}
      
      #colnames
      names(rs_tab) <- paste0("doy.",1:ncol(rs_tab))
      rs_tab <- rs_tab %>%
        dplyr::bind_cols(ws_tab) %>%
        dplyr::mutate(year=yr) %>%
        dplyr::relocate(station_id:year, .before=1)
      if (terra::nlyr(rs_data) == 365) {rs_tab <- rs_tab %>% dplyr::mutate(doy.366=NA)}
      data_all[[which(his_yrs %in% yr)]] <- rs_tab
    }
    data_all <- do.call("rbind", data_all)
    write.csv(data_all, ofile, row.names=FALSE)
  }
}


#second, extract future scenario data
#1. define period, ssp, and gcm
gcms <- c('ACCESS-ESM1-5','EC-Earth3','INM-CM5-0','MPI-ESM1-2-HR','MRI-ESM2-0')
ssps <- c('ssp126','ssp245','ssp370','ssp585')
prds <- c('2021_2040','2041_2060','2061_2080','2081_2100')
stp <- base::expand.grid(gcms,ssps,prds) %>% base::as.data.frame()
names(stp) <- c('gcm','ssp','prd'); rm(gcms, ssps, prds)
stp <- stp %>%
  dplyr::arrange(gcm,ssp,prd) %>%
  base::as.data.frame()

#function to extract the data
extract_daily_data <- function(gcm, ssp, prd) {
  prd_num <- as.numeric(unlist(strsplit(x = prd, split = '_')))
  fut_yrs <- prd_num[1]:prd_num[2]
  
  #1. dates
  fut_dates <- seq(from = as.Date(paste0(min(fut_yrs),'-01-01')),
                   to   = as.Date(paste0(max(fut_yrs),'-12-31')),
                   by   = 'day')
  
  #2. extract variables
  for (varname in c("pr", "tmax", "tmin", "srad")) {
    ofile <- paste0(out_dir, "/", gcm, "/", varname, "_", gcm, "_", ssp, "_", prd, ".csv")
    if (!file.exists(dirname(ofile))) {dir.create(dirname(ofile))}
    if (!file.exists(ofile)) {
      data_all <- list()
      for (yr in fut_yrs) {
        cat("...extracting for ssp=", ssp, "/ gcm=", gcm, "/ variable=", varname, "/ year=", yr, "\n")
        
        #filter year from dates
        yrdates <- data.frame(dates=fut_dates) %>%
          dplyr::mutate(year=year(fut_dates)) %>%
          dplyr::filter(year == yr)
        
        #input directory, append gcm_ssp_prd
        in_dir <- ifelse(varname == "pr", paste0(pr_fut, gcm, "_", ssp, "_", prd, "/chirps-v2.0."), 
                         ifelse(varname == "tmax", paste0(tx_fut, gcm, "_", ssp, "_", prd, "/", yr, "/Tmax."), 
                                ifelse(varname == "tmin", paste0(tn_fut, gcm, "_", ssp, "_", prd, "/", yr, "/Tmin."), 
                                       paste0(sr_fut, gcm, "_", ssp, "_", prd, "/Solar-Radiation-Flux_C3S-glob-agric_AgERA5_"))))
        
        #data files
        if (varname %in% c("pr", "tmax", "tmin")) {
          rs_data <- paste0(in_dir, gsub(pattern='-', replacement='.', x=yrdates$dates, fixed=T), '.tif')
        } else {
          rs_data <- paste0(in_dir, gsub(pattern='-', replacement='', x=yrdates$dates, fixed=T), '_final-v1.0.nc')
        }
        
        #load raster data
        rs_data <- rs_data[file.exists(rs_data)]
        rs_data <- terra::rast(rs_data)
        rs_tab <- terra::extract(rs_data, as.matrix(ws_tab[,c("lon","lat")]))
        
        names(rs_tab) <- paste0("doy.",1:ncol(rs_tab))
        rs_tab <- rs_tab %>%
          dplyr::bind_cols(ws_tab) %>%
          dplyr::mutate(year=yr) %>%
          dplyr::relocate(station_id:year, .before=1)
        if (terra::nlyr(rs_data) == 365) {rs_tab <- rs_tab %>% dplyr::mutate(doy.366=NA)}
        data_all[[which(fut_yrs %in% yr)]] <- rs_tab
      }
      data_all <- do.call("rbind", data_all)
      write.csv(data_all, ofile, row.names=FALSE)
    }
  }
}

#apply function for future data extract
plan(multicore, workers = 14)
1:nrow(stp) %>%
  furrr::future_map(.f = function(i){extract_daily_data(gcm = paste0(stp$gcm[i]),
                                                        ssp = paste0(stp$ssp[i]),
                                                        prd = paste0(stp$prd[i]))
    gc(verbose=F, full=T, reset=T)
  })
plan(sequential); gc(reset = T)

