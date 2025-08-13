
# This script is used to extract 90th percentiles of TWI of each catchment of DOC_TOC_DIN_SRP_daily_median_sd_subbasins.shp shapefile


library(exactextractr)
#library("rgdal")--> not anymore available, retired, deprecated
library(terra)
library(sf)


# set working directory:
setwd("../../input_data/geodata")

# read tif file TWI_global.tif: use terra::rast to read in tif file
twi_global<-terra::rast("TWI_global.tif")



basins<-read_sf("../../output_data/CNP_data_catchments/DOC_TOC_DIN_SRP_daily_median_sd_subbasins.shp")



q90_twi<-exact_extract(twi_global, basins, fun = 'quantile', quantiles= 0.90)
max(q90_twi)


twi90_df<-data.frame(HYBAS_ID = as.numeric(basins$HYBAS_ID), 
                     twi90 = as.numeric(q90_twi))

write.csv(twi90_df, file.path("..","..","output_data","TWI_QGIS_and_TWI90_extraction","TWI90_DOC_TOC_DIN_SRP_subbasins.csv"))

summary(twi90_df)



max_twi<-exact_extract(twi_global, basins, fun = 'max')




hist(q90_twi)
q90_twi[1:11]

