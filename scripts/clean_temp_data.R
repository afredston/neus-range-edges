# this analysis uses data from SODA, the Simple Ocean Data Assimilation project: http://www.soda.umd.edu/
# various datasets for SODA are available on that website. some are also available through NOAA's ERDDAP API (the rerddap package), but they are not the newest versions of the models (J. Carton pers. comm.). 
# selecting a SODA model for ecological purposes is challenging because the different sub-models vary in certain assumptions that are difficult for us to judge. I used soda 3.4.2 which is commonly used by meteorologists (J. Carton pers. comm.)
# the US EEZ shapefile used here can be downloaded from: http://www.marineregions.org/downloads.php

library(here)
library(raster)
library(sf)
library(oceanmap)
library(tidyverse)

#############
# READ IN TEMPERATURE DATA 
#############

soda_in <- raster::stack(here("data", "soda3.4.2_mn_ocean_reg_bottemp.nc")) %>% 
  rotate() # Rotate it to be -180 to 180

# reproject
soda_lonlat <- projectRaster(soda_in, crs = "+proj=longlat +ellps=WGS84 +no_defs")

#############
# CREATE SHELF MASK 
#############

lonrange <- c(-77, -66)
latrange <- c(35, 45)

bathy <- get.bathy(lon = lonrange, lat = latrange, visualize = F, res = 15) 

bathy.crs <- bathy %>% 
  as("SpatialPolygonsDataFrame") %>% 
  st_as_sf() %>% 
  st_crs()

eezs <- st_read(here("data/World_EEZ_v10_20180221","eez_v10.shp")) 

useez <- eezs %>% 
  dplyr::filter(Sovereign1 == "United States") %>% 
  st_transform(crs=bathy.crs) # reproject to match bathymetry 
#slow

#also slow
bathy.mask <- bathy %>% 
  as("SpatialPolygonsDataFrame") %>% 
  st_as_sf() %>% # retains CRS of bathy.raw
  dplyr::filter(layer <= 300) %>% # get rid of values over 300m deep
  st_intersection(st_union(useez)) # keep only points within the EEZ; crop out lakes, Canada 

# plot(bathy.mask) # check that it's a shelf and that lakes are gone 

#############
# CROP AND SUMMARIZE TEMPERATURE DATA
#############

# crop to extent of shelf <400m 
soda_neus <- soda_lonlat %>% 
  raster::mask(bathy.mask) %>% 
  raster::crop(extent(bathy.mask))

# create summary data 
soda_df <- raster::as.data.frame(soda_neus, xy=TRUE, long=TRUE) %>% 
  mutate(
         time = lubridate::ymd(str_sub(str_replace(layer, "X", ""), 1, 10)),
         year = lubridate::year(time),
         month = lubridate::month(time)
         ) %>% 
  dplyr::select(-layer) %>% 
  rename(btemp = value) %>% 
  filter(!is.na(btemp)) %>% 
  mutate(year_measured = ifelse(month %in% c(1,2), year-1, year),
         year_match = year_measured + 1) 
# the trawl survey data is from spring (starts in april), and the coldest temperatures are often found in early spring
# so it's not appropriate to compare a survey from spring in one year to either all temperatures in that calendar year or last year
# rather, we're "redefining" the year to run from march to february, and then comparing edge position to the previous 12 months of temperaturue data 

soda_stats <- soda_df %>% 
  group_by(year_measured) %>% 
  mutate(
    btemp.year.mean = mean(btemp),
    btemp.year.max = max(btemp),
    btemp.year.min = min(btemp)
  ) %>% 
  ungroup() %>% 
  group_by(year_measured, y) %>% 
  mutate(
    btemp.lat.year.mean = mean(btemp),
    btemp.lat.year.max = max(btemp),
    btemp.lat.year.min = min(btemp) 
    ) %>% 
  ungroup() 

write_rds(soda_stats, here("processed-data","soda_stats.rds"))
rm(list=ls())
