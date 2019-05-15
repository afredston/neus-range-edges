# this analysis uses data from SODA, the Simple Ocean Data Assimilation project: http://www.soda.umd.edu/
# various datasets for SODA are available on that website. some are also available through NOAA's ERDDAP API (the rerddap package), but they are not the newest versions of the models (J. Carton pers. comm.). 
# selecting a SODA model for ecological purposes is challenging because the different sub-models vary in certain assumptions that are difficult for us to judge. I used soda 3.4.2 which is commonly used by meteorologists (J. Carton pers. comm.)
# the US EEZ shapefile used here can be downloaded from: http://www.marineregions.org/downloads.php

library(here)
library(tidyverse)
library(raster)
library(sf)
library(oceanmap)
library(data.table)
source(here("functions","sfc_as_cols.R"))


# create shelf mask
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

# load raw temperature datasets, crop to extent of shelf, change back into dataframes 
hadisst.neus <- read_rds(here("processed-data","hadisst_raw.rds")) %>% 
  filter(!is.na(sst)) %>% # do early to make the object smaller
  st_as_sf(coords=c("lon","lat"), crs = bathy.crs) %>% 
  st_join(bathy.mask, left=FALSE) %>% 
  rename(bathymetry = layer) %>% 
  sfc_as_cols() %>% 
  as.data.table() %>% 
  mutate(
    time = lubridate::ymd(str_sub(time, 1, 10)),
    year = lubridate::year(time),
    month = lubridate::month(time)
  ) %>% 
  mutate(year_measured = ifelse(month %in% c(1,2), year-1, year),
         year_match = year_measured + 1)
# the trawl survey data is from spring (starts in april), and the coldest temperatures are often found in early spring
# so it's not appropriate to compare a survey from spring in one year to either all temperatures in that calendar year or last year
# rather, we're "redefining" the year to run from march to february, and then comparing edge position to the previous 12 months of temperaturue data 

oisst.neus <- read_rds(here("processed-data","oisst_raw.rds")) %>% 
  filter(!is.na(sst)) %>% # do early to make the object smaller
  st_as_sf(coords=c("lon","lat"), crs = bathy.crs) %>% 
  st_join(bathy.mask, left=FALSE) %>% 
  rename(bathymetry = layer) %>% 
  sfc_as_cols() %>% 
  as.data.table() %>% 
  mutate(
    time = lubridate::ymd(str_sub(time, 1, 10)),
    year = lubridate::year(time),
    month = lubridate::month(time)
  ) %>% 
  mutate(year_measured = ifelse(month %in% c(1,2), year-1, year),
         year_match = year_measured + 1)

# different because this one is a .nc file
soda.neus <- raster::stack(here("data", "soda3.4.2_mn_ocean_reg_bottemp.nc")) %>% 
  rotate() %>%  # Rotate it to be -180 to 180
  projectRaster(crs = "+proj=longlat +ellps=WGS84 +no_defs") %>%
  raster::mask(bathy.mask) %>% 
  raster::crop(extent(bathy.mask)) %>% 
  raster::as.data.frame(xy=TRUE, long=TRUE) %>% 
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

# save files--they are slow to generate!
write_rds(oisst.neus, here("processed-data","oisst_neus.rds"))
write_rds(hadisst.neus, here("processed-data","hadisst_neus.rds"))
write_rds(soda.neus, here("processed-data","soda_neus.rds"))

oisst.stats <- oisst.neus %>% 
  dplyr::select(-altitude, -bathymetry) %>% 
  group_by(year, month, x, y) %>% 
  mutate(
    cell.month.mean = mean(sst)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, month, year_measured, year_match, x, y, cell.month.mean) %>% # trim down to resolution of other datasets 
  distinct() %>% 
  group_by(year_measured) %>% 
  mutate(
    year.month.mean = mean(cell.month.mean),
    year.month.sd = sd(cell.month.mean),
    year.month.max = max(cell.month.mean),
    year.month.min = min(cell.month.mean)
  ) %>% 
  ungroup() %>% 
  group_by(year_measured, y) %>% 
  mutate(
    lat.year.month.mean = mean(cell.month.mean),
    lat.year.month.sd = sd(cell.month.mean),
    lat.year.month.max = max(cell.month.mean),
    lat.year.month.min = min(cell.month.mean) 
  ) %>% 
  ungroup() %>% 
  dplyr::select(y, year_measured, year_match, year.month.mean, year.month.max, year.month.sd, year.month.min, lat.year.month.mean, lat.year.month.sd, lat.year.month.max, lat.year.month.min) %>% 
  distinct()

hadisst.stats <- hadisst.neus %>% 
  group_by(year_measured) %>% 
  mutate(
    year.month.mean = mean(sst),
    year.month.sd = sd(sst),
    year.month.max = max(sst),
    year.month.min = min(sst)
  ) %>% 
  ungroup() %>% 
  group_by(year_measured, y) %>% 
  mutate(
    lat.year.month.mean = mean(sst),
    lat.year.month.sd = sd(sst),
    lat.year.month.max = max(sst),
    lat.year.month.min = min(sst) 
  ) %>% 
  ungroup() %>% 
  dplyr::select(y, year_measured, year_match, year.month.mean, year.month.max, year.month.sd, year.month.min, lat.year.month.mean, lat.year.month.sd, lat.year.month.max, lat.year.month.min) %>% 
  distinct()

soda.stats <- soda.neus %>% 
  group_by(year_measured) %>% 
  mutate(
    year.month.mean = mean(btemp),
    year.month.sd = sd(btemp),
    year.month.max = max(btemp),
    year.month.min = min(btemp)
  ) %>% 
  ungroup() %>% 
  group_by(year_measured, y) %>% 
  mutate(
    lat.year.month.mean = mean(btemp),
    lat.year.month.sd = sd(btemp),
    lat.year.month.max = max(btemp),
    lat.year.month.min = min(btemp) 
  ) %>% 
  ungroup() %>% 
  dplyr::select(y, year_measured, year_match, year.month.mean, year.month.max, year.month.sd, year.month.min, lat.year.month.mean, lat.year.month.sd, lat.year.month.max, lat.year.month.min) %>% 
  distinct()

write_rds(oisst.stats, here("processed-data","oisst_stats.rds"))
write_rds(hadisst.stats, here("processed-data","hadisst_stats.rds"))
write_rds(soda.stats, here("processed-data","soda_stats.rds"))
