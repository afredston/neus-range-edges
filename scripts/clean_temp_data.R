# this analysis uses data from SODA, the Simple Ocean Data Assimilation project: http://www.soda.umd.edu/
# various datasets for SODA are available on that website. some are also available through NOAA's ERDDAP API (the rerddap package), but they are not the newest versions of the models (J. Carton pers. comm.). 
# selecting a SODA model for ecological purposes is challenging because the different sub-models vary in certain assumptions that are difficult for us to judge. I used soda 3.4.2 which is commonly used by meteorologists (J. Carton pers. comm.), but of course the most thorough approach would be to try all of them and see how sensitive the ecological results are to the temperature model. 
# for replication, any time-series dataset of sea bottom temperature (or sea surface temperature, for that matter) from 1968-2017 could be substituted here. 
# the US EEZ shapefile used here can be downloaded from: http://www.marineregions.org/downloads.php

library(here)
library(ncdf4)
library(stars)
library(sf)
library(oceanmap)

# crop to extent of shelf, <400m 

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

bathy.mask <- bathy %>% 
  as("SpatialPolygonsDataFrame") %>% 
  st_as_sf() %>% # retains CRS of bathy.raw
  dplyr::filter(layer <= 400) %>% # get rid of values over 400m deep; justify with Kleisner paper 
  st_intersection(st_union(useez)) # keep only points within the EEZ; crop out lakes, Canada 

plot(bathy.mask) # check that it's a shelf and that lakes are gone 


soda <- stars::read_ncdf(here("data","soda3.4.2_mn_ocean_reg_bottemp.nc")) 

# st_crs(soda) <- bathy.crs 
# don't think this is right--because I think it translates into nonsensical coordinates, below, which causes the error 

soda.neus <- soda %>% 
  st_crop(bathy.mask)
soda.neus <- soda[bathy.mask] # what is wrong with the coordinates of the soda.nc file!? 

time <- ncvar_get(nc, "time") # https://code.mpimet.mpg.de/boards/1/topics/5623
tunits <- ncatt_get(nc,"time","units")
nt <- dim(time)

ncdates = as.Date(nc$dim$time$vals, origin='1980-01-15')


tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))
