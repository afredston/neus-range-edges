
# note that the actual analysis for this uses sea bottom temperature
library(data.table)
library(tidyverse)
library(here)
library(sf)
library(oceanmap)
library(rnaturalearth)
library(RColorBrewer)

lonrange <- c(-78, -60)
latrange <- c(34.5, 48)

usoutline <- rnaturalearth::ne_states("united states of america", returnclass = "sf") %>% 
  # st_union() %>% #just to combine all polygons into a large one
  st_sf()

smoothline <- st_read(here("processed-data","coastline.shp"))
coastdistdat <- read_rds(here("processed-data","coastdistdat.rds"))



coastdistrefs <- coastdistdat %>% 
  filter(seglength > 0,
         seglength < 100000) %>% # get rid of weird points
  mutate(coastdist = lengthfromhere/1000,
         coastdistround = round(coastdist, digits = -2), # round to nearest hundred 
         coastdistdiff = abs(coastdist-coastdistround)) %>% 
  group_by(coastdistround) %>% 
  filter(coastdistdiff == min(coastdistdiff),
         coastdistround <= 1400) %>% 
  ungroup() %>% 
  dplyr::select(x, y, coastdistround)
  
  
neusmap <- ggplot() + 
  geom_sf(data=usoutline, color="#999999") +
  geom_sf(data=smoothline, size=2, color="#0072B2") + 
  geom_point(data=coastdistrefs, aes(x=x, y=y), color="black") +
  geom_text(data=coastdistrefs, aes(x=x, y=y, label=coastdistround),hjust=0, nudge_x = 0.5, fontface="bold", size=3) +
  scale_x_continuous(limits = lonrange, expand = c(0, 0)) +
  scale_y_continuous(limits = latrange, expand = c(0, 0)) +
  theme(legend.position = "none", plot.margin = margin(t = 15, r = 15, b = 0, l = 0)) +
  NULL
neusmap





sst.raw <- na.omit(readRDS(here("processed-data","sstraw.rds")))

# crop to shelf before proceeding with calculations 

bathy.raw <- get.bathy(lon = lonrange, lat = latrange, visualize = F, res = 20) 

orig.bathy.crs <- bathy.raw %>% 
  as("SpatialPolygonsDataFrame") %>% 
  st_as_sf() %>% 
  st_crs()

# tried using mregions but couldn't figure out the key value that would get US EEZs
# downloading world EEZ v10 
# http://www.marineregions.org/downloads.php

eezdat <- st_read(here("data/World_EEZ_v10_20180221","eez_v10.shp")) 

useez <- eezdat %>% 
  filter(Sovereign1 == "United States") %>% 
  st_transform(crs=orig.bathy.crs) # reproject to match bathymetry 

#slow
bathy.sf <- bathy.raw %>% 
  as("SpatialPolygonsDataFrame") %>% 
  st_as_sf() %>% # retains CRS of bathy.raw
  filter(layer <= 400) %>% # get rid of values over 400m deep; justify with Kleisner paper 
  st_intersection(st_union(useez)) # keep only points within the EEZ; crop out lakes, Canada 

# crop sst to shelf 

sst.sf <- sst.raw %>% 
  st_as_sf(coords=c("lon","lat"), crs = orig.bathy.crs) # sf-ify the dataframe 

#slow
sst.crop <- st_join(sst.sf, bathy.sf, left=FALSE) %>% # keep only SST points that fall within the cropped bathymetry layer
  rename(bathymetry = layer) %>% 
  sfc_as_cols() %>% 
  rename(lat = y, lon = x) %>% 
  as.data.table() # note: can remove this if I decide later that I want sst.stats to stay a sf object 


sst.crop$year = lubridate::year(sst.crop$time)
sst.crop$month = lubridate::month(sst.crop$time)
sst.crop$day = lubridate::day(sst.crop$time)

sstdat <- sst.crop %>% 
  mutate(
    year = lubridate::year(time), 
    month = lubridate::month(time),
    day = lubridate::day(time)
  ) %>% 
  group_by(lat, year) %>% 
  mutate(mean.lat.sst = mean(sst)) %>% 
  ungroup() %>% 
  dplyr::select(mean.lat.sst, year, lat) %>% 
  distinct()

temprange = seq(min(as.integer(sstdat$mean.lat.sst))+1, max(as.integer(sstdat$mean.lat.sst))-1, 1) # trim off the extreme values

isothermdf <- NULL

for(i in min(sstdat$year):max(sstdat$year)) {
  for(j in min(temprange):max(temprange)) {
    sstdatyear = sstdat[sstdat$year==paste0(i),] # get sst values for only one year 
    sstmatch = sapply(j, function(x) sstdatyear$mean.lat.sst[order(abs(x-sstdatyear$mean.lat.sst))][1]) # get the value in mean.lat.sst for year i that minimizes the difference from j, the rounded temperature value 
    latmatch = sstdatyear[sstdatyear$mean.lat.sst==sstmatch,]$lat
    year = i
    sstref = j
    out = as.data.frame(cbind(year, sstref, sstmatch, latmatch))
    isothermdf = rbind(isothermdf, out)
  }
}

gg.sst.iso <- sstdat %>% 
  ggplot() +
  geom_raster(aes(x=year, y=lat, fill=mean.lat.sst)) + 
  geom_line(data=isothermdf, aes(x=year, y=latmatch, group=sstref)) +
  scale_fill_gradientn(colors=c("blue3","darkturquoise", "gold", "orangered", "red3")) + 
  NULL
gg.sst.iso
# this isn't going to work unless I get higher-resolution SST data from the past 

gg.sst <- sstdat %>% 
  ggplot() +
  geom_raster(aes(x=year, y=lat, fill=mean.lat.sst)) + 
  scale_fill_gradientn(colors=c("blue3","darkturquoise", "gold", "orangered", "red3")) + 
  NULL
gg.sst

isothermdf.2c <- isothermdf %>%
  filter(sstref %% 2 == 0)

gg.sst.iso2 <- sstdat %>% 
  ggplot() +
  geom_raster(aes(x=year, y=lat, fill=mean.lat.sst)) + 
  scale_fill_gradientn(colors=c("blue3","darkturquoise", "gold", "orangered", "red3")) + 
  geom_line(data=isothermdf.2c, aes(x=year, y=latmatch, group=sstref)) +
  NULL
gg.sst.iso2
# this isn't going to work unless I get higher-resolution SST data from the past 