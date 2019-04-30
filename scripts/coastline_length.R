library(here)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(smoothr)
library(raster)

# doing this with a high-resolution map of the coastline gives too much resolution around the Chesapeake, Long Island, etc. I want a really coarse shape (similar to the 10m isobath approach)

xmin=-78
xmax=-66
ymin=35
ymax=45

bbox <- extent(xmin, xmax, ymin, ymax)

usamap <- rnaturalearth::ne_countries(scale = "small", country = "united states of america", returnclass = "sf")[1] %>% 
  st_cast("MULTILINESTRING")

bbox2 <- st_set_crs(st_as_sf(as(raster::extent(-78, -74, 42, 45), "SpatialPolygons")), st_crs(usamap))

neusmap <- usamap %>% 
  st_crop(bbox) %>% 
  st_difference(bbox2) # get rid of extra non coastal line 

smoothmap <- neusmap %>% 
  smoothr::smooth(method="ksmooth", smoothness=8)
# ggplot() + geom_sf(data=smoothmap)
# smoother was applied incrementally more until the Chesapeake went away 
# https://cran.r-project.org/web/packages/smoothr/vignettes/smoothr.html

st_length(smoothmap)

smoothgeom <- smoothmap %>% 
  as("Spatial") %>% 
  geom()

geomdists <- pointDistance(smoothgeom[-nrow(smoothgeom), c("x", "y")], smoothgeom[-1, c("x", "y")], lonlat=TRUE)
coastdistdat <- data.frame(smoothgeom[, c('x','y')], seglength=c(0, geomdists))
coastdistdat$lengthfromhere <- rev(cumsum(rev(coastdistdat[,"seglength"])))
# first row should match st_length(smoothmap)

write_rds(coastdistdat, here("processed-data","coastdistdat.rds"))
rm(list=ls())