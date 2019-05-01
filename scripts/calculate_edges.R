library(here)
library(tidyverse)
library(sf)

coastline <- st_read(here("processed-data","coastline.shp"))
coastdistdat <- readRDS(here("processed-data","coastdistdat.rds"))
poldat <- readRDS(here("processed-data","poldat.rds"))
eqdat <- readRDS(here("processed-data","eqdat.rds"))

point.tmp <- st_point(x=c(poldat$lon[100], poldat$lat[100])) 

match.tmp <- st_nearest_points(point.tmp, coastline)

match.tmp <- st_nearest_points(point.tmp, coastline) %>% 
  st_cast("POINT") %>% 
  st_set_crs(st_crs(coastline))

ggplot() + geom_sf(data=coastline) + geom_sf(data=match.tmp[2])

match.tmp.coords <- match.tmp[2] %>% 
  st_coordinates()

abs.diff.x <- abs(coastdistdat$x-match.tmp.coords[1])
abs.diff.y <- abs(coastdistdat$y-match.tmp.coords[2])
abs.diff.xy <- abs.diff.x + abs.diff.y

tmpdf <- coastdistdat %>% 
  mutate(abs.diff.x = abs(x-match.tmp.coords[1]),
         abs.diff.y = abs(y-match.tmp.coords[2]),
         abs.diff.xy = abs.diff.x + abs.diff.y
) %>% 
  filter(abs.diff.xy == min(abs.diff.xy))

get_length <- function(lon, lat, distdf) {
  tmp <- distdf %>% 
    mutate(abs.diff.x2 = abs(x-lon)^2,
           abs.diff.y2 = abs(y-lat)^2,
           abs.diff.xy = sqrt(abs.diff.x2 + abs.diff.y2
    )) %>% 
    filter(abs.diff.xy == min(abs.diff.xy)) %>% 
    dplyr::select(lengthfromhere) %>% 
    pull()
  return(tmp)
}

poldat.stats <- poldat %>% 
  rowwise() %>% 
  mutate(coastdist = get_length(lon=lon, lat=lat, distdf = coastdistdat)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    assemblage.dist95 = quantile(coastdist, 0.95)
  ) %>% 
  ungroup() %>% 
  group_by(year, latinname) %>% 
  mutate(
    spp.dist90 = quantile(coastdist, 0.90), 
    spp.dist95 = quantile(coastdist, 0.95), 
    spp.dist99 = quantile(coastdist, 0.99), 
    spp.distmax = max(coastdist),
    biomass.sum = sum(biomass.correct), 
    depth.mean = mean(depth),
    depth.mean.wt = weighted.mean(depth, w=biomass.correct, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, latinname, commonname, genus, family, order, class, phylum, numyears, numobs, numobsyear, meanobsyear, assemblage.dist95, spp.dist90, spp.dist95, spp.dist99, spp.distmax, biomass.sum, depth.mean, depth.mean.wt) %>% 
  distinct() %>% 
  group_by(latinname) %>% 
  mutate(overall.biomass = mean(biomass.sum)) %>% 
  ungroup()

# HOW SHOULD POINTS BE MATCHED? If a point is offshore on the shelf, should it just be snapped to the coast to minimize distance, or should I just take the latitude of that point on the coastline? I think it's the former, since if a point has coastline nearby to the north and far away to the west, it seems more likely to be "off the shelf" from the northward part than the westward part. 

coastdistsf <- coastdistdat %>% 
  st_as_sf(coords=c("x","y"), crs=st_crs(coastline))

