library(here)
library(tidyverse)
library(sf)

coastdistdat <- readRDS(here("processed-data","coastdistdat.rds"))
poldat <- readRDS(here("processed-data","poldat.rds"))
eqdat <- readRDS(here("processed-data","eqdat.rds"))

# NOTE that this matches the points of the survey to points on the coast using simple distance minimization. This means that a point that is close to a coastline to the north and slightly further offshore to the west will be assigned a coastal distance further north than if I had just measured the latitude of the observation and matched it to the coast. However, I think this is more accurate because it assigns observations to the closest part of the shelf. 

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

eqdat.stats <- eqdat %>% 
  rowwise() %>% 
  mutate(coastdist = get_length(lon=lon, lat=lat, distdf = coastdistdat)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    assemblage.dist05 = quantile(coastdist, 0.05)
  ) %>% 
  ungroup() %>% 
  group_by(year, latinname) %>% 
  mutate(
    spp.dist10 = quantile(coastdist, 0.10), 
    spp.dist05 = quantile(coastdist, 0.05), 
    spp.dist01 = quantile(coastdist, 0.01), 
    spp.distmin = min(coastdist),
    biomass.sum = sum(biomass.correct), 
    depth.mean = mean(depth),
    depth.mean.wt = weighted.mean(depth, w=biomass.correct, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, latinname, commonname, genus, family, order, class, phylum, numyears, numobs, numobsyear, meanobsyear, assemblage.dist05, spp.dist10, spp.dist05, spp.dist01, spp.distmin, biomass.sum, depth.mean, depth.mean.wt) %>% 
  distinct() %>% 
  group_by(latinname) %>% 
  mutate(overall.biomass = mean(biomass.sum)) %>% 
  ungroup()

write_rds(eqdat.stats, here("processed-data","eqdat.stats.rds"))
write_rds(poldat.stats, here("processed-data","poldat.stats.rds"))

rm(list=ls())
