# NEED TO REDO THIS SLIGHTLY AFTER UPDATING THE TEMPERATURE DATASETS 

# calculate species-specific isotherm displacement over time 

library(here)
library(tidyverse)
library(data.table)

poldat.stats <- readRDS(here("processed-data","poldat.stats.rds"))
eqdat.stats <- readRDS(here("processed-data","eqdat.stats.rds")) 
btemp.stats <- readRDS(here("processed-data","soda_stats.rds")) 
firstyear.tempdata <- 1980 

# calculate temperature isotherms occupied by each species based on their first 3 years of occurrence (for which we have temperature data!)

poldat.stats.y1 <- poldat.stats %>% 
  filter(year>firstyear.tempdata) %>% 
  group_by(commonname) %>% 
  arrange(year) %>% 
  filter(year==year[1]) %>% # first year when species was observed
  left_join(btemp.stats, by=c('year' = 'year_match', 'spp.lat95round'='y')) %>% 
  mutate(
    spp.maxT.sample = btemp.lat.year.max, 
    spp.minT.sample = btemp.lat.year.min, 
    spp.meanT.sample = btemp.lat.year.mean
  ) %>% 
  dplyr::select(commonname, spp.maxT.sample, spp.minT.sample, spp.meanT.sample) %>%
  distinct()

poldat.stats.y2 <- poldat.stats %>% 
  filter(year>firstyear.tempdata) %>% 
  group_by(commonname) %>% 
  arrange(year) %>% 
  filter(year==year[2]) %>% 
  left_join(btemp.stats, by=c('year' = 'year_match', 'spp.lat95round'='y')) %>% 
  mutate(
    spp.maxT.sample = btemp.lat.year.max, 
    spp.minT.sample = btemp.lat.year.min, 
    spp.meanT.sample = btemp.lat.year.mean
  ) %>% 
  dplyr::select(commonname, spp.maxT.sample, spp.minT.sample, spp.meanT.sample) %>%
  distinct()

poldat.stats.y3 <- poldat.stats %>% 
  filter(year>firstyear.tempdata) %>% 
  group_by(commonname) %>% 
  arrange(year) %>% 
  filter(year==year[3]) %>% 
  left_join(btemp.stats, by=c('year' = 'year_match', 'spp.lat95round'='y')) %>% 
  mutate(
    spp.maxT.sample = btemp.lat.year.max, 
    spp.minT.sample = btemp.lat.year.min, 
    spp.meanT.sample = btemp.lat.year.mean
  ) %>% 
  dplyr::select(commonname, spp.maxT.sample, spp.minT.sample, spp.meanT.sample) %>%
  distinct()

poldat.iso.baseline <- rbind(poldat.stats.y1, poldat.stats.y2, poldat.stats.y3) %>% 
  group_by(commonname) %>% 
  mutate(spp.maxT.baseline = mean(spp.maxT.sample),
         spp.meanT.baseline = mean(spp.meanT.sample),
         spp.minT.baseline = mean(spp.minT.sample)) %>% 
  ungroup() %>% 
  dplyr::select(commonname, spp.maxT.baseline, spp.minT.baseline, spp.meanT.baseline) %>% 
  distinct()

eqdat.stats.y1 <- eqdat.stats %>% 
  filter(year>firstyear.tempdata) %>% 
  group_by(commonname) %>% 
  arrange(year) %>% 
  filter(year==year[1]) %>% # first year when species was observed
  left_join(btemp.stats, by=c('year' = 'year_match', 'spp.lat05round'='y')) %>% 
  mutate(
    spp.maxT.sample = btemp.lat.year.max, 
    spp.minT.sample = btemp.lat.year.min, 
    spp.meanT.sample = btemp.lat.year.mean
  ) %>% 
  dplyr::select(commonname, spp.maxT.sample, spp.minT.sample, spp.meanT.sample) %>%
  distinct()

eqdat.stats.y2 <- eqdat.stats %>% 
  filter(year>firstyear.tempdata) %>% 
  group_by(commonname) %>% 
  arrange(year) %>% 
  filter(year==year[2]) %>% 
  left_join(btemp.stats, by=c('year' = 'year_match', 'spp.lat05round'='y')) %>% 
  mutate(
    spp.maxT.sample = btemp.lat.year.max, 
    spp.minT.sample = btemp.lat.year.min, 
    spp.meanT.sample = btemp.lat.year.mean
  ) %>% 
  dplyr::select(commonname, spp.maxT.sample, spp.minT.sample, spp.meanT.sample) %>%
  distinct()

eqdat.stats.y3 <- eqdat.stats %>% 
  filter(year>firstyear.tempdata) %>% 
  group_by(commonname) %>% 
  arrange(year) %>% 
  filter(year==year[3]) %>% 
  left_join(btemp.stats, by=c('year' = 'year_match', 'spp.lat05round'='y')) %>% 
  mutate(
    spp.maxT.sample = btemp.lat.year.max, 
    spp.minT.sample = btemp.lat.year.min, 
    spp.meanT.sample = btemp.lat.year.mean
  ) %>% 
  dplyr::select(commonname, spp.maxT.sample, spp.minT.sample, spp.meanT.sample) %>%
  distinct()

eqdat.iso.baseline <- rbind(eqdat.stats.y1, eqdat.stats.y2, eqdat.stats.y3) %>% 
  group_by(commonname) %>% 
  mutate(spp.maxT.baseline = mean(spp.maxT.sample),
         spp.meanT.baseline = mean(spp.meanT.sample),
         spp.minT.baseline = mean(spp.minT.sample)) %>% 
  ungroup() %>% 
  dplyr::select(commonname, spp.maxT.baseline, spp.minT.baseline, spp.meanT.baseline) %>% 
  distinct()

# match species thermal preferences to positions of isotherms in soda

iso.baseline <- rbind(eqdat.iso.baseline, poldat.iso.baseline)

out <- NULL
for(i in min(btemp.stats$year_match):max(btemp.stats$year_match)) {
  for(j in 1:length(iso.baseline$commonname)) {
    commonname = iso.baseline$commonname[j]
    spp.maxT.baseline = iso.baseline[j,]$spp.maxT.baseline
    spp.minT.baseline = iso.baseline[j,]$spp.minT.baseline
    spp.meanT.baseline = iso.baseline[j,]$spp.meanT.baseline
    year = i
    btempyear = btemp.stats[btemp.stats$year_match==year,]
    thisyear.meanT.iso = sapply(spp.meanT.baseline, function(x) btempyear$btemp.lat.year.mean[order(abs(x-btempyear$btemp.lat.year.mean))][1]) # in that year, find the closest matching temperature to the species preference  
    thisyear.maxT.iso = sapply(spp.maxT.baseline, function(x) btempyear$btemp.lat.year.max[order(abs(x-btempyear$btemp.lat.year.max))][1])  
    thisyear.minT.iso = sapply(spp.minT.baseline, function(x) btempyear$btemp.lat.year.min[order(abs(x-btempyear$btemp.lat.year.min))][1]) 
    thisyear.meanT.lat = btempyear[btempyear$btemp.lat.year.mean==thisyear.meanT.iso,]$y[1] # find the latitude of the closest matching temperature 
    thisyear.minT.lat = btempyear[btempyear$btemp.lat.year.min==thisyear.minT.iso,]$y[1] 
    thisyear.maxT.lat = btempyear[btempyear$btemp.lat.year.max==thisyear.maxT.iso,]$y[1] 
    tempdf = as.data.frame(cbind(commonname, spp.maxT.baseline, spp.minT.baseline, spp.meanT.baseline, thisyear.meanT.lat, thisyear.maxT.lat, thisyear.minT.lat, year))
    out = rbind(out, tempdf)
    rm(btempyear, thisyear.minT.lat, thisyear.meanT.lat, thisyear.maxT.lat, thisyear.maxT.iso, thisyear.meanT.iso, thisyear.minT.iso)
  }
}

isodf <- as.data.frame(out) %>% 
  mutate(
    spp.maxT.baseline = as.numeric(as.character(spp.maxT.baseline)),
    spp.minT.baseline = as.numeric(as.character(spp.minT.baseline)),
    spp.meanT.baseline = as.numeric(as.character(spp.meanT.baseline)),
    thisyear.minT.lat = as.numeric(as.character(thisyear.minT.lat)),
    thisyear.meanT.lat = as.numeric(as.character(thisyear.meanT.lat)),
    thisyear.maxT.lat = as.numeric(as.character(thisyear.maxT.lat))
  )

poldat.stats.iso <- poldat.stats %>% 
  mutate(year=as.factor(year)) %>% 
  left_join(isodf, by=c("commonname","year")) 
# note that the year match has already occurred, so the "year" value can now be used for analysis and the edge position will be matched to last year's temperature values 

eqdat.stats.iso <- eqdat.stats %>% 
  mutate(year=as.factor(year)) %>% 
  left_join(isodf, by=c("commonname","year")) 

write_rds(poldat.stats.iso, here("processed-data","poldat.stats.iso.rds"))
write_rds(eqdat.stats.iso, here("processed-data","eqdat.stats.iso.rds"))
rm(list=ls())
