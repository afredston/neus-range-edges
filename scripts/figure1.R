
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

sbt <- read_rds(here("processed-data","soda_stats.rds")) %>% 
  rename(lat = y, lon = x)

temprange = seq(min(as.integer(sbt$btemp.lat.year))+1, max(as.integer(sbt$btemp.lat.year))-1, 1) # trim off the extreme values

sbt.iso.df <- NULL

for(i in min(sbt$year):max(sbt$year)) {
  for(j in min(temprange):max(temprange)) {
    sbtyear = sbt[sbt$year==paste0(i),] # get sst values for only one year 
    sbtmatch = sapply(j, function(x) sbtyear$btemp.lat.year[order(abs(x-sbtyear$btemp.lat.year))][1]) # get the value for year i that minimizes the difference from j, the rounded temperature value 
    latmatch = sbtyear[sbtyear$btemp.lat.year==sbtmatch,]$lat
    year = i
    sbtref = j
    out = as.data.frame(cbind(year, sbtref, sbtmatch, latmatch))
    sbt.iso.df = rbind(sbt.iso.df, out)
  }
}

gg.sbt.iso <- sbt %>% 
  ggplot() +
  geom_raster(aes(x=year, y=lat, fill=btemp.lat.year)) + 
  geom_line(data=sbt.iso.df, aes(x=year, y=latmatch, group=sbtref)) +
  scale_fill_gradientn(colors=c("blue3","darkturquoise", "gold", "orangered", "red3")) + 
  NULL
gg.sbt.iso

gg.sbt <- sbt %>% 
  ggplot() +
  geom_raster(aes(x=year, y=lat, fill=btemp.lat.year)) + 
  scale_fill_gradientn(colors=c("blue3","darkturquoise", "gold", "orangered", "red3")) + 
  NULL
gg.sbt

sbt.iso.df.2c <- sbt.iso.df %>%
  filter(sbtref %% 2 == 0)

gg.sbt.iso2 <- sbt %>% 
  ggplot() +
  geom_raster(aes(x=year, y=lat, fill=btemp.lat.year)) + 
  scale_fill_gradientn(colors=c("blue3","darkturquoise", "gold", "orangered", "red3")) + 
  geom_line(data=sbt.iso.df.2c, aes(x=year, y=latmatch, group=sbtref)) +
  NULL
gg.sbt.iso2
# this isn't going to work unless I get higher-resolution SST data from the past 