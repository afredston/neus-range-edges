

# install.packages("rerddap")
library(ggplot2) 
library(rerddap) 
library(tidyverse)
library(here)
# sstlist <- ed_search("oisst") # explore to find the right dataset, copy code
hadisst <- "erdHadISST" #MONTHLY 1870-present https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdHadISST.html
oisst <- "ncdcOisst2Agg_LonPM180" #DAILY from late 1981 https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst2Agg_LonPM180.html
rerddap::info(hadisst) # this is griddap data

startyear.hadisst <- 1967
endyear.hadisst <- 2017 # as of last time running, 2018 was not complete in hadISST
startyear.oisst <- 1982
endyear.oisst <- 2017


lonrange <- c(-78, -60)
latrange <- c(34.5, 48)

get.hadisst <- NULL 
for(i in startyear.hadisst:endyear.hadisst) {
  daterange = c(paste0(i,'-01-01'),paste0(i,'-12-31'))
  out = griddap(hadisst, latitude = latrange, longitude = lonrange, time = daterange, fields = 'sst')$data
  get.hadisst <- rbind(get.hadisst, out)
}

get.oisst <- NULL
for(i in startyear.oisst:endyear.oisst) {
  daterange = c(paste0(i,'-01-01'),paste0(i,'-12-31'))
  out = griddap(oisst, latitude = latrange, longitude = lonrange, time = daterange, fields = 'sst')$data
  get.oisst <- rbind(get.oisst, out)
}

saveRDS(get.hadisst, here("processed-data", "hadisst_raw.rds")) 
saveRDS(get.oisst, here("processed-data","oisst_raw.rds"))
rm(list=ls())
