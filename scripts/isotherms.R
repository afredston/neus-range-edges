
# calculate species-specific and regional isotherm displacement over time 

#################
### LOAD IN PACKAGES
#################

library(here)
library(tidyverse)
library(data.table)
library(purrr)
library(broom)

#################
### READ IN DATASETS
#################

poldat.stats <- readRDS(here("processed-data","poldat.stats.rds"))
eqdat.stats <- readRDS(here("processed-data","eqdat.stats.rds")) 
soda.stats <- readRDS(here("processed-data","soda_stats.rds")) 
oisst.stats <- readRDS(here("processed-data","oisst_stats.rds"))
hadisst.stats <- readRDS(here("processed-data","hadisst_stats.rds"))

oisst.neus <- readRDS(here("processed-data","oisst_neus.rds"))
hadisst.neus <- readRDS(here("processed-data","hadisst_neus.rds"))
soda.neus <- readRDS(here("processed-data","soda_neus.rds"))

#################
### MAKE LINEAR MODELS AND FUNCTIONS FOR TEMPERATURE DATASETS 
#################

# gives the equation of a line from which temperatures at any latitude can be calculated 

lm.hadisst <- hadisst.neus %>% 
  nest(-year_match) %>% 
  mutate(
    model = purrr::map(data, ~lm(sst ~ y, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) 
get.had.lat <- function(temp, year) {
  tmp <- lm.hadisst[lm.hadisst$year_match==year,]
  out <- (temp-tmp[[1,3]])/tmp[[2,3]]
  return(out)
}
get.had.temp <- function(lat, year) {
  tmp <- lm.hadisst[lm.hadisst$year_match==year,]
  out <- lat*tmp[[2,3]]+tmp[[1,3]]
  return(out)
}

lm.soda <- soda.neus %>% 
  nest(-year_match) %>% 
  mutate(
    model = purrr::map(data, ~lm(btemp ~ y, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE)
get.soda.lat <- function(temp, year) {
  tmp <- lm.soda[lm.soda$year_match==year,]
  out <- (temp-tmp[[1,3]])/tmp[[2,3]]
  return(out)
}
get.soda.temp <- function(lat, year) {
  tmp <- lm.soda[lm.soda$year_match==year,]
  out <- lat*tmp[[2,3]]+tmp[[1,3]]
  return(out)
}

lm.oisst <- oisst.neus %>% 
  nest(-year_match) %>% 
  mutate(
    model = purrr::map(data, ~lm(sst ~ y, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE)

#################
### INTEGER AND ASSEMBLAGE ISOTHERMS
#################

# this script is a nightmare and doesn't work

# get.soda.lat2 <- function(temp, year) {
#   tmp <- lm.soda[lm.soda$year_match==year,]
#   out <- (temp-tmp[[1,3]])/tmp[[2,3]]
#   return(out)
# }
# 
# degrees <- seq(8, 16, 2)
# sodayrs <- min(soda.stats$year_measured):max(soda.stats$year_measured)
# cross <- cross2(degrees, sodayrs)
# expand <- expand.grid(degrees, sodayrs)
# mapply(get.soda.lat, expand$Var1, expand$Var2)
# 
# out <- degrees %>% 
#   map(., get.soda.lat, sodayrs) %>% 
#   unlist()
# 
# map2(expand$Var1, expand$Var2, get.soda.lat)
# 
# data <- list(
#   degrees <- seq(8, 16, 2),
#   sodayrs <- min(soda.stats$year_measured):max(soda.stats$year_measured)
# ) %>% 
#   cross() %>% 
#   map2(lift(get.soda.lat))


#################
### SPECIES-SPECIFIC ISOTHERMS 
#################

# matching range edges to isotherms

# SODA first

# prep species data 

eqdat.iso.soda.prep <- eqdat.stats %>% 
  filter(year >= min(soda.neus$year_match)) %>% 
  group_by(latinname) %>% 
  arrange(year) %>% 
  slice(1:3) %>% # get first 3 years that species is observed
  ungroup()

# calculate temperature values for first three years when species was observed, using the linear model for each dataset and each year 

eqdat.iso.soda.prep2 <- NULL

for(i in unique(eqdat.iso.soda.prep$latinname)) {
  year.range <- eqdat.iso.soda.prep[eqdat.iso.soda.prep$latinname==i,]$year
  for(j in year.range) {
  spp.lat05 <- eqdat.iso.soda.prep[eqdat.iso.soda.prep$year==j & eqdat.iso.soda.prep$latinname==i,]$spp.lat05
  est.soda <- get.soda.temp(spp.lat05, j)
  out <- cbind(i, j, spp.lat05, est.soda)
  eqdat.iso.soda.prep2 <- rbind(out, eqdat.iso.soda.prep2)
  }
  rm(i, j, year.range, spp.lat05, est.soda, out)}

# tidy data frame to get the baseline temperature values for each species
eqdat.iso.soda.prep3 <- eqdat.iso.soda.prep2 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  group_by(latinname) %>% 
  mutate(est.soda = as.numeric(as.character(est.soda)), 
    est.edge.temp.soda = mean(est.soda)) %>% 
  dplyr::select(latinname, est.edge.temp.soda) %>% 
  ungroup() %>% 
  distinct() %>% 
  full_join(eqdat.stats, by="latinname") %>% 
  filter(year >= min(soda.neus$year_match))

# calculate future latitudes for the species-specific isotherms

eqdat.iso.soda.prep4 <- NULL

for(i in unique(eqdat.iso.soda.prep3$latinname)) {
  year.range <- eqdat.iso.soda.prep3[eqdat.iso.soda.prep3$latinname==i,]$year 
  est.edge.temp.soda <- eqdat.iso.soda.prep3 %>% filter(latinname==i) %>% group_by(latinname) %>% dplyr::select(est.edge.temp.soda) %>% distinct() %>% pull()
  for(j in year.range) {
    est.edge.lat.soda <- get.soda.lat(est.edge.temp.soda, j)
    out <- cbind(i, j, est.edge.lat.soda, est.edge.temp.soda)
    eqdat.iso.soda.prep4 <- rbind(out, eqdat.iso.soda.prep4)
  }
  rm(i, j, year.range, est.edge.temp.soda, est.edge.lat.soda, out)
}

eqdat.iso.soda.prep5 <- 
  eqdat.iso.soda.prep4 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  mutate(latinname = as.character(latinname),
         year = as.integer(as.character(year)),
         est.edge.lat.soda = as.numeric(as.character(est.edge.lat.soda))) 

# now HADISST

# prep species data 

eqdat.iso.had.prep <- eqdat.stats %>% 
  filter(year >= min(hadisst.neus$year_match)) %>% 
  group_by(latinname) %>% 
  arrange(year) %>% 
  slice(1:3) %>% # get first 3 years that species is observed
  ungroup()

# calculate temperature values for first three years when species was observed, using the linear model for each dataset and each year 

eqdat.iso.had.prep2 <- NULL

for(i in unique(eqdat.iso.had.prep$latinname)) {
  year.range <- eqdat.iso.had.prep[eqdat.iso.had.prep$latinname==i,]$year
  for(j in year.range) {
    spp.lat05 <- eqdat.iso.had.prep[eqdat.iso.had.prep$year==j & eqdat.iso.had.prep$latinname==i,]$spp.lat05
    est.had <- get.had.temp(spp.lat05, j)
    out <- cbind(i, j, spp.lat05, est.had)
    eqdat.iso.had.prep2 <- rbind(out, eqdat.iso.had.prep2)
  }
  rm(i, j, year.range, spp.lat05, est.had, out)}

# tidy data frame to get the baseline temperature values for each species
eqdat.iso.had.prep3 <- eqdat.iso.had.prep2 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  group_by(latinname) %>% 
  mutate(est.had = as.numeric(as.character(est.had)), 
         est.edge.temp.hadisst = mean(est.had)) %>% 
  dplyr::select(latinname, est.edge.temp.hadisst) %>% 
  ungroup() %>% 
  distinct() %>% 
  full_join(eqdat.stats, by="latinname") %>% 
  filter(year >= min(hadisst.neus$year_match))

# calculate future latitudes for the species-specific isotherms

eqdat.iso.had.prep4 <- NULL

for(i in unique(eqdat.iso.had.prep3$latinname)) {
  year.range <- eqdat.iso.had.prep3[eqdat.iso.had.prep3$latinname==i,]$year 
  est.edge.temp.hadisst <- eqdat.iso.had.prep3 %>% filter(latinname==i) %>% group_by(latinname) %>% dplyr::select(est.edge.temp.hadisst) %>% distinct() %>% pull()
  for(j in year.range) {
    est.edge.lat.hadisst <- get.had.lat(est.edge.temp.hadisst, j)
    out <- cbind(i, j, est.edge.lat.hadisst, est.edge.temp.hadisst)
    eqdat.iso.had.prep4 <- rbind(out, eqdat.iso.had.prep4)
  }
  rm(i, j, year.range, est.edge.temp.hadisst, est.edge.lat.hadisst, out)
}

eqdat.iso.had.prep5 <- 
  eqdat.iso.had.prep4 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  mutate(latinname = as.character(latinname),
         year = as.integer(as.character(year)),
         est.edge.lat.hadisst = as.numeric(as.character(est.edge.lat.hadisst)))

eqdat.stats.iso <- eqdat.stats %>% 
  left_join(eqdat.iso.had.prep5, by=c('year','latinname')) %>% 
  left_join(eqdat.iso.soda.prep5, by=c('year','latinname'))

# now poleward edge group

poldat.iso.soda.prep <- poldat.stats %>% 
  filter(year >= min(soda.neus$year_match)) %>% 
  group_by(latinname) %>% 
  arrange(year) %>% 
  slice(1:3) %>% # get first 3 years that species is observed
  ungroup()

# calculate temperature values for first three years when species was observed, using the linear model for each dataset and each year 

poldat.iso.soda.prep2 <- NULL

for(i in unique(poldat.iso.soda.prep$latinname)) {
  year.range <- poldat.iso.soda.prep[poldat.iso.soda.prep$latinname==i,]$year
  for(j in year.range) {
    spp.lat95 <- poldat.iso.soda.prep[poldat.iso.soda.prep$year==j & poldat.iso.soda.prep$latinname==i,]$spp.lat95
    est.soda <- get.soda.temp(spp.lat95, j)
    out <- cbind(i, j, spp.lat95, est.soda)
    poldat.iso.soda.prep2 <- rbind(out, poldat.iso.soda.prep2)
  }
  rm(i, j, year.range, spp.lat95, est.soda, out)}

# tidy data frame to get the baseline temperature values for each species
poldat.iso.soda.prep3 <- poldat.iso.soda.prep2 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  group_by(latinname) %>% 
  mutate(est.soda = as.numeric(as.character(est.soda)), 
         est.edge.temp.soda = mean(est.soda)) %>% 
  dplyr::select(latinname, est.edge.temp.soda) %>% 
  ungroup() %>% 
  distinct() %>% 
  full_join(poldat.stats, by="latinname") %>% 
  filter(year >= min(soda.neus$year_match))

# calculate future latitudes for the species-specific isotherms

poldat.iso.soda.prep4 <- NULL

for(i in unique(poldat.iso.soda.prep3$latinname)) {
  year.range <- poldat.iso.soda.prep3[poldat.iso.soda.prep3$latinname==i,]$year 
  est.edge.temp.soda <- poldat.iso.soda.prep3 %>% filter(latinname==i) %>% group_by(latinname) %>% dplyr::select(est.edge.temp.soda) %>% distinct() %>% pull()
  for(j in year.range) {
    est.edge.lat.soda <- get.soda.lat(est.edge.temp.soda, j)
    out <- cbind(i, j, est.edge.lat.soda, est.edge.temp.soda)
    poldat.iso.soda.prep4 <- rbind(out, poldat.iso.soda.prep4)
  }
  rm(i, j, year.range, est.edge.temp.soda, est.edge.lat.soda, out)
}

poldat.iso.soda.prep5 <- 
  poldat.iso.soda.prep4 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  mutate(latinname = as.character(latinname),
         year = as.integer(as.character(year)),
         est.edge.lat.soda = as.numeric(as.character(est.edge.lat.soda))) 

# now HADISST

# prep species data 

poldat.iso.had.prep <- poldat.stats %>% 
  filter(year >= min(hadisst.neus$year_match)) %>% 
  group_by(latinname) %>% 
  arrange(year) %>% 
  slice(1:3) %>% # get first 3 years that species is observed
  ungroup()

# calculate temperature values for first three years when species was observed, using the linear model for each dataset and each year 

poldat.iso.had.prep2 <- NULL

for(i in unique(poldat.iso.had.prep$latinname)) {
  year.range <- poldat.iso.had.prep[poldat.iso.had.prep$latinname==i,]$year
  for(j in year.range) {
    spp.lat95 <- poldat.iso.had.prep[poldat.iso.had.prep$year==j & poldat.iso.had.prep$latinname==i,]$spp.lat95
    est.had <- get.had.temp(spp.lat95, j)
    out <- cbind(i, j, spp.lat95, est.had)
    poldat.iso.had.prep2 <- rbind(out, poldat.iso.had.prep2)
  }
  rm(i, j, year.range, spp.lat95, est.had, out)}

# tidy data frame to get the baseline temperature values for each species
poldat.iso.had.prep3 <- poldat.iso.had.prep2 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  group_by(latinname) %>% 
  mutate(est.had = as.numeric(as.character(est.had)), 
         est.edge.temp.hadisst = mean(est.had)) %>% 
  dplyr::select(latinname, est.edge.temp.hadisst) %>% 
  ungroup() %>% 
  distinct() %>% 
  full_join(poldat.stats, by="latinname") %>% 
  filter(year >= min(hadisst.neus$year_match))

# calculate future latitudes for the species-specific isotherms

poldat.iso.had.prep4 <- NULL

for(i in unique(poldat.iso.had.prep3$latinname)) {
  year.range <- poldat.iso.had.prep3[poldat.iso.had.prep3$latinname==i,]$year 
  est.edge.temp.hadisst <- poldat.iso.had.prep3 %>% filter(latinname==i) %>% group_by(latinname) %>% dplyr::select(est.edge.temp.hadisst) %>% distinct() %>% pull()
  for(j in year.range) {
    est.edge.lat.hadisst <- get.had.lat(est.edge.temp.hadisst, j)
    out <- cbind(i, j, est.edge.lat.hadisst, est.edge.temp.hadisst)
    poldat.iso.had.prep4 <- rbind(out, poldat.iso.had.prep4)
  }
  rm(i, j, year.range, est.edge.temp.hadisst, est.edge.lat.hadisst, out)
}

poldat.iso.had.prep5 <- 
  poldat.iso.had.prep4 %>% 
  as.data.frame() %>% 
  rename(latinname=i, year=j) %>% 
  mutate(latinname = as.character(latinname),
         year = as.integer(as.character(year)),
         est.edge.lat.hadisst = as.numeric(as.character(est.edge.lat.hadisst)))

poldat.stats.iso <- poldat.stats %>% 
  left_join(poldat.iso.had.prep5, by=c('year','latinname')) %>% 
  left_join(poldat.iso.soda.prep5, by=c('year','latinname'))

write_rds(poldat.stats.iso, here("processed-data","poldat.stats.iso.rds"))
write_rds(eqdat.stats.iso, here("processed-data","eqdat.stats.iso.rds"))
rm(list=ls())
