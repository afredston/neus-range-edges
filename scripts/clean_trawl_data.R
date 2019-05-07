# this script identifies the species to be studied in the neus dataset 

# load packages 
library(tidyverse)
library(data.table)
library(here)
library(gridExtra)
library(rfishbase) 
library(stringr)

neus <- readRDS(here("processed-data", "neus.rds")) 
sa.spp <- read_csv(here("data", "oceanadapt_SA_050519.csv")) %>% 
  dplyr::select(Species) %>% 
  distinct() %>% 
  pull()

################
### SET DATA PREFERENCES
################

num_obs_year_cutoff <- 10 # how many times does a species need to be observed in a year for that species*year combo to be included in analysis? 
numyears_cutoff <- 10 
northerncutoff <- 42 # max lat at which a range edge can start and still be classified a northern edge (beyond this, the species could extend into Cape Cod / Canada )
aquamaps.pol.cutoff <- 50 # if aquamaps says a species' 95th percentile lat falls ABOVE this value, we discard it 
aquamaps.eq.cutoff <- 30 # if aquamaps says a species' 05th percentile lat falls BELOW this value, we discard it 

keepclass <- c('Actinopteri',"Actinopterygii","Elasmobranchii","Chondrichthyes") %>% tolower()

################
### FIND TRUE EDGES
################

# these are large datasets provided by the Aquamaps team. contact them or see website for details:
# https://www.aquamaps.org/

load(here("data","aquamaps.RData"))
load(here("data","aquamaps.species.Rdata")) 
load(here("data","aquamaps.spatial.Rdata"))

aquamaps.neus <- aquamaps.species %>% 
  dplyr::select(-Species) %>% 
  filter(species %in% unique(neus$sciname)) %>% # almost all spp are present 
  left_join(aquamaps, by="SPECIESID") %>% 
  left_join(aquamaps.spatial, by="LOICZID") %>% 
  filter(probability > 0.8) %>% 
  group_by(species) %>% 
  mutate(
    maxlat = max(CenterLat), 
    minlat = min(CenterLat), 
    lat05 = quantile(CenterLat, 0.05), 
    lat95 = quantile(CenterLat, 0.95)
  ) %>% 
  ungroup() %>% 
  dplyr::select(species, maxlat, minlat, lat05, lat95) %>% 
  distinct()
rm(aquamaps, aquamaps.species, aquamaps.spatial)

aquamaps.notpol <- aquamaps.neus[aquamaps.neus$lat95>aquamaps.pol.cutoff,]$species # very conservative--just saying that if the 95th percentile latitude is above XX degrees, the poleward edge probably isn't in NEUS

aquamaps.noteq <- aquamaps.neus[aquamaps.neus$lat05<aquamaps.eq.cutoff,]$species 

iucnfb.manual.notpol <- c("Brevoortia tyrannus","Dipturus laevis","Morone saxatilis","Peprilus triacanthus","Pomatomus saltatrix","Urophycis regia","Zenopsis conchifera","Alosa sapidissima", "Tautogolabrus adspersus","Prionotus evolans") 
# having looked up the leading edge species by hand, remove these! in this case they all have range edges in canada somewhere. if iucn was unclear I used fishbase as a tiebreaker--if fishbase said the range was as far north as 46N or so (since fishbase has the color coding to show abundance), I kept it in. if IUCN said it's in the US but Fishbase says it's way into Canada I took it out.

iucnfb.manual.noteq <- c("Menidia menidia","Zenopsis conchifera","Alosa sapidissima","Squatina dumeril","Cynoscion regalis","Prionotus evolans","Lophius americanus") 

################
### MAKE DATAFRAMES
################

# get common names from fishbase 
fishbaselist <- rfishbase::validate_names(unique(neus$sciname)) # this replaces names with no match with NAs

fishbase.species <- rfishbase::species(fishbaselist, fields = c("FBname","Species")) %>% 
  rename(latinname = Species,
         commonname = FBname) 

# equatorward edge df 
eqdat <- neus %>% 
  rename(latinname = sciname) %>% 
  filter(!latinname %in% sa.spp, # keep only species that have never been observed in SA--true northern species 
         class %in% keepclass, # keep only fish 
         !latinname %in% aquamaps.noteq,
         !latinname %in% iucnfb.manual.noteq) %>%  
  left_join(fishbase.species, by="latinname") %>% 
  group_by(latinname, year) %>% 
  mutate(
    numobsyear = length(unique(haulid))
  ) %>% 
  ungroup() %>% 
  filter(numobsyear >= num_obs_year_cutoff) %>% # get rid of all species*year combos where the species was observed fewer than 10 times 
  group_by(latinname) %>% 
  mutate(
    numyears = length(unique(year)), # do this late in the workflow so it is an accurate count for remaining species/hauls 
    numobs = length(unique(haulid)),
    meanobsyear = numobs / numyears) %>% 
  ungroup() %>% 
  filter(numyears >= numyears_cutoff)

eqdat.summ <- eqdat %>% 
  group_by(latinname, commonname, numobs, numyears, meanobsyear) %>% 
  summarise()

# poleward edge df--more complex because I don't have an analog to SA to help identify which species have true edges 

poldat.firstyear <- neus %>% # calculate first year when species was seen
  rename(latinname = sciname) %>% 
  filter(class %in% keepclass) %>% 
  group_by(latinname) %>%
  mutate(firstyear = min(year)) %>% 
  filter(year == firstyear) %>% 
  ungroup() %>% 
  group_by(latinname, year) %>% 
  mutate(firstlat_max = max(lat)) %>% 
  ungroup() %>% 
  dplyr::select(latinname, firstyear, firstlat_max) %>%
  distinct() 

poldat <- neus %>% 
  rename(latinname = sciname) %>% 
  filter(class %in% keepclass) %>% 
  left_join(fishbase.species, by="latinname") %>% 
  left_join(poldat.firstyear, by="latinname") %>% 
  filter(firstlat_max < 42,
         !latinname %in% aquamaps.notpol,
         !latinname %in% iucnfb.manual.notpol) %>% 
  group_by(latinname, year) %>% 
  mutate(numobsyear = length(unique(haulid))) %>% 
  ungroup() %>% 
  filter(numobsyear >= num_obs_year_cutoff) %>% 
  group_by(latinname) %>% 
  mutate(numobs = length(unique(haulid)),
         numyears = length(unique(year)),
         meanobsyear = numobs / numyears) %>% 
  ungroup() %>% 
  filter(numyears >= numyears_cutoff)

poldat.summ <- poldat %>% 
  group_by(latinname, commonname, numobs, numyears, meanobsyear) %>% 
  summarise()

# final check:
intersect(poldat.summ$commonname, eqdat.summ$commonname)

saveRDS(eqdat, "processed-data/eqdat.rds")
saveRDS(poldat, "processed-data/poldat.rds")

write_csv(eqdat.summ, "processed-data/eqdat_summary.csv")
write_csv(poldat.summ, "processed-data/poldat_summary.csv")

rm(list=ls())
