# this script organizes the raw trawl datasets 

# go to https://oceanadapt.rutgers.edu/
# download all data from the Northeast US 
# this was done for this project on March 14, 2019
# South Atlantic summary data was downloaded from OceanAdapt on May 5, 2019
# trawlData is used here only to get useful taxonomic information
# see here: https://github.com/rBatt/trawlData/wiki/Install-trawlData

library(here) 
library(tidyverse)
library(trawlData)

neus.data.raw <- read_csv(here("data/oceanadapt031419","neus_data.csv"))
neus.strata.raw <- read_csv(here("data/oceanadapt031419","neus_strata.csv"))
neus.svspp.raw <- read_csv(here("data/oceanadapt031419","neus_svspp.csv"))

neus.strata.prep <- neus.strata.raw %>% 
  dplyr::select(StratumCode, Areanmi2) # get rid of unwanted columns before joining

# use useful taxonomic information from trawlData 
data(clean.neus)
trawldata.spplist <- clean.neus %>% 
  dplyr::select(spp, taxLvl, species, genus, family, order, class, phylum, kingdom) %>% 
  distinct() %>% 
  rename_all(tolower) %>% 
  mutate_all(tolower) 

neus.spp.prep <- neus.svspp.raw %>% 
  rename_all(tolower) %>% 
  mutate_all(tolower) %>% 
  dplyr::select(-x1) %>% # note that these are not all "good" species names, which is why I'm also using trawlData; any other data frame with taxonomic information on the same species would work fine  
  left_join(trawldata.spplist, by=c('sciname'='spp'))

# clean neus data and get rid of duplicate rows for length and sex bins that we don't care about. also throwing out tows below 36 because they're too rare
neus <- neus.data.raw %>%
  rename_all(tolower) %>% 
  filter(season=="SPRING", 
         biomass>0,
         lat>36) %>%
  dplyr::select(-botsalin, -surfsalin, -abundance, -numlen, -length, -x1) %>%
  distinct() %>%
  mutate(haulid = paste0(cruise6,"-",station,"-",stratum)) %>%
  group_by(haulid, svspp) %>%
  mutate(biomass.correct = sum(biomass)) %>%
  ungroup() %>%
  dplyr::select(-catchsex, -biomass) %>%
  distinct() %>% 
  mutate(svspp=as.character(svspp)) %>% 
  left_join(neus.spp.prep, by="svspp") %>% 
  filter(taxlvl=="species",
         !is.na(lat),
         !is.na(lon)) %>% # retain only records IDed to species
  mutate(sciname = str_to_sentence(sciname))

# script below explores patterns of missingness and latitude
# neus.explore <- neus %>% 
#   group_by(stratum) %>%
#   mutate(
#     pres=1,
#     numyears = length(unique(year)),
#     meanstratlat = as.factor(round(mean(lat), 5))) %>% # produces as many values as there are in stratum--same number of rows (141)
#   ungroup() %>%
#   dplyr::select(year, numyears, pres, meanstratlat) %>%
#   distinct() %>% 
#   ggplot(aes(x=year, y=meanstratlat, fill=pres)) +
#   geom_tile() +
#   labs(y="") +
#   theme(
#     legend.position="none",
#     axis.text.y=element_text(size=4)
#   ) +
#   NULL
# neus.explore 

saveRDS(neus, here("processed-data","neus.rds"))
rm(list=ls())
