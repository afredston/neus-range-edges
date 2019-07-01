# this script organizes the raw trawl data
# it does basic data cleaning, primarily discarding rows without full species names or coordinates 
# I also created an estimate of true annual biomass for each species by standardizing by area surveyed and including true absences 
# some of these commands could take up to a few minutes to run

# to get trawl data, go to https://oceanadapt.rutgers.edu/
# download all data from the Northeast US 
# this was done for this project on March 14, 2019
# South Atlantic summary data was downloaded from OceanAdapt on May 5, 2019

# the OceanAdapt data does not have an automated way of filtering for records that are identified to species. To do this, I used trawlData, an older data package also meant to access NOAA trawl survey data. it is nontrivial to install, but its function could be replaced here by manually going through the species list from OceanAdapt and discarding non-species entries (e.g., "Rajiformes", "Clupeidae"). 

library(here) 
library(tidyverse)
library(trawlData) # https://github.com/rBatt/trawlData

neus.data.raw <- read_csv(here("data/oceanadapt031419","neus_data.csv")) %>% 
  filter(SEASON=="SPRING") # immediately get rid of fall data to make df smaller 

# note that a more complete .csv of the survey strata became available after the trawl data was downloaded, so that file alone is updated here after the 03/14/19 download
neus.strata.raw <- read_csv(here("data/oceanadapt031419","neus_strata_updated063019.csv")) %>% 
  dplyr::select(STRATUM, STRATUM_AREA) %>% # get rid of unwanted columns
  rename(STRATUM_AREA_NMI2 = STRATUM_AREA) %>% 
  mutate(STRATUM = as.character(STRATUM))# make consistent with data
neus.svspp.raw <- read_csv(here("data/oceanadapt031419","neus_svspp.csv"))
survey.area.nmi2 <- 0.01 # NOAA trawl surveys are standardized to cover 0.01 square nautical miles in each tow

# biomass corrections 

# the trawl data reports biomass separately for each sex class of each species. they calculate the biomass from the number of individuals caught at different lengths in that sex class, using a length-weight regression. note that the "abundance" column refers to numbers of individuals, not their biomass. 

# the script below calculates a sum biomass for each species in each haul and then converts it to an effort-corrected biomass value.

biomass.prep.df <- neus.data.raw %>% 
  group_by(SVSPP,YEAR,STRATUM,CRUISE6,STATION,CATCHSEX) %>% # group by species, haul, and sex 
  mutate(biomass.sex = mean(BIOMASS, na.rm=TRUE)) %>% # get mean biomass for each sex class
  ungroup() %>% 
  dplyr::select(-ABUNDANCE, -LENGTH, -NUMLEN, -BIOMASS) %>% # get rid of all the categories that subdivide species * haul combinations except catchsex
  distinct() %>% 
  group_by(SVSPP,YEAR,STRATUM,CRUISE6,STATION) %>% # group by species and haul
  mutate(biomass.haul = sum(biomass.sex)) %>% # add up biomass from different sex classes
  ungroup() %>% 
  dplyr::select(-CATCHSEX, -biomass.sex) %>% 
  distinct() %>% 
  mutate(HAULID = paste(YEAR, CRUISE6, STRATUM, STATION, sep="-")) %>% 
  dplyr::select(HAULID, SVSPP, biomass.haul) %>% 
  distinct() # just a check; this should not remove any rows

expand.prep1 <- neus.data.raw %>% 
  dplyr::select(CRUISE6, STATION, STRATUM, YEAR) %>% 
  distinct() %>% 
  mutate(HAULID = paste(YEAR, CRUISE6, STRATUM, STATION, sep="-")) %>% 
  pull(HAULID)
expand.prep2 <- neus.svspp.raw$SVSPP 

# calculate true biomass for each species standardized for effort and including true absences 
biomass.df <- expand.grid(expand.prep1, expand.prep2) %>% 
  rename(HAULID = Var1, SVSPP = Var2) %>% # all possible species * haul combinations, to create df with true zeroes 
  left_join(biomass.prep.df, by=c("HAULID","SVSPP")) %>% # get actual biomass records 
  mutate(biomass.haul = replace_na(biomass.haul, 0)) %>%  # replace NAs with zeroes
  separate(HAULID, into=c("YEAR", "CRUISE6", "STRATUM", "STATION"), sep="-") %>% 
  left_join(neus.strata.raw, by="STRATUM") %>% # get stratum area only for those strata used in the trawl survey 
  filter(!is.na(STRATUM_AREA_NMI2)) %>% 
  group_by(SVSPP, YEAR, CRUISE6, STRATUM) %>% # not grouping by station, to get the mean for the entire stratum, including zeroes
  mutate(biomass.stratum.mean = mean(biomass.haul),
         biomass.stratum.mean = replace_na(biomass.stratum.mean, 0)) %>% # carry through true zeroes 
  ungroup() %>% 
  dplyr::select(-biomass.haul, -STATION) %>% # get rid of replicates within a stratum now that we have the stratum mean
  distinct() %>% 
  rowwise() %>% 
  mutate(biomass.stratum.total = (biomass.stratum.mean / survey.area.nmi2)*STRATUM_AREA_NMI2) %>% # calculate the biomass per area swept, and then multiply by the area of the stratum 
  group_by(SVSPP, YEAR) %>% 
  mutate(biomass.correct.kg = sum(biomass.stratum.total),
         biomass.correct.kg = replace_na(biomass.correct.kg, 0)) %>% # carry through true zeroes 
  ungroup() %>% 
  dplyr::select(SVSPP, YEAR, biomass.correct.kg) %>% 
  distinct() %>%  # get final list of corrected biomasses for each species in each year 
  mutate(YEAR = as.numeric(YEAR))

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

# clean neus data: discarding tows below 36N (too rare), unnecessary columns, 
neus <- neus.data.raw %>%
  group_by(SVSPP,YEAR,STRATUM,CRUISE6,STATION,CATCHSEX) %>% # recalculate total biomass per haul, for later analysis (need it to weight depth estimates) 
  mutate(biomass.sex = mean(BIOMASS, na.rm=TRUE)) %>% # get mean biomass for each sex class
  ungroup() %>% 
  dplyr::select(-ABUNDANCE, -LENGTH, -NUMLEN, -BIOMASS) %>% # get rid of all the categories that subdivide species * haul combinations except catchsex
  distinct() %>% 
  group_by(SVSPP,YEAR,STRATUM,CRUISE6,STATION) %>% # group by species and haul
  mutate(biomass.raw = sum(biomass.sex)) %>% # add up biomass from different sex classes
  ungroup() %>% 
  rename_all(tolower) %>% 
  dplyr::select(-botsalin, -surfsalin, -x1, -catchsex, -biomass.sex) %>%
  distinct() %>%
  left_join(biomass.df, by=c('svspp'='SVSPP','year'='YEAR')) %>% 
  mutate(svspp=as.character(svspp)) %>% 
  left_join(neus.spp.prep, by="svspp") %>% 
  filter(taxlvl=="species",
         !is.na(lat), # they are all georeferenced, this doesn't do anything
         !is.na(lon),
         biomass.raw>0,
        lat>36
         ) %>% # retain only records IDed to species
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
