# this script calculates all of the statistics reported in-text in the manuscript

library(here)
library(tidyverse)
library(purrr)
library(broom)
library(lme4)

###################
# 1. Region and historical warming
###################

soda.stats <- readRDS(here("processed-data","soda_stats.rds")) 

hadisst.stats <- readRDS(here("processed-data","hadisst_stats.rds")) 

oisst.stats <- readRDS(here("processed-data","oisst_neus.rds"))

hadisst.lm.mean <- hadisst.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% # use year_measured which refers to the actual year measured not the edge year to match to
  distinct() %>% 
  filter(year_measured >= 1968) %>% 
  lm(year.month.mean ~ year_measured, data=.) %>% 
  summary()

oisst.lm.high <- oisst.stats %>% 
  filter(year_measured >= 1982) %>% 
  group_by(year_measured) %>% 
  mutate(year.daily.99 = quantile(sst, 0.99)) %>% 
  ungroup() %>% 
  dplyr::select(year_measured, year.daily.99) %>% 
  distinct() %>% 
  lm(year.daily.99 ~ year_measured, data=.) %>% 
  summary()

oisst.lm.low <- oisst.stats %>% 
  filter(year_measured >= 1982) %>% 
  group_by(year_measured) %>% 
  mutate(year.daily.01 = quantile(sst, 0.01)) %>% 
  ungroup() %>% 
  dplyr::select(year_measured, year.daily.01) %>% 
  distinct() %>% 
  lm(year.daily.01 ~ year_measured, data=.) %>% 
  summary()

soda.lm.mean <- soda.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.mean ~ year_measured, data=.) %>% 
  summary()

###################
# 2. Survey data 
###################

poldat.stats.samples <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  dplyr::select(commonname, numyears, numobs) %>% 
  distinct()
quantile(poldat.stats.samples$numobs)
quantile(poldat.stats.samples$numyears)

eqdat.stats.samples <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  dplyr::select(commonname, numyears, numobs) %>% 
  distinct()
quantile(eqdat.stats.samples$numobs)
quantile(eqdat.stats.samples$numyears)

###################
# 3. Range edge position analysis
###################

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

poldat.assemblage.lm <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.dist95) %>% 
  distinct() %>% 
  lm(assemblage.dist95 ~ year, data = .) %>% 
  summary()

eqdat.assemblage.lm <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.dist05) %>% 
  distinct() %>% 
  lm(assemblage.dist05 ~ year, data = .) %>% 
  summary() 

###################
# 5. Changes in depth and biomass 
###################

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

poldat.depth.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, depth.mean.wt, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(depth.mean.wt ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

eqdat.depth.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, depth.mean.wt, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(depth.mean.wt ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")


poldat.abund.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, biomass.correct.kg, year) %>% 
  distinct() %>% 
  mutate(biomass.correct.mt = biomass.correct.kg/1000) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(biomass.correct.mt ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

eqdat.abund.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, biomass.correct.kg, year) %>% 
  distinct() %>% 
  mutate(biomass.correct.mt = biomass.correct.kg/1000) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(biomass.correct.mt ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")
