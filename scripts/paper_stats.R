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

oisst.neus <- readRDS(here("processed-data","oisst_neus.rds"))

hadisst.isotherms <- readRDS(here("processed-data","hadisst_isotherms_time.rds"))

hadisst.lm.mean <- hadisst.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% # use year_measured which refers to the actual year measured not the edge year to match to
  distinct() %>% 
  filter(year_measured >= 1968) %>% 
  lm(year.month.mean ~ year_measured, data=.) %>% 
  summary()

oisst.lm.high <- oisst.neus %>% 
  group_by(time) %>% 
  mutate(day.mean.sst = mean(sst)) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(year.daily.99 = quantile(day.mean.sst, 0.99)) %>% 
  ungroup() %>% 
  dplyr::select(year, year.daily.99) %>% 
  distinct() %>% 
  lm(year.daily.99 ~ year, data=.) %>% 
  summary()

oisst.lm.low <- oisst.neus %>% 
  group_by(time) %>% 
  mutate(day.mean.sst = mean(sst)) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(year.daily.01 = quantile(day.mean.sst, 0.01)) %>% 
  ungroup() %>% 
  dplyr::select(year, year.daily.01) %>% 
  distinct() %>% 
  lm(year.daily.01 ~ year, data=.) %>% 
  summary()

soda.lm.mean <- soda.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.mean ~ year_measured, data=.) %>% 
  summary()

isotherm.shifts <- hadisst.isotherms %>%
  group_by(degrees) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(est.iso.hadisst ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year") %>%
  dplyr::select(-data, -model)

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

#changes in depth over time
poldat.depth.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, depth.mean.wt, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(depth.mean.wt ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  filter(term=="year")

eqdat.depth.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, depth.mean.wt, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(depth.mean.wt ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  filter(term=="year")

# changes in biomass over time
poldat.abund.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, biomass.correct.kg, year) %>% 
  distinct() %>% 
  mutate(biomass.correct.mt = biomass.correct.kg/1000) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(biomass.correct.mt ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  filter(term=="year")

eqdat.abund.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, biomass.correct.kg, year) %>% 
  distinct() %>% 
  mutate(biomass.correct.mt = biomass.correct.kg/1000) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(biomass.correct.mt ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  filter(term=="year")

# changes in depth vs edge
poldat.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.dist95 ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

eqdat.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist05, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.dist05 ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

poldat.edge.depth <- poldat.depth.lm %>%
  rename(depth.coeff = estimate) %>%
  left_join(poldat.lm %>% select(commonname, estimate), by="commonname") %>%
  rename(edge.coeff = estimate) %>%
  select(commonname, depth.coeff, edge.coeff)

cor.test(poldat.edge.depth$depth.coeff, poldat.edge.depth$edge.coeff, method="spearman")

eqdat.edge.depth <- eqdat.depth.lm %>%
  rename(depth.coeff = estimate) %>%
  left_join(eqdat.lm %>% select(commonname, estimate), by="commonname") %>%
  rename(edge.coeff = estimate) %>%
  select(commonname, depth.coeff, edge.coeff)

cor.test(eqdat.edge.depth$depth.coeff, eqdat.edge.depth$edge.coeff, method="spearman")

poldat.edge.abund <- poldat.abund.lm %>%
  rename(abund.coeff = estimate) %>%
  left_join(poldat.lm %>% select(commonname, estimate), by="commonname") %>%
  rename(edge.coeff = estimate) %>%
  select(commonname, abund.coeff, edge.coeff)

cor.test(poldat.edge.abund$abund.coeff, poldat.edge.abund$edge.coeff, method="spearman")

eqdat.edge.abund <- eqdat.abund.lm %>%
  rename(abund.coeff = estimate) %>%
  left_join(eqdat.lm %>% select(commonname, estimate), by="commonname") %>%
  rename(edge.coeff = estimate) %>%
  select(commonname, abund.coeff, edge.coeff)

cor.test(eqdat.edge.abund$abund.coeff, eqdat.edge.abund$edge.coeff, method="spearman")
