library(here)
library(tidyverse)
library(purrr)
library(broom)
library(lme4)

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
mutate(year = as.numeric(year)) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

soda.stats <- readRDS(here("processed-data","soda_stats.rds")) %>% 
  filter(year_match > 1980) 

soda.stats.summary <- soda.stats %>% 
  dplyr::select(year_match, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() # get rid of lat-specific stats

hadisst.stats <- readRDS(here("processed-data","hadisst_stats.rds")) %>% 
  filter(year_match > 1968) 

hadisst.stats.summary <- hadisst.stats %>% 
  dplyr::select(year_match, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() # get rid of lat-specific stats

oisst.extremes <- readRDS(here("processed-data","oisst_neus.rds")) %>% 
  filter(year_measured > 1981) %>% 
  group_by(year_measured) %>% 
  mutate(
    year.daily.mean = mean(sst),
    year.daily.99 = quantile(sst, 0.99),
    year.daily.01 = quantile(sst, 0.01)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year_measured, year_match, year.daily.99, year.daily.01, year.daily.mean) %>% 
  distinct()

#################
# LM: temperatures ~ time 
#################

hadisst.lm.mean <- hadisst.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% # use year_measured which refers to the actual year measured not the edge year to match to
  distinct() %>% 
  lm(year.month.mean ~ year_measured, data=.) %>% 
  summary()

hadisst.lm.max <- hadisst.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.max ~ year_measured, data=.) %>% 
  summary()

hadisst.lm.min <- hadisst.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.min ~ year_measured, data=.) %>% 
  summary()

soda.lm.mean <- soda.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.mean ~ year_measured, data=.) %>% 
  summary()

soda.lm.max <- soda.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.max ~ year_measured, data=.) %>% 
  summary()

soda.lm.min <- soda.stats %>% 
  dplyr::select(year_measured, year.month.mean, year.month.max, year.month.sd, year.month.min) %>% 
  distinct() %>% 
  lm(year.month.min ~ year_measured, data=.) %>% 
  summary()

oisst.lm.min <- oisst.extremes %>% 
  lm(year.daily.01 ~ year_measured, data = .) %>% 
  summary()

oisst.lm.max <- oisst.extremes %>% 
  lm(year.daily.99 ~ year_measured, data = .) %>% 
  summary()

#################
# LM: edge ~ time 
#################

poldat.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist95 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

poldat.corr <- poldat.stats.iso %>% 
  dplyr::select(commonname, spp.dist95, year) %>% 
  distinct() %>% 
  nest(-commonname) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$spp.dist95, .x$year, method="spearman")),
    tidied = map(test, tidy)) %>% 
  unnest(tidied, .drop=TRUE) 

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

eqdat.corr <- eqdat.stats.iso %>% 
  dplyr::select(commonname, spp.dist05, year) %>% 
  distinct() %>% 
  nest(-commonname) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$spp.dist05, .x$year, method="spearman")),
    tidied = map(test, tidy)) %>% 
  unnest(tidied, .drop=TRUE) 

#################
# LM: assemblage ~ time   
#################

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

#################
# GLM/LME: assemblage ~ temperature   
#################

poldat.btemp.mean <- poldat.stats.iso %>% 
  left_join(soda.stats.summary, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean)) %>% 
  dplyr::select(assemblage.dist95, year.month.mean, year.month.max, year.month.min) %>% 
  distinct() %>% 
  glm(assemblage.dist95 ~ year.month.mean, data = .) 

eqdat.btemp.mean <- eqdat.stats.iso %>% 
  left_join(soda.stats.summary, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean)) %>% 
  dplyr::select(assemblage.dist05, year.month.mean, year.month.max, year.month.min) %>% 
  distinct() %>% 
  glm(assemblage.dist05 ~ year.month.mean, data = .)

poldat.stemp.mean <- poldat.stats.iso %>% 
  left_join(hadisst.stats.summary, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean)) %>% 
  dplyr::select(assemblage.dist95, year.month.mean, year.month.max, year.month.min) %>% 
  distinct() %>% 
  glm(assemblage.dist95 ~ year.month.mean, data = .) 

eqdat.stemp.mean <- eqdat.stats.iso %>% 
  left_join(hadisst.stats.summary, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean)) %>% 
  dplyr::select(assemblage.dist05, year.month.mean, year.month.max, year.month.min) %>% 
  distinct() %>% 
  glm(assemblage.dist05 ~ year.month.mean, data = .) 

poldat.btemp.all <- poldat.stats.iso %>% 
  left_join(soda.stats.summary, by=c('year'='year_match')) %>% 
  left_join(oisst.extremes, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean), !is.na(year.daily.mean)) %>% 
  dplyr::select(year, assemblage.dist95, year.month.mean, year.month.max, year.month.min, year.daily.mean, year.daily.99, year.daily.01) %>% 
  distinct() %>% 
  glm(assemblage.dist95 ~ year.month.mean + year.daily.mean + year.daily.99 + year.daily.01 + year.month.mean*year.daily.mean + year.daily.99*year.daily.mean, data = .)

eqdat.btemp.all <- eqdat.stats.iso %>% 
  left_join(soda.stats.summary, by=c('year'='year_match')) %>% 
  left_join(oisst.extremes, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean), !is.na(year.daily.mean)) %>% 
  dplyr::select(year, assemblage.dist05, year.month.mean, year.month.max, year.month.min, year.daily.mean, year.daily.99, year.daily.01) %>% 
  distinct() %>% 
  glm(assemblage.dist05 ~ year.month.mean + year.daily.mean + year.daily.99 + year.daily.01 + year.month.mean*year.daily.mean + year.daily.99*year.daily.mean, data = .) 

poldat.stemp.lme <- poldat.stats.iso %>% 
  left_join(hadisst.stats.summary, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean)) %>% 
  dplyr::select(commonname, spp.dist95, year.month.mean, year.month.max, year.month.min) %>% 
  distinct() %>% 
  lmer(spp.dist95 ~ year.month.mean + (1|commonname), data = .) 

eqdat.stemp.lme <- eqdat.stats.iso %>% 
  left_join(hadisst.stats.summary, by=c('year'='year_match')) %>% 
  filter(!is.na(year.month.mean)) %>% 
  dplyr::select(commonname, spp.dist05, year.month.mean, year.month.max, year.month.min) %>% 
  distinct() %>% 
  lmer(spp.dist05 ~ year.month.mean + (1|commonname), data = .) 

#################
# LM: edge ~ isotherm 
#################

eqdat.iso.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.lat05 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")

poldat.iso.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.lat95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")


#################
# LM: depth ~ time 
#################

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

#################
# LM: abundance ~ time 
#################

poldat.abund.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, biomass.sum, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(biomass.sum ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

eqdat.abund.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, biomass.sum, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(biomass.sum ~ year, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

#################
# GLM: edge ~ temperature 
#################

poldat.spp.glm.all <- poldat.stats.iso %>% 
  nest(-commonname) %>% 
  mutate(
    model = map(data, ~glm(spp.dist95 ~ thisyear.minT.lat + thisyear.maxT.lat + thisyear.meanT.lat, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(!term=="(Intercept)")

poldat.spp.glm.mean <- poldat.stats.iso %>% 
  nest(-commonname) %>% 
  mutate(
    model = map(data, ~glm(spp.dist95 ~ thisyear.meanT.lat, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(!term=="(Intercept)")

eqdat.spp.glm.all <- eqdat.stats.iso %>% 
  nest(-commonname) %>% 
  mutate(
    model = map(data, ~glm(spp.dist05 ~ thisyear.minT.lat + thisyear.maxT.lat + thisyear.meanT.lat, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(!term=="(Intercept)")

eqdat.spp.glm.mean <- eqdat.stats.iso %>% 
  nest(-commonname) %>% 
  mutate(
    model = map(data, ~glm(spp.dist05 ~ thisyear.meanT.lat, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(!term=="(Intercept)")

#################
# time series 
#################

pol.spp.time <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.dist95) %>% 
  distinct() %>% 
  ggplot(aes(x=year, y=spp.dist95)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Poleward Edge Position") +
  xlab("Year") +
  NULL
pol.spp.time

eq.spp.time <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, spp.dist05) %>% 
  distinct() %>% 
  ggplot(aes(x=year, y=spp.dist05)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Equatorward Edge Position") +
  xlab("Year") +
  NULL
eq.spp.time
