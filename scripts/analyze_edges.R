library(here)
library(tidyverse)
library(purrr)
library(broom)

# add in lat analysis too? to compare/contrast

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
mutate(year = as.numeric(year)) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

btemp.stats <- readRDS(here("processed-data","soda_stats.rds")) %>% 
  filter(year_match > 1980) 

btemp.stats.overall <- btemp.stats %>% 
  dplyr::select(btemp.year.mean, btemp.year.min, btemp.year.max, year_match) %>% 
  distinct()

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
# LM: assemblage ~ time and temperature   
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

poldat.btemp <- poldat.stats.iso %>% 
  left_join(btemp.stats.overall, by=c('year'='year_match')) %>% 
  filter(!is.na(btemp.year.mean)) %>% 
  dplyr::select(assemblage.dist95, btemp.year.mean) %>% 
  distinct() %>% 
  glm(assemblage.dist95 ~ btemp.year.mean, data = .) %>% 
  summary()

eqdat.btemp <- eqdat.stats.iso %>% 
  left_join(btemp.stats.overall, by=c('year'='year_match')) %>% 
  filter(!is.na(btemp.year.mean)) %>% 
  dplyr::select(assemblage.dist05, btemp.year.mean) %>% 
  distinct() %>% 
  lm(assemblage.dist05 ~ btemp.year.mean, data = .) %>% 
  summary()

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
