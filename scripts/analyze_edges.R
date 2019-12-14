library(here)
library(tidyverse)
library(purrr)
library(broom)
library(broom.mixed)
library(lmerTest)

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
mutate(year = as.numeric(year)) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

soda.stats <- readRDS(here("processed-data","soda_stats.rds")) %>% 
  filter(year_match > 1980) 

soda.stats.summary <- soda.stats %>% 
  dplyr::select(year_match, year.month.mean) %>% 
  distinct() # get rid of lat-specific stats

hadisst.stats <- readRDS(here("processed-data","hadisst_stats.rds")) 

hadisst.stats.summary <- hadisst.stats %>% 
  dplyr::select(year_match, year.month.mean) %>% 
  distinct() # get rid of lat-specific stats

oisst.extremes <- readRDS(here("processed-data","oisst_neus.rds")) %>% 
  filter(year_match > 1982) %>% 
  group_by(time) %>% 
  mutate(day.mean.sst = mean(sst)) %>% # calculate mean temperature across entire region on that day
  ungroup() %>%
  group_by(year_match) %>% 
  mutate( # calculate 1st and 99th percentiles of regional daily temps
    year.daily.99 = quantile(day.mean.sst, 0.99),
    year.daily.01 = quantile(day.mean.sst, 0.01)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year_match, year.daily.99, year.daily.01) %>% 
  distinct()

all.temp.df <- soda.stats.summary %>% 
  rename(soda.year.month.mean = year.month.mean) %>% 
  full_join(hadisst.stats.summary, by="year_match") %>% 
  left_join(oisst.extremes, by="year_match") 

#################
# single-species LMs
#################

poldat.lm.time <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist95 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel)  %>% 
  filter(!term=="(Intercept)") %>%
  select(-data) %>%
  ungroup()%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

eqdat.lm.time <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist05, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist05 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel)  %>% 
  filter(!term=="(Intercept)") %>%
  select(-data) %>%
  ungroup()%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

poldat.lm.sst <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist95 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  filter(!term=="(Intercept)") %>%
  select(-data) %>%
  ungroup()%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

eqdat.lm.sst <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist05, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist05 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel)  %>% 
  filter(!term=="(Intercept)") %>%
  select(-data) %>%
  ungroup()%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

poldat.lm.sbt <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel)  %>% 
  filter(!term=="(Intercept)") %>%
  select(-data) %>%
  ungroup()%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

eqdat.lm.sbt <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist05, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist05 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  filter(!term=="(Intercept)") %>%
  select(-data) %>%
  ungroup()%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

poldat.lm.results <- rbind(poldat.lm.time, poldat.lm.sbt, poldat.lm.sst)
write_csv(poldat.lm.results, here("results","poleward_edge_results.csv"))

eqdat.lm.results <- rbind(eqdat.lm.time, eqdat.lm.sst, eqdat.lm.sbt)
write_csv(eqdat.lm.results, here("results","equatorward_edge_results.csv"))

#################
# assemblages over time
#################

poldat.assemblage.lm.time <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.dist95) %>% 
  distinct() %>% 
  mutate(commonname = "ASSEMBLAGE") %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(assemblage.dist95 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
unnest(tidymodel) %>% 
  select(-data)

eqdat.assemblage.lm.time <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.dist05) %>% 
  distinct() %>% 
  mutate(commonname = "ASSEMBLAGE") %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(assemblage.dist05 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel) %>% 
  select(-data)

#################
# assemblages vs temperature
#################

# shelf-wide LMs 

# set up dataframes with edge data and corresponding temperatures
poldat.pooled.lm.df <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.dist95) %>% 
  distinct() %>% 
  left_join(all.temp.df, by=c('year'='year_match')) 
eqdat.pooled.lm.df <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.dist05) %>% 
  distinct() %>% 
  left_join(all.temp.df, by=c('year'='year_match')) 

# set up list of models for LMs with distance as response var
pol.pooled.lm.list <- list(
  soda <- "assemblage.dist95 ~ soda.year.month.mean",
  had <- "assemblage.dist95 ~ year.month.mean",
  cold <- "assemblage.dist95 ~ year.daily.01",
  warm <- "assemblage.dist95 ~ year.daily.99"
)

eq.pooled.lm.list <- list(
  soda <- "assemblage.dist05 ~ soda.year.month.mean",
  had <- "assemblage.dist05 ~ year.month.mean",
  cold <- "assemblage.dist05 ~ year.daily.01",
  warm <- "assemblage.dist05 ~ year.daily.99"
)

## run all LMs and extract the outputs in a table with AICs 
pol.pooled.lm.results <- tibble(model = pol.pooled.lm.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lm(., data = poldat.pooled.lm.df)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel) %>% 
  select(-fit) 

eq.pooled.lm.results <- tibble(model = eq.pooled.lm.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lm(., data = eqdat.pooled.lm.df)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel) %>% 
  select(-fit)

# Isotherm assemblage LMs

# make dataframes 
poldat.pooled.lm.df.iso <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.lat95, assemblage.edge.lat.hadisst, assemblage.edge.lat.soda) %>% 
  distinct() 

eqdat.pooled.lm.df.iso <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.lat05, assemblage.edge.lat.hadisst, assemblage.edge.lat.soda) %>% 
  distinct() 

# make model lists 
pol.pooled.lm.list.iso <- list(
  soda <- "assemblage.lat95 ~ assemblage.edge.lat.soda",
  had <- "assemblage.lat95 ~ assemblage.edge.lat.hadisst"
)

eq.pooled.lm.list.iso <- list(
  soda <- "assemblage.lat05 ~ assemblage.edge.lat.soda",
  had <- "assemblage.lat05 ~ assemblage.edge.lat.hadisst"
)

## run all LMs and extract the outputs in a table with AICs 
pol.pooled.lm.results.iso <- tibble(model = pol.pooled.lm.list.iso) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lm(., data = poldat.pooled.lm.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel) %>% 
  select(-fit) 

eq.pooled.lm.results.iso <- tibble(model = eq.pooled.lm.list.iso) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lm(., data = eqdat.pooled.lm.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel) %>% 
  select(-fit)

# Isotherm LMEs 

pol.lme.df.iso <- poldat.stats.iso %>% 
  dplyr::select(year, commonname, spp.lat95, est.edge.lat.hadisst, est.edge.lat.soda) %>% 
  distinct() 

eq.lme.df.iso <- eqdat.stats.iso %>% 
  dplyr::select(year, commonname, spp.lat05, est.edge.lat.hadisst, est.edge.lat.soda) %>% 
  distinct() 

pol.iso.lme.list <- list(
  soda <- "spp.lat95 ~ est.edge.lat.soda + (1|commonname)",
  had <- "spp.lat95 ~ est.edge.lat.hadisst+ (1|commonname)"
)

eq.iso.lme.list <- list(
  soda <- "spp.lat05 ~ est.edge.lat.soda + (1|commonname)",
  had <- "spp.lat05 ~ est.edge.lat.hadisst+ (1|commonname)"
)

# run all LMEMs and extract the outputs in a table with AICs 
pol.iso.lme.results <- tibble(model = pol.iso.lme.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lmerTest::lmer(., data = pol.lme.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, broom.mixed::tidy)) %>% 
  unnest(tidymodel) %>% 
  dplyr::select(-fit) 

eq.iso.lme.results <- tibble(model = eq.iso.lme.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lmer(., data = eq.lme.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel) %>% 
  dplyr::select(-fit)

# Make results data frame with all assemblage model results 
poldat.results.df <- rbind(pol.pooled.lm.results, pol.pooled.lm.results.iso) %>% 
  mutate(group = NA, effect = NA, df=NA) %>% 
  rbind(pol.iso.lme.results) %>% 
  mutate(model = as.character(model)) # necessary for saving the file 

eqdat.results.df <- rbind(eq.pooled.lm.results, eq.pooled.lm.results.iso) %>% 
  mutate(group = NA, effect = NA, df=NA) %>% 
  rbind(eq.iso.lme.results) %>% 
  mutate(model = as.character(model)) # necessary for saving the file 

write_csv(poldat.results.df, here("results","poleward_assemblage_models.csv"))
write_csv(eqdat.results.df, here("results","equatorward_assemblage_models.csv"))
