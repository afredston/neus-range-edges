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
  dplyr::select(year_match, year.month.mean, year.month.sd) %>% 
  distinct() # get rid of lat-specific stats

hadisst.stats <- readRDS(here("processed-data","hadisst_stats.rds")) 

hadisst.stats.summary <- hadisst.stats %>% 
  dplyr::select(year_match, year.month.mean, year.month.sd) %>% 
  distinct() # get rid of lat-specific stats

oisst.extremes <- readRDS(here("processed-data","oisst_neus.rds")) %>% 
  filter(year_match > 1982) %>% 
  group_by(year_match) %>% 
  mutate(
    year.daily.mean = mean(sst),
    year.daily.99 = quantile(sst, 0.99),
    year.daily.01 = quantile(sst, 0.01)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year_match, year.daily.99, year.daily.01, year.daily.mean) %>% 
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
# models of edges and temperature
#################

# shelf-wide GLMs 

## make dataframe of rescaled temperature statistics 
temps.scale <- soda.stats.summary %>% 
  dplyr::select(year_match, year.month.mean) %>% 
  rename(soda.year.month.mean = year.month.mean) %>% 
  full_join(hadisst.stats.summary, by="year_match") %>% 
  left_join(oisst.extremes, by="year_match") %>% 
  mutate_at(vars(-year_match), scale)

## make unscaled df for back-transforming predictor variables later 
temps.unscale <- soda.stats.summary %>% 
  dplyr::select(year_match, year.month.mean) %>% 
  rename(soda.year.month.mean = year.month.mean) %>% 
  full_join(hadisst.stats.summary, by="year_match") %>% 
  left_join(oisst.extremes, by="year_match") 

## make all GLMs 

# set up dataframes with edge data and corresponding temperatures
poldat.glm.df <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.dist95) %>% 
  distinct() %>% 
  left_join(temps.scale, by=c('year'='year_match')) 
eqdat.glm.df <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.dist05) %>% 
  distinct() %>% 
  left_join(temps.scale, by=c('year'='year_match')) 

# set up list of models for GLMs with distance as response var
pol.glm.list <- list(
  soda <- "assemblage.dist95 ~ soda.year.month.mean",
  had <- "assemblage.dist95 ~ year.month.mean",
  full <- "assemblage.dist95 ~ soda.year.month.mean + year.daily.mean + year.daily.99 + year.daily.01 + soda.year.month.mean*year.daily.mean + year.daily.mean*year.daily.99"
)

eq.glm.list <- list(
  soda <- "assemblage.dist05 ~ soda.year.month.mean",
  had <- "assemblage.dist05 ~ year.month.mean",
  full <- "assemblage.dist05 ~ soda.year.month.mean + year.daily.mean + year.daily.99 + year.daily.01 + soda.year.month.mean*year.daily.mean + year.daily.mean*year.daily.99"
)

## run all GLMs and extract the outputs in a table with AICs 
pol.glm.frame <- tibble(model = pol.glm.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~glm(., data = poldat.glm.df)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel, .drop=FALSE) %>% 
  dplyr::select(-fit)

eq.glm.frame <- tibble(model = eq.glm.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~glm(., data = eqdat.glm.df)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel, .drop=FALSE) %>% 
  dplyr::select(-fit)

# Isotherm GLMs

# make dataframes 
poldat.glm.df.iso <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.lat95, assemblage.edge.lat.hadisst, assemblage.edge.lat.soda) %>% 
  distinct() %>% 
  mutate_at(vars(-year, -assemblage.lat95), scale) 

eqdat.glm.df.iso <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.lat05, assemblage.edge.lat.hadisst, assemblage.edge.lat.soda) %>% 
  distinct() %>% 
  mutate_at(vars(-year, -assemblage.lat05), scale) 

# make model lists 
pol.iso.glm.list <- list(
  soda <- "assemblage.lat95 ~ assemblage.edge.lat.soda",
  had <- "assemblage.lat95 ~ assemblage.edge.lat.hadisst"
)

eq.iso.glm.list <- list(
  soda <- "assemblage.lat05 ~ assemblage.edge.lat.soda",
  had <- "assemblage.lat05 ~ assemblage.edge.lat.hadisst"
)


## run all GLMs and extract the outputs in a table with AICs 
pol.iso.glm.frame <- tibble(model = pol.iso.glm.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~glm(., data = poldat.glm.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel, .drop=FALSE) %>% 
  dplyr::select(-fit)

eq.iso.glm.frame <- tibble(model = eq.iso.glm.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~glm(., data = eqdat.glm.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel, .drop=FALSE) %>% 
  dplyr::select(-fit)

# Isotherm LMEs 

pol.lme.df.iso <- poldat.stats.iso %>% 
  dplyr::select(year, commonname, spp.lat95, est.edge.lat.hadisst, est.edge.lat.soda) %>% 
  distinct() %>% 
  mutate_at(vars(-year, -commonname, -spp.lat95), scale)

eq.lme.df.iso <- eqdat.stats.iso %>% 
  dplyr::select(year, commonname, spp.lat05, est.edge.lat.hadisst, est.edge.lat.soda) %>% 
  distinct() %>% 
  mutate_at(vars(-year, -commonname, -spp.lat05), scale) 

pol.iso.lme.list <- list(
  soda <- "spp.lat95 ~ est.edge.lat.soda + (1|commonname)",
  had <- "spp.lat95 ~ est.edge.lat.hadisst+ (1|commonname)"
)

eq.iso.lme.list <- list(
  soda <- "spp.lat05 ~ est.edge.lat.soda + (1|commonname)",
  had <- "spp.lat05 ~ est.edge.lat.hadisst+ (1|commonname)"
)

# run all GLMs and extract the outputs in a table with AICs 
pol.iso.lme.frame <- tibble(model = pol.iso.lme.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lmer(., data = pol.lme.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel, .drop=FALSE) %>% 
  dplyr::select(-fit) %>% 
  mutate(p.value = NA) # for joining to results df 

eq.iso.lme.frame <- tibble(model = eq.iso.lme.list) %>%
  mutate(model.name = names(model),
         model = map(model, as.formula),
         fit = map(model, ~lmer(., data = eq.lme.df.iso)),
         aic = map_dbl(fit, ~AIC(.)), 
         tidymodel = map(fit, tidy)) %>% 
  unnest(tidymodel, .drop=FALSE) %>% 
  dplyr::select(-fit)%>% 
  mutate(p.value = NA) # for joining to results df 

# Make results data frame with all model results 
poldat.results.df <- rbind(pol.glm.frame, pol.iso.glm.frame) %>% 
  mutate(group = NA) %>% 
  rbind(pol.iso.lme.frame) %>% 
  mutate(model = as.character(model)) # necessary for saving the file 

eqdat.results.df <- rbind(eq.glm.frame, eq.iso.glm.frame) %>% 
  mutate(group = NA) %>% 
  rbind(eq.iso.lme.frame)%>% 
  mutate(model = as.character(model)) # necessary for saving the file 

write_csv(poldat.results.df, here("results","poleward_assemblage_models.csv"))
write_csv(eqdat.results.df, here("results","equatorward_assemblage_models.csv"))
write_csv(temps.unscale, here("results", "unscaled_temp_predictors.csv"))

# Species isotherms 

eqdat.iso.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  mutate(est.edge.lat.soda = scale(est.edge.lat.soda)) %>% 
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
  mutate(est.edge.lat.soda = scale(est.edge.lat.soda)) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.lat95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")
