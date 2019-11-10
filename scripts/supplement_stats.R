# generating tables for the supplement

library(here)
library(tidyverse)

# correlation between temperature datasets 

oisst.neus <- readRDS(here("processed-data","oisst_neus.rds"))
soda.neus <- readRDS(here("processed-data","soda_neus.rds"))
hadisst.neus <- readRDS(here("processed-data","hadisst_neus.rds"))

oisst.prep <- oisst.neus %>%
  filter(year_measured >= 1982,
         year_measured <= 2017) %>% 
  group_by(year_measured) %>% 
  mutate(oisst.daily.99 = quantile(sst, 0.99),
         oisst.daily.01 = quantile(sst, 0.01)) %>% 
  ungroup() %>% 
  dplyr::select(year_measured, oisst.daily.99, oisst.daily.01) %>% 
  distinct() 

soda.prep <- soda.neus %>%
  filter(year_measured >= 1982,
         year_measured <= 2017) %>% 
  group_by(year_measured) %>% 
mutate(soda.monthly.mean=mean(btemp)) %>%
  ungroup() %>%
  dplyr::select(year_measured, soda.monthly.mean) %>%
  distinct()

hadisst.prep <- hadisst.neus %>%
  filter(year_measured >= 1982,
         year_measured <= 2017) %>% 
  group_by(year_measured) %>% 
  mutate(hadisst.monthly.mean=mean(sst)) %>%
  ungroup() %>%
  dplyr::select(year_measured, hadisst.monthly.mean) %>%
  distinct()

temp.cors <- oisst.prep %>%
  left_join(soda.prep, by="year_measured") %>%
  left_join(hadisst.prep, by="year_measured") %>%
  dplyr::select(-year_measured) %>%
  cor()

write_csv(oisst.cor, here("results","supplement_temp_cors.csv"))