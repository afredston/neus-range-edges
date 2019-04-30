library(here)
library(tidyverse)
library(sf)

coastdistdat <- readRDS(here("processed-data","coastdistdat.rds"))
poldat <- readRDS(here("processed-data","poldat.rds"))
eqdat <- readRDS(here("processed-data","eqdat.rds"))

