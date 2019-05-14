library(here)
library(tidyverse)
library(purrr)
library(broom)

# load in datasets 
poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 
eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds"))%>% 
  mutate(year = as.numeric(year)) 
btemp.stats <- readRDS(here("processed-data","soda_stats.rds")) 
btemp.stats.overall <- btemp.stats %>% 
  dplyr::select(btemp.year.mean, btemp.year.min, btemp.year.max, year_match) %>% 
  distinct()

# run lms for plot 
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

# make forest plots of edge position vs time 
poldat.lm.gg <- poldat.lm %>% 
  arrange(estimate) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value<=0.05,"yes","no")) %>% 
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_colour_manual(values=c('yes'='#0072B2', 'no'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Poleward Edge Shift (km/yr)") +
  scale_y_continuous(breaks=seq(-35, 35, 5), limits=c(-35, 35)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

eqdat.lm.gg <- eqdat.lm %>% 
  arrange(estimate) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value<=0.05,"yes","no")) %>% 
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_colour_manual(values=c('yes'='#0072B2', 'no'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Equatorward Edge Shift (km/yr)") +
  scale_y_continuous(breaks=seq(-35, 35, 5), limits=c(-35, 35)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

# make assemblage edge plots 

poldat.assemblage.gg <- poldat.stats.iso %>% 
  filter(year >= min(btemp.stats.overall$year_match)) %>% 
  left_join(btemp.stats.overall, by=c('year'='year_match')) %>% 
  ggplot() +
  geom_line(aes(x=year, y=btemp.year.mean*100),color="#0072B2",size=3) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Mean Annual Sea Bottom Temperature (C)")) +
  geom_line(aes(x=year, y=spp.dist95, group=commonname), color="#999999") +
  geom_line(aes(x=year, y=assemblage.dist95), color="#D55E00", size=3) +  
  scale_x_continuous(limits=c(min(btemp.stats.overall$year_match),max(btemp.stats.overall$year_match)), breaks=seq(min(btemp.stats.overall$year_match), max(btemp.stats.overall$year_match), 4)) +
  labs(x="Year", y="Assemblage Edge Position (km)") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL
  

