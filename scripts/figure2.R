library(here)
library(tidyverse)
library(purrr)
library(broom)
library(gridExtra)

# load in datasets 
poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 
eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds"))%>% 
  mutate(year = as.numeric(year)) 


# use the .neus files not the .stats ones because we don't want to get confused with the adjusted years for the spring survey here--just want the actual years and temperatures
hadisst.summary <- read_rds(here("processed-data","hadisst_neus.rds"))%>% 
  group_by(year) %>% 
  mutate(
    year.month.mean = mean(sst),
    year.month.sd = sd(sst),
    year.month.max = max(sst),
    year.month.min = min(sst)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, year.month.mean, year.month.sd, year.month.max, year.month.min) %>% 
  distinct()
soda.summary <- read_rds(here("processed-data","soda_neus.rds")) %>% 
  group_by(year) %>% 
  mutate(
    year.month.mean = mean(btemp),
    year.month.sd = sd(btemp),
    year.month.max = max(btemp),
    year.month.min = min(btemp)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, year.month.mean, year.month.sd, year.month.max, year.month.min) %>% 
  distinct()


# make assemblage edge plots 

temp.gg <- hadisst.summary %>% 
  mutate(source = "SST") %>% 
  full_join(soda.summary, by=c('year','year.month.mean','year.month.sd','year.month.max','year.month.min')) %>% 
  mutate(source = replace_na(source, "SBT")) %>% 
  ggplot() +
  geom_line(aes(x=year, y=year.month.mean, group=source, color=source),size=1.2) +
  scale_y_continuous(limits=c(6, 16), breaks=seq(6, 16, 2)) +
  scale_x_continuous(limits=c(min(hadisst.summary$year),max(hadisst.summary$year)), breaks=seq(min(hadisst.summary$year), max(hadisst.summary$year), 4)) +
  scale_color_manual(values=c('#56B4E9','navy')) +
  labs(x="Year", y="Mean Annual Temperature (C)") +
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(.1, .2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

pol.assemb.gg <- poldat.stats.iso %>% 
  ggplot(aes(x=year, y=assemblage.dist95)) +
  geom_line(size=1.2, color="darkorange") +
  scale_y_continuous(limits=c(400, 1600), breaks=seq(400, 1600, 200)) +
  scale_x_continuous(limits=c(min(poldat.stats.iso$year),max(poldat.stats.iso$year)), breaks=seq(min(poldat.stats.iso$year), max(poldat.stats.iso$year), 4)) +
  labs(title="Poleward Assemblage",x="Year", y="Edge Position (km)") +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(margin = margin(t = 10, b = -20)), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

eq.assemb.gg <- eqdat.stats.iso %>% 
  ggplot(aes(x=year, y=assemblage.dist05)) +
  geom_line(size=1.2, color="darkorange") +
  scale_y_continuous(limits=c(400, 1600), breaks=seq(400, 1600, 200)) +
  scale_x_continuous(limits=c(min(eqdat.stats.iso$year),max(eqdat.stats.iso$year)), breaks=seq(min(eqdat.stats.iso$year), max(eqdat.stats.iso$year), 4)) +
  labs(title="Equatorward Assemblage",x="Year", y="Edge Position (km)") +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(margin = margin(t = 10, b = -20)), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

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
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_color_manual(values=c('p < 0.05'='#009E73', 'p < 0.10'='#7EC3A2', 'p > 0.10'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Poleward Edge Shift (km/yr)") +
  scale_y_continuous(breaks=seq(-35, 20, 5), limits=c(-35, 20)) + 
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
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_color_manual(values=c('p < 0.05'='#009E73', 'p < 0.10'='#7EC3A2', 'p > 0.10'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Equatorward Edge Shift (km/yr)") +
  scale_y_continuous(breaks=seq(-35, 20, 5), limits=c(-35, 20)) + 
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


# couldn't get grid.arrange or cowplot to arrange these correctly--keep working on it! 
fig2A <- grid.arrange(temp.gg, pol.assemb.gg, eq.assemb.gg, ncol=1)
ggsave(fig2A, filename=here("results","fig2_part1.png"),height=8, width=4, dpi=300, scale=1.1)
fig2B <- grid.arrange(poldat.lm.gg, eqdat.lm.gg, ncol=2)
ggsave(fig2B, filename=here("results","fig2_part2.png"),height=8, width=7, dpi=300, scale=0.9)
