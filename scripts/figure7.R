library(here)
library(tidyverse)
library(purrr)
library(broom)

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds"))

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


pol.edge.tmp <- poldat.lm %>% 
  rename( 
    edge.est = estimate,
    edge.sd = std.error,
    edge.statistic = statistic, 
    edge.pvalue = p.value) 

pol.depth.tmp <- poldat.depth.lm %>% 
  rename(
    depth.est = estimate,
    depth.sd = std.error,
    depth.statistic = statistic,
    depth.pvalue = p.value) %>% 
  left_join(pol.edge.tmp, by=c('commonname','term')) 

pol.ord.tmp <- poldat.abund.lm %>% 
  rename(
    abund.est = estimate,
    abund.sd = std.error,
    abund.statistic = statistic,
    abund.pvalue = p.value) %>% 
  left_join(pol.depth.tmp, by=c('commonname','term')) 

eq.edge.tmp <- eqdat.lm %>% 
  rename(edge.est = estimate,
         edge.sd = std.error,
         edge.statistic = statistic, 
         edge.pvalue = p.value) 

eq.depth.tmp <- eqdat.depth.lm %>% 
  rename(depth.est = estimate,
         depth.sd = std.error,
         depth.statistic = statistic,
         depth.pvalue = p.value) %>% 
  full_join(eq.edge.tmp, by=c('commonname','term'))

eq.ord.tmp <- eqdat.abund.lm %>% 
  rename(abund.est = estimate,
         abund.sd = std.error,
         abund.statistic = statistic,
         abund.pvalue = p.value) %>% 
  left_join(eq.depth.tmp, by=c('commonname','term')) 

pol.depth.shift.gg <- ggplot() + 
  geom_hline(yintercept=0, color="darkgrey") + 
  geom_vline(xintercept=0, color="darkgrey") + 
  geom_point(data=pol.ord.tmp, aes(x=depth.est, y=edge.est), shape = 19) +
  geom_errorbar(data=pol.ord.tmp, aes(x=depth.est, ymin = edge.est-edge.sd, ymax = edge.est+edge.sd)) + 
  geom_errorbarh(data=pol.ord.tmp, aes(y=edge.est, xmin = depth.est-depth.sd,xmax = depth.est+depth.sd)) + 
  labs(x="Depth Shift (m/yr)", y="Poleward Edge Shift (km/yr)") + 
  scale_x_continuous(breaks=seq(-4, 4, 1), limits=c(-4,4)) +
  scale_y_continuous(breaks=seq(-35, 35, 5), limits=c(-35, 35)) + 
  annotate("text", x=-3.5, y=30, size=8, label="A") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +  
  NULL

eq.depth.shift.gg <- ggplot() + 
  geom_hline(yintercept=0, color="darkgrey") + 
  geom_vline(xintercept=0, color="darkgrey") + 
  geom_point(data=eq.ord.tmp, aes(x=depth.est, y=edge.est), shape = 15) +
  geom_errorbar(data=eq.ord.tmp, aes(x=depth.est, ymin = edge.est-edge.sd, ymax = edge.est+edge.sd)) + 
  geom_errorbarh(data=eq.ord.tmp, aes(y=edge.est, xmin = depth.est-depth.sd,xmax = depth.est+depth.sd)) + 
  labs(x="Depth Shift (m/yr)", y="Equatorward Edge Shift (km/yr)") + 
  scale_x_continuous(breaks=seq(-4, 4, 1), limits=c(-4,4)) +
  scale_y_continuous(breaks=seq(-35, 35, 5), limits=c(-35, 35)) + 
  annotate("text", x=-3.5, y=30, size=8, label="C") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

pol.abund.shift.gg <- ggplot() + 
  geom_hline(yintercept=0, color="darkgrey") + 
  geom_vline(xintercept=0, color="darkgrey") + 
  geom_errorbar(data=pol.ord.tmp, aes(x=abund.est, ymin = edge.est-edge.sd,ymax = edge.est+edge.sd)) + 
  geom_errorbarh(data=pol.ord.tmp, aes(y=edge.est, xmin = abund.est-abund.sd,xmax = abund.est+abund.sd)) + 
  geom_point(data=pol.ord.tmp, aes(x=abund.est, y=edge.est), shape=21, fill="white") +
  labs(x="Biomass Shift (mt/yr)", y="Edge Shift (km/yr)") + 
  scale_x_continuous(breaks=seq(-3500, 3500, 1000), limits=c(-3500, 3500)) +
  scale_y_continuous(breaks=seq(-35, 35, 5), limits=c(-35, 35)) + 
  annotate("text", x=-3300, y=30, size=8, label="B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  NULL

eq.abund.shift.gg <- ggplot() + 
  geom_hline(yintercept=0, color="darkgrey") + 
  geom_vline(xintercept=0, color="darkgrey") + 
  geom_errorbar(data=eq.ord.tmp, aes(x=abund.est, ymin = edge.est-edge.sd,ymax = edge.est+edge.sd)) + 
  geom_errorbarh(data=eq.ord.tmp, aes(y=edge.est, xmin = abund.est-abund.sd,xmax = abund.est+abund.sd)) + 
  geom_point(data=eq.ord.tmp, aes(x=abund.est, y=edge.est), shape=22, fill="white") +
  labs(x="Biomass Shift (mt/yr)", y="Edge Shift (km/yr)") + 
  scale_x_continuous(breaks=seq(-3500, 3500, 1000), limits=c(-3500, 3500)) +
  scale_y_continuous(breaks=seq(-35, 35, 5), limits=c(-35, 35)) + 
  annotate("text", x=-3300, y=30, size=8, label="D") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  NULL

fig7 <- cowplot::plot_grid(pol.depth.shift.gg, pol.abund.shift.gg, eq.depth.shift.gg, eq.abund.shift.gg, align="h",ncol=2, rel_widths = c(1,1,1,1), rel_heights = c(1,1,1,1))
ggsave(filename=here("results","fig7.png"), width=6, height=5, dpi=300)
