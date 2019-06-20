library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra) 

# load in data 
poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

# make assemblage-wide plots

poldat.stats.assemblage <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.lat95, assemblage.edge.temp.hadisst, assemblage.edge.lat.hadisst, assemblage.edge.temp.soda, assemblage.edge.lat.soda) %>% 
  distinct()

eqdat.stats.assemblage <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.lat05, assemblage.edge.temp.hadisst, assemblage.edge.lat.hadisst, assemblage.edge.temp.soda, assemblage.edge.lat.soda) %>% 
  distinct()

eq.assemb.soda.gg <- ggplot(data=eqdat.stats.assemblage) + 
  geom_line(aes(x=year, y=assemblage.lat05, color="darkorange"), size=1.2) + 
  geom_line(aes(x=year, y=assemblage.edge.lat.soda, color="#56B4E9"), size=1.2) +
  scale_color_manual(labels=c('Isotherm','Assemblage Edge'), values=c('#56B4E9','darkorange')) + 
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(37,47), breaks=seq(37, 47, 2)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title="Equatorward Assemblage", x="Year", y="Latitude") + 
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(.35, .75),
        plot.title = element_text(margin = margin(t = 10, b = -20), hjust=0.1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL


pol.assemb.soda.gg <- ggplot(data=poldat.stats.assemblage) + 
  geom_line(aes(x=year, y=assemblage.lat95, color="darkorange"), size=1.2) + 
  geom_line(aes(x=year, y=assemblage.edge.lat.soda, color="#56B4E9"), size=1.2) +
  scale_color_manual(labels=c('Isotherm','Assemblage Edge'), values=c('#56B4E9','darkorange')) + 
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(37,47), breaks=seq(37, 47, 2)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title="Poleward Assemblage", x="Year", y="Latitude") + 
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(.35, .75),
        plot.title = element_text(margin = margin(t = 10, b = -20), hjust=0.1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

# make species-specific plots

eqdat.iso.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  mutate(est.edge.lat.soda = scale(est.edge.lat.soda)) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat05 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
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
    model = purrr::map(data, ~lm(spp.lat95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")

# make forest plots of edge position vs isotherm position 
poldat.iso.gg <- poldat.iso.lm %>% 
  arrange(estimate) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_color_manual(values=c('p < 0.05'='#009E73', 'p < 0.10'='#7EC3A2', 'p > 0.10'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Isotherm Effect on \nPoleward Edge (°lat/yr)") +
  scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), limits=c(-1.52, 1.52)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

# WARNING! REMOVING DATA! greater argentine had a huge estimated effect (almost 4 degrees lat) and a high p-value based on a pretty small time-series, so I am just dropping it from the plot 

eqdat.iso.gg <- eqdat.iso.lm %>% 
  filter(!commonname=="Greater argentine") %>% 
  arrange(estimate) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_color_manual(values=c('p < 0.05'='#009E73', 'p < 0.10'='#7EC3A2', 'p > 0.10'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Isotherm Effect on \nEquatorward Edge (°lat/yr)") +
  scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), limits=c(-1.52, 1.52)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

fig5A <- grid.arrange(pol.assemb.soda.gg, eq.assemb.soda.gg, ncol=1)
ggsave(fig5A, filename=here("results","fig5_part1.png"),height=8, width=3, dpi=300, scale=1.1)
fig5B <- grid.arrange(poldat.iso.gg, eqdat.iso.gg, ncol=2)
ggsave(fig5B, filename=here("results","fig5_part2.png"),height=8, width=8, dpi=300, scale=0.8)
