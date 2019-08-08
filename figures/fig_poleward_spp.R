library(here)
library(tidyverse)
library(purrr)
library(broom)
library(gridExtra)
library(cowplot)

# load in datasets 
poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) 

# run models for plot 
poldat.glm.time <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~glm(spp.dist95 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

poldat.glm.sst <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~glm(spp.lat95 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.hadisst")

poldat.glm.sbt <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~glm(spp.lat95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")

# make forest plots of edge position vs time 
polspp.gg.time <- poldat.glm.time %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#006837', 'p < 0.10'='#51C574', 'p > 0.10'='grey85')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Edge Shift (km/yr) \n                 ") +
  scale_y_continuous(breaks=seq(-35, 20, 5), limits=c(-35, 20)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        plot.margin=unit(c(5.5, 15, 5.5, 5.5), "points")) +
  NULL

# make forest plots of edge position vs isotherm position 
polspp.gg.sst <- poldat.glm.sst %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#006837', 'p < 0.10'='#51C574', 'p > 0.10'='grey85')) +
  geom_hline(yintercept=0, color="black") +
  geom_hline(yintercept=1, color="grey40", linetype="dashed") +
  labs(x=NULL, y="Edge Shift (°lat) per 1°lat \nSST Isotherm Shift") +
  scale_y_continuous(breaks=seq(-2, 2, 1), limits=c(-2,2)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  NULL

polspp.gg.sbt <- poldat.glm.sbt %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#006837', 'p < 0.10'='#51C574', 'p > 0.10'='grey85')) +
  geom_hline(yintercept=0, color="black") +
  geom_hline(yintercept=1, color="grey40", linetype="dashed") +
  labs(x=NULL, y="Edge Shift (°lat) per 1°lat \nSBT Isotherm Shift") +
  scale_y_continuous(breaks=seq(-2, 2, 1), limits=c(-2,2)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  NULL

polspp.glm.gg <- cowplot::plot_grid(polspp.gg.time, polspp.gg.sst, polspp.gg.sbt, align = "h", nrow = 1, rel_widths = c(0.4, 0.3, 0.3))
ggsave(polspp.glm.gg, width=11, height=7, dpi=300, filename=here("results","fig_poleward_spp.png"))


