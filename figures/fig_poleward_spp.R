library(here)
library(tidyverse)
library(purrr)
library(broom)
library(gridExtra)
library(cowplot)

# load in datasets 
poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) 

# run lms for plot 
poldat.lm.time <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist95, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist95 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  ungroup() %>%
  unnest(tidymodel) %>% 
  select(-model, -data) %>% 
  filter(term=="year")

poldat.lm.sst <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat95 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  ungroup() %>%
  unnest(tidymodel) %>% 
  select(-model, -data) %>% 
  filter(term=="est.edge.lat.hadisst")

poldat.lm.sbt <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  ungroup() %>%
  unnest(tidymodel) %>% 
  select(-model, -data) %>% 
  filter(term=="est.edge.lat.soda")

# make forest plots of edge position vs time 
polspp.gg.time <- poldat.lm.time %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05","p > 0.05")) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#3A4ED0','p > 0.05'='grey85')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="Edge Shift (km/yr) \n                 ") +
  scale_y_continuous(breaks=seq(-30, 20, 10), limits=c(-35, 20)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=12,color="black"),
        plot.margin=unit(c(5.5, 15, 5.5, 5.5), "points")) +
  NULL

# make forest plots of edge position vs isotherm position 
polspp.gg.sst <- poldat.lm.sst %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05","p > 0.05")) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#3A4ED0', 'p > 0.05'='grey85')) +
  geom_hline(yintercept=0, color="black") +
  geom_hline(yintercept=1, color="grey40", linetype="dashed") +
  labs(x=NULL, y="Edge Shift (°lat) per 1°lat \nSST Isotherm Shift") +
  scale_y_continuous(breaks=seq(-2, 2, 1), limits=c(-2.1,2.1)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=12,color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  NULL

polspp.gg.sbt <- poldat.lm.sbt %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05","p > 0.05")) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#3A4ED0','p > 0.05'='grey85')) +
  geom_hline(yintercept=0, color="black") +
  geom_hline(yintercept=1, color="grey40", linetype="dashed") +
  labs(x=NULL, y="Edge Shift (°lat) per 1°lat \nSBT Isotherm Shift") +
  scale_y_continuous(breaks=seq(-2, 2, 1), limits=c(-2.1,2.1)) + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=12,color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  NULL

polspp.lm.gg <- cowplot::plot_grid(polspp.gg.time, polspp.gg.sst, polspp.gg.sbt, align = "h", nrow = 1, rel_widths = c(0.44, 0.28, 0.28))
ggsave(polspp.lm.gg, width=11, height=7, dpi=300, filename=here("results","fig_poleward_spp.png"))
