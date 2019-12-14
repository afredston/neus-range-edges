library(here)
library(tidyverse)
library(purrr)
library(broom)
library(gridExtra)
library(cowplot)

# load in datasets 
eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) 

# run lms for plot 
eqdat.lm.time <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist05, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.dist05 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  ungroup() %>%
  unnest(tidymodel) %>% 
  select(-model, -data) %>% 
  filter(term=="year")

eqdat.lm.sst <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat05 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  ungroup() %>%
  unnest(tidymodel) %>% 
  select(-model, -data) %>% 
  filter(term=="est.edge.lat.hadisst")

eqdat.lm.sbt <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat05 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  ungroup() %>%
  unnest(tidymodel) %>% 
  select(-model, -data) %>% 
  filter(term=="est.edge.lat.soda")

# make forest plots of edge position vs time 
eqspp.gg.time <- eqdat.lm.time %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05","p > 0.05")) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#C7361D','p > 0.05'='grey85')) +
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
eqspp.gg.sst <- eqdat.lm.sst %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05","p > 0.05")) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#C7361D', 'p > 0.05'='grey85')) +
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
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  NULL

eqspp.gg.sbt <- eqdat.lm.sbt %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05","p > 0.05")) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#C7361D','p > 0.05'='grey85')) +
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
        axis.title=element_text(family="sans",size=10,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  NULL

eqspp.lm.gg <- cowplot::plot_grid(eqspp.gg.time, eqspp.gg.sst, eqspp.gg.sbt, align = "h", nrow = 1, rel_widths = c(0.4, 0.3, 0.3))
ggsave(eqspp.lm.gg, width=11, height=7, dpi=300, filename=here("results","fig_equatorward_spp.png"))
