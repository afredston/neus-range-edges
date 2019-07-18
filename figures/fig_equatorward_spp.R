library(here)
library(tidyverse)
library(purrr)
library(broom)
library(gridExtra)

# load in datasets 
eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  filter(!commonname=="Greater argentine") 


# run glms for plot 
eqdat.glm.time <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.dist05, year) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~glm(spp.dist05 ~ year, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="year")

eqdat.glm.sst <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~glm(spp.lat05 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.hadisst")

eqdat.glm.sbt <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~glm(spp.lat05 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")

# make forest plots of edge position vs time 
eqspp.gg.time <- eqdat.glm.time %>% 
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
eqspp.gg.sst <- eqdat.glm.sst %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#006837', 'p < 0.10'='#51C574', 'p > 0.10'='grey85')) +
  geom_hline(yintercept=0, color="black") +
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
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

eqspp.gg.sbt <- eqdat.glm.sbt %>% 
  arrange(desc(commonname)) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange(size=0.7) +
  scale_color_manual(values=c('p < 0.05'='#006837', 'p < 0.10'='#51C574', 'p > 0.10'='grey85')) +
  geom_hline(yintercept=0, color="black") +
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
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

eqspp.glm.gg <- grid.arrange(eqspp.gg.time, eqspp.gg.sst, eqspp.gg.sbt, ncol=3)
ggsave(eqspp.glm.gg, width=11, height=7, dpi=300, filename=here("results","fig_equatorward_spp.png"))
