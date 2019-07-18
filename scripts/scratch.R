poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

poldat.stats.assemblage <- poldat.stats.iso %>% 
  dplyr::select(year, assemblage.lat95, assemblage.edge.temp.hadisst, assemblage.edge.lat.hadisst, assemblage.edge.temp.soda, assemblage.edge.lat.soda) %>% 
  distinct()

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds")) %>% 
  mutate(year = as.numeric(year)) 

eqdat.stats.assemblage <- eqdat.stats.iso %>% 
  dplyr::select(year, assemblage.lat05, assemblage.edge.temp.hadisst, assemblage.edge.lat.hadisst, assemblage.edge.temp.soda, assemblage.edge.lat.soda) %>% 
  distinct()

eqdat.iso.lm <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.lat05 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")

poldat.iso.lm <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = map(data, ~lm(spp.lat95 ~ est.edge.lat.soda, data = .x)), 
    tidymodel = map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.soda")

gg.eq.assemblage.soda <- ggplot(data=eqdat.stats.assemblage) + geom_line(aes(x=year, y=assemblage.lat05), color="black") + geom_line(aes(x=year, y=assemblage.edge.lat.soda), color="red") +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Latitude") +
  xlab("Year") +
  ggtitle("Equatorward Edge Position (black) and Isotherm (red)") +
  NULL

gg.pol.assemblage.soda <- ggplot(data=poldat.stats.assemblage) + geom_line(aes(x=year, y=assemblage.lat95), color="black") + geom_line(aes(x=year, y=assemblage.edge.lat.soda), color="red")+
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Latitude") +
  xlab("Year") +
  ggtitle("Poleward Edge Position (black) and Isotherm (red)") +
  NULL
ggsave(grid.arrange(gg.pol.assemblage.soda, gg.eq.assemblage.soda, ncol=1), width=7, height=10, filename=here("results","explore1.png"))

gg.pol.spp.soda <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  left_join(poldat.iso.lm, by="commonname") %>% 
  mutate(signif = ifelse(p.value <= 0.10, "Y","N")) %>% 
  ggplot() +
  geom_line(aes(x=year, y=spp.lat95, color=signif)) +
  geom_point(aes(x=year, y=spp.lat95, color=signif, fill=signif), size=1) +
  scale_colour_manual(values = c("grey55","black")) +
  geom_line(aes(x=year, y=est.edge.lat.soda), color="red") +
  geom_point(aes(x=year, y=est.edge.lat.soda), size=1, color="red") +
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Latitude") +
  xlab("Year") +
  NULL

gg.eq.spp.soda <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  ggplot() +
  geom_line(aes(x=year, y=spp.lat05), color="grey39") +
  geom_point(aes(x=year, y=spp.lat05), color="grey39") +
  geom_line(aes(x=year, y=est.edge.lat.soda), color="red") +
  geom_point(aes(x=year, y=est.edge.lat.soda), color="red") +
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Latitude") +
  xlab("Year") +
  NULL

gg.eq.spp.ord <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat05, est.edge.lat.soda) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(aes(x=est.edge.lat.soda, y=spp.lat05, color=year, fill=year)) + 
  geom_abline(slope=1) + 
  scale_x_continuous(limits=c(32, 50)) +
  scale_y_continuous(limits=c(32, 44)) +
  facet_wrap(~ commonname)

gg.eq.spp.ord2 <- eqdat.stats.iso %>% 
  left_join(eqdat.iso.lm, by="commonname") %>% 
  dplyr::select(commonname, year, spp.lat05, est.edge.lat.soda, estimate, p.value, std.error, statistic, term) %>% 
  distinct() %>% 
  mutate(signif = ifelse(p.value <= 0.05, "Y","N")) %>% 
  ggplot() +
  geom_point(aes(x=est.edge.lat.soda, y=spp.lat05, color=signif, fill=signif)) + 
  geom_abline(slope=1) + 
  scale_x_continuous(limits=c(32, 50)) +
  scale_y_continuous(limits=c(32, 44)) +
  facet_wrap(~ commonname)

gg.pol.spp.ord <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(aes(x=est.edge.lat.soda, y=spp.lat95, color=year, fill=year)) + 
  geom_abline(slope=1) + 
  scale_x_continuous(limits=c(32, 50)) +
  scale_y_continuous(limits=c(32, 44)) +
  facet_wrap(~ commonname)

gg.pol.spp.ord2 <- poldat.stats.iso %>% 
  left_join(poldat.iso.lm, by="commonname") %>% 
  dplyr::select(commonname, year, spp.lat95, est.edge.lat.soda, estimate, p.value, std.error, statistic, term) %>% 
  distinct() %>% 
  mutate(signif = ifelse(p.value <= 0.05, "Y","N")) %>% 
  ggplot() +
  geom_point(aes(x=est.edge.lat.soda, y=spp.lat95, color=signif, fill=signif)) + 
  scale_x_continuous(limits=c(32, 50)) +
  scale_y_continuous(limits=c(32, 44)) +
  geom_abline(slope=1) + 
  facet_wrap(~ commonname)
  

# make species-specific plots

eqdat.iso.lm.had <- eqdat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat05, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  mutate(est.edge.lat.hadisst = scale(est.edge.lat.hadisst)) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat05 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.hadisst")

poldat.iso.lm.had <- poldat.stats.iso %>% 
  dplyr::select(latinname, commonname, spp.lat95, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  mutate(est.edge.lat.hadisst = scale(est.edge.lat.hadisst)) %>% 
  group_by(commonname) %>% 
  nest() %>% 
  mutate(
    model = purrr::map(data, ~lm(spp.lat95 ~ est.edge.lat.hadisst, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE) %>% 
  filter(term=="est.edge.lat.hadisst")

# make forest plots of edge position vs isotherm position 
poldat.iso.gg <- poldat.iso.lm.had %>% 
  arrange(estimate) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_color_manual(values=c('p < 0.05'='#009E73', 'p < 0.10'='#7EC3A2', 'p > 0.10'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="SST Isotherm Effect on \nPoleward Edge (°lat/yr)") +
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

eqdat.iso.gg <- eqdat.iso.lm.had %>% 
  filter(!commonname=="Greater argentine") %>% 
  arrange(estimate) %>% 
  mutate(commonname = factor(commonname, unique(commonname)),
         signif = ifelse(p.value <= 0.05, "p < 0.05", ifelse(p.value <= 0.10, "p < 0.10", "p > 0.10"))) %>%
  ggplot(aes(x=commonname, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error, color=signif, fill=signif)) +
  geom_pointrange() +
  scale_color_manual(values=c('p < 0.05'='#009E73', 'p < 0.10'='#7EC3A2', 'p > 0.10'='#999999')) +
  geom_hline(yintercept=0, color="black") +
  labs(x=NULL, y="SST Isotherm Effect on \nEquatorward Edge (°lat/yr)") +
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

