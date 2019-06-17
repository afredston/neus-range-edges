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



gg.eq.assemblage.soda <- ggplot(data=eqdat.stats.assemblage) + geom_line(aes(x=year, y=assemblage.lat05), color="black") + geom_line(aes(x=year, y=assemblage.edge.lat.soda), color="red")

gg.pol.assemblage.soda <- ggplot(data=poldat.stats.assemblage) + geom_line(aes(x=year, y=assemblage.lat95), color="black") + geom_line(aes(x=year, y=assemblage.edge.lat.soda), color="red")

gg.pol.spp.soda <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  ggplot() +
  geom_line(aes(x=year, y=spp.lat95), color="grey39") +
  geom_point(aes(x=year, y=spp.lat95), color="grey39") +
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
  facet_wrap(~ commonname)

gg.pol.spp.ord <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat95, est.edge.lat.soda) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(aes(x=est.edge.lat.soda, y=spp.lat95, color=year, fill=year)) + 
  geom_abline(slope=1) + 
  facet_wrap(~ commonname)
  