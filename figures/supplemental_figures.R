library(tidyverse)
library(here)
library(ggplot2)

poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds"))
eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds"))

# use the .neus files not the .stats ones because we don't want to get confused with the adjusted years for the spring survey here--just want the actual years and temperatures
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

oisst.summary <- read_rds(here("processed-data","oisst_neus.rds")) %>% 
  dplyr::select(-altitude, -bathymetry) %>% 
  group_by(year, month, x, y) %>% 
  mutate(
    cell.month.mean = mean(sst)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, month, x, y, cell.month.mean) %>% # trim down to resolution of other datasets 
  distinct() %>% 
  group_by(year) %>% 
  mutate(
    year.month.mean = mean(cell.month.mean),
    year.month.sd = sd(cell.month.mean),
    year.month.max = max(cell.month.mean),
    year.month.min = min(cell.month.mean)
  ) %>% 
  ungroup() %>% 
  dplyr::select(year, year.month.mean, year.month.sd, year.month.max, year.month.min) %>% 
  distinct()

# make figure showing temperature trends over time

gg.temperature <- ggplot() + 
  geom_line(data=soda.summary, aes(x=year, y=year.month.mean), color="navy", lwd=1.2) +
  geom_ribbon(data=soda.summary, aes(ymin=year.month.mean-year.month.sd, ymax=year.month.mean+year.month.sd, x=year), alpha = 0.3, fill="grey") +
  geom_line(data=soda.summary, aes(x=year, y=year.month.max), color="navy", linetype="dashed") +
  geom_line(data=soda.summary, aes(x=year, y=year.month.min), color="navy", linetype="dotted") +
  geom_line(data=hadisst.summary, aes(x=year, y=year.month.mean), color="orangered", lwd=1.2) +
  geom_ribbon(data=hadisst.summary, aes(ymin=year.month.mean-year.month.sd, ymax=year.month.mean+year.month.sd, x=year), alpha = 0.3, fill="grey") +
  geom_line(data=hadisst.summary, aes(x=year, y=year.month.max), color="orangered", linetype="dashed") +
  geom_line(data=hadisst.summary, aes(x=year, y=year.month.min), color="orangered", linetype="dotted") +
  geom_line(data=oisst.summary, aes(x=year, y=year.month.mean), color="violetred", lwd=1.2) +
  geom_ribbon(data=oisst.summary, aes(ymin=year.month.mean-year.month.sd, ymax=year.month.mean+year.month.sd, x=year), alpha = 0.3, fill="grey") +
  geom_line(data=oisst.summary, aes(x=year, y=year.month.max), color="violetred", linetype="dashed") +
  geom_line(data=oisst.summary, aes(x=year, y=year.month.min), color="violetred", linetype="dotted") +
  labs(x="Year", y="Temperature (Degrees C)") +
  scale_y_continuous(breaks=seq(-5, 30, 5), limits=c(-5, 30)) + 
  scale_x_continuous(breaks=seq(1968, 2018, 4), limits=c(1968, 2018)) + 
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL
ggsave(plot=gg.temperature, filename=here("results","figS1.png"), scale=0.8, width=11, height=8, dpi=300)

# make plots of species edges (distance) over time 
gg.pol.edges.dist <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.dist95) %>% 
  distinct() %>% 
  ggplot(aes(x=year, y=spp.dist95)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Poleward Edge Position Along Coastline (km)") +
  xlab("Year") +
  NULL
ggsave(plot=gg.pol.edges.dist, filename=here("results","figS2.png"), scale=1.2, width=11, height=7, dpi=300)


gg.eq.edges.dist <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, spp.dist05) %>% 
  distinct() %>% 
  ggplot(aes(x=year, y=spp.dist05)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Equatorward Edge Position Along Coastline (km)") +
  xlab("Year") +
  NULL
ggsave(plot=gg.eq.edges.dist, filename=here("results","figS3.png"), scale=1.2, width=11, height=11, dpi=300)

# plot species edge position (lat) vs isotherms 

gg.pol.edges.iso <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat95, est.edge.lat.soda, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  ggplot() +
  geom_line(aes(x=year, y=spp.lat95), color="grey39") +
  geom_line(aes(x=year, y=est.edge.lat.hadisst), color="navy") +
  geom_line(aes(x=year, y=est.edge.lat.soda), color="#56B4E9") +
  geom_point(aes(x=year, y=spp.lat95), color="grey39", fill="grey39", size=0.6) +
  geom_point(aes(x=year, y=est.edge.lat.hadisst), color="navy", fill="navy", size=0.6) +
  geom_point(aes(x=year, y=est.edge.lat.soda), color="#56B4E9", fill="#56B4E9", size=0.6) +
  scale_color_manual(labels=c("Species","SST","SBT"), values=c('grey39','navy','#56B4E9')) +
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(31,49),breaks=seq(32,48,4)) +
  theme(legend.title=element_blank(), 
        legend.position=c(.1, .2),
        axis.text.x = element_text(angle=90)) +
  ylab("Latitude of Edge or Isotherm") +
  xlab("Year") +
  NULL
ggsave(plot=gg.pol.edges.iso, filename=here("results","figS4.png"), scale=1.2, width=11, height=7, dpi=300)

gg.eq.edges.iso <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, spp.lat05, est.edge.lat.soda, est.edge.lat.hadisst) %>% 
  distinct() %>% 
  ggplot() +
  geom_line(aes(x=year, y=spp.lat05), color="grey39") +
  geom_line(aes(x=year, y=est.edge.lat.hadisst), color="navy") +
  geom_line(aes(x=year, y=est.edge.lat.soda), color="#56B4E9") +
  geom_point(aes(x=year, y=spp.lat05), color="grey39", fill="grey39", size=0.6) +
  geom_point(aes(x=year, y=est.edge.lat.hadisst), color="navy", fill="navy", size=0.6) +
  geom_point(aes(x=year, y=est.edge.lat.soda), color="#56B4E9", fill="#56B4E9", size=0.6) +
  scale_color_manual(labels=c("Species","SST","SBT"), values=c('grey39','navy','#56B4E9')) +
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(30.9,49),breaks=seq(32,48,4)) +
  theme(legend.title=element_blank(), 
        legend.position=c(.1, .2),
        axis.text.x = element_text(angle=90)) +
  ylab("Latitude of Edge or Isotherm") +
  xlab("Year") +
  NULL
ggsave(plot=gg.eq.edges.iso, filename=here("results","figS5.png"), scale=1.2, width=11, height=11, dpi=300)

# plot species depth over time
gg.pol.depth <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, depth.mean.wt) %>% 
  distinct() %>% 
  ggplot(aes(x=year, y=depth.mean.wt)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(0, 350), breaks=seq(0, 300, 100)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Biomass-Weighted Mean Depth") +
  xlab("Year") +
  NULL
ggsave(plot=gg.pol.depth, filename=here("results","figS6.png"), scale=1.2, width=11, height=7, dpi=300)

gg.eq.depth <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, depth.mean.wt) %>% 
  distinct() %>% 
  ggplot(aes(x=year, y=depth.mean.wt)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(0, 350), breaks=seq(0, 300, 100)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Biomass-Weighted Mean Depth") +
  xlab("Year") +
  NULL
ggsave(plot=gg.eq.depth, filename=here("results","figS7.png"), scale=1.2, width=11, height=11, dpi=300)

# plot species biomass over time 

gg.pol.biomass <- poldat.stats.iso %>% 
  dplyr::select(commonname, year, biomass.correct.kg) %>% 
  distinct() %>% 
  mutate(biomass.correct.mt = biomass.correct.kg/1000) %>% 
  ggplot(aes(x=year, y=biomass.correct.mt)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4, scales="free") +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Total Biomass (tonnes)") +
  xlab("Year") +
  NULL
ggsave(plot=gg.pol.biomass, filename=here("results","figS8.png"), scale=1.2, width=11, height=7, dpi=300)

gg.eq.biomass <- eqdat.stats.iso %>% 
  dplyr::select(commonname, year, biomass.correct.kg) %>% 
  distinct() %>% 
  mutate(biomass.correct.mt = biomass.correct.kg/1000) %>% 
  ggplot(aes(x=year, y=biomass.correct.mt)) +
  geom_line(color="grey39") +
  geom_point(size=0.75) + 
  facet_wrap(~ commonname, ncol=4, scales="free") +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(1968,2017), breaks=seq(1968, 2017, 4)) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Total Biomass (tonnes)") +
  xlab("Year") +
  NULL
ggsave(plot=gg.eq.biomass, filename=here("results","figS9.png"), scale=1.2, width=11, height=11, dpi=300)