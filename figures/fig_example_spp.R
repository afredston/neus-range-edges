library(tidyverse)
library(here)
library(ggplot2)
library(directlabels) 

poldat.stats <- readRDS(here("processed-data","poldat.stats.rds"))
eqdat.stats <- readRDS(here("processed-data","eqdat.stats.rds"))

pol.ex <- poldat.stats %>% 
  filter(commonname=="Black seabass") %>% 
  dplyr::select(year, spp.dist95, commonname) %>% 
  distinct() %>% 
  rename(edge.dist = spp.dist95) %>% 
  mutate(edgetype = "pol")

eq.ex <- eqdat.stats %>% 
  filter(commonname=="Silver hake")%>% 
  dplyr::select(year, spp.dist05, commonname) %>% 
  distinct()%>% 
  rename(edge.dist = spp.dist05)%>% 
  mutate(edgetype = "eq")

gg.spp.ex <- rbind(pol.ex, eq.ex) %>% 
  ggplot(aes(x=year, y=edge.dist, group=commonname, color = commonname)) + 
  geom_line() + 
  geom_point(size=0.75) + 
  scale_color_manual(values=c('darkorange','orangered')) +
  labs(x="Year", y="Edge Position (km)") +
  scale_y_continuous(limits=c(0, 1200), breaks=seq(0, 1200, 200)) +
  scale_x_continuous(limits=c(min(eqdat.stats$year),max(eqdat.stats$year)), breaks=seq(min(eqdat.stats$year), max(eqdat.stats$year), 4)) +
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(.15, .88),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

ggsave(gg.spp.ex, filename=here("results","fig_example_spp.png"),width=8,height=5, scale=0.9, dpi=300)
