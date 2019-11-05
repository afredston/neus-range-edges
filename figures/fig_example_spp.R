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
  filter(commonname=="White hake")%>% 
  dplyr::select(year, spp.dist05, commonname) %>% 
  distinct()%>% 
  rename(edge.dist = spp.dist05)%>% 
  mutate(edgetype = "eq")

exdf <- rbind(pol.ex, eq.ex)
anndf <- data.frame(x=rep(1968, 2), y=rep(1150, 2), commonname=unique(exdf$commonname))

gg.spp.ex <- exdf %>% 
  ggplot(aes(x=year, y=edge.dist)) + 
  geom_line(color="darkorange") + 
  geom_point(color="darkorange",size=0.75) + 
  labs(x="Year", y="Edge Position (km)") +
  facet_wrap(~commonname, ncol=1) + 
  geom_text(data=anndf, aes(x=x, y=y, label=commonname), hjust=0) +
  scale_y_continuous(limits=c(400, 1200), breaks=seq(400, 1200, 200)) +
  scale_x_continuous(limits=c(min(eqdat.stats$year),max(eqdat.stats$year)), breaks=seq(min(eqdat.stats$year), max(eqdat.stats$year), 4)) +
  theme_bw() +
  theme(
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black"),
        strip.text.x = element_blank()) +
  NULL

ggsave(gg.spp.ex, filename=here("results","fig_example_spp.png"),width=8,height=5, scale=0.9, dpi=300)
