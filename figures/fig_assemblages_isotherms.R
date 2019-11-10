library(here)
library(tidyverse)
library(purrr)
library(broom)
library(gridExtra)

# load in datasets 
poldat.stats.iso <- readRDS(here("processed-data","poldat.stats.iso.rds")) 

poldat.assembl <- poldat.stats.iso %>% 
  dplyr::select(assemblage.edge.lat.hadisst, assemblage.edge.lat.soda, assemblage.lat95, year) %>% 
  distinct() %>% 
  gather("grouptype","value",-year)

eqdat.stats.iso <- readRDS(here("processed-data","eqdat.stats.iso.rds"))

eqdat.assembl <- eqdat.stats.iso %>% 
  dplyr::select(assemblage.edge.lat.hadisst, assemblage.edge.lat.soda, assemblage.lat05, year) %>% 
  distinct()%>% 
  gather("grouptype","value",-year)

# make assemblage edge plots 
# plotting help: https://community.rstudio.com/t/ggplot2-change-legend-title-while-controlling-line-types-and-colors/14966/2

ggcolors <- c('navy','#56B4E9','darkorange')
gglines <- c('1131','1111','solid')
gglabels1 <- c("SST Isotherm","SBT Isotherm","Cold Edge Assemblage")
gglabels2 <- c("SST Isotherm","SBT Isotherm","Warm Edge Assemblage")

poldat.assembl.gg <- poldat.assembl %>% 
  ggplot(aes(x=year, y=value, group=grouptype)) +
  geom_line(aes(linetype=grouptype, color=grouptype), size=1.2) +
  scale_color_manual(values = ggcolors, labels=gglabels1) +
  scale_linetype_manual(values = gglines, labels=gglabels1) +
  scale_x_continuous(limits=c(1968, 2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(37, 47), breaks=seq(38, 46, 1)) +
  labs(x="Year", y="Latitude") +
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(.3, .8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

eqdat.assembl.gg <- eqdat.assembl %>% 
  ggplot(aes(x=year, y=value, group=grouptype)) +
  geom_line(aes(linetype=grouptype, color=grouptype), size=1.2) +
  scale_color_manual(values = ggcolors, labels=gglabels2) +
  scale_linetype_manual(values = gglines, labels=gglabels2) +
  scale_x_continuous(limits=c(1968, 2017), breaks=seq(1968, 2017, 4)) +
  scale_y_continuous(limits=c(37, 47), breaks=seq(38, 46, 1)) +
  labs(x="Year", y="Latitude") +
  theme_bw() +
  theme(legend.title=element_blank(), 
        legend.position=c(.3, .8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

fig_assemblages_isotherms <- grid.arrange(poldat.assembl.gg, eqdat.assembl.gg, ncol=2)
ggsave(fig_assemblages_isotherms, width=11, height=5, dpi=300, filename=here("results","fig_assemblages_isotherms.png"))
