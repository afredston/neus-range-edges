
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra) 

soda.neus <- readRDS(here("processed-data","soda_neus.rds"))

lm.soda <- soda.neus %>% 
  nest(-year_match) %>% 
  mutate(
    model = purrr::map(data, ~lm(btemp ~ y, data = .x)), 
    tidymodel = purrr::map(model, tidy)
  ) %>% 
  unnest(tidymodel, .drop=TRUE)
get.soda.lat <- function(temp, year) {
  tmp <- lm.soda[lm.soda$year_match==year,]
  out <- (temp-tmp[[1,3]])/tmp[[2,3]]
  return(out)
}
get.soda.temp <- function(lat, year) {
  tmp <- lm.soda[lm.soda$year_match==year,]
  out <- lat*tmp[[2,3]]+tmp[[1,3]]
  return(out)
}


soda.1981 <- soda.neus %>% 
  filter(year_measured==1981) %>% 
  ggplot(aes(x=y, y=btemp)) + 
  geom_point(shape=4) + 
  geom_abline(intercept=26.9081886, slope=-0.4858256, color="#56B4E9", size=1.2) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(35, 45), breaks=seq(35, 45, 2)) +
  scale_y_continuous(limits=c(0, 30), breaks=seq(0, 30, 5)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title="1981", x="Latitude", y="Bottom Temperature") + 
  theme_bw() +
  theme(
        legend.position=NULL,
        plot.title = element_text(margin = margin(t = 10, b = -20), hjust=0.1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=12),
        axis.title=element_text(family="sans",size=12,color="black"),
     #   axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

soda.1982 <- soda.neus %>% 
  filter(year_measured==1982) %>% 
  ggplot(aes(x=y, y=btemp)) + 
  geom_point(shape=4) + 
  geom_abline(intercept=24.7227497, slope=-0.4316357, color="#56B4E9", size=1.2) +
  theme_linedraw() +
  theme(strip.background =element_rect(fill="grey39"))+
  theme(strip.text = element_text(colour = 'white', face="bold")) +
  scale_x_continuous(limits=c(35, 45), breaks=seq(35, 45, 2)) +
  scale_y_continuous(limits=c(0, 30), breaks=seq(0, 30, 5)) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title="1982", x="Latitude", y="Bottom Temperature") + 
  theme_bw() +
  theme(
    legend.position=NULL,
    plot.title = element_text(margin = margin(t = 10, b = -20), hjust=0.1), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    text=element_text(family="sans",size=12,color="black"),
    legend.text = element_text(size=12),
    axis.title=element_text(family="sans",size=12,color="black"),
    #   axis.text.x = element_text(angle = 90, hjust = 1), 
    axis.text=element_text(family="sans",size=8,color="black")) +
  NULL

figure0 <- grid.arrange(soda.1981, soda.1982, ncol=2)
ggsave(figure0, width=8, height=4, dpi=300, filename=here("results","fig0.png"))
