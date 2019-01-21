.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)



#import csv file of results
baseline <- read_csv("raw_baseline-spon.csv")

baseline <- baseline %>%
  mutate(age = as.factor(age))

#create default boxplot function
default.single.boxplot <- list(geom_boxplot(aes(fill = age)),
                               # geom_point(aes(colour = age)),
                               theme(plot.title = element_text(size=11, face='bold',hjust=0.5),
                                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                                     axis.title = element_text(size=24, face='bold'),
                                     axis.text = element_text(size=20, face='bold', color = 'black'),
                                     plot.margin = unit(c(1,0,1,0), "cm"),
                                     panel.spacing = unit(0, "lines"),
                                     panel.border = element_blank(),
                                     panel.background = element_rect(fill = "white"),
                                     panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
                                     legend.position = 'none'),
                               scale_fill_brewer(palette='Set1'))

#raw basleines (spon) - whole cortex
ggplot(baseline, aes(age,raw_baseline)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Baseline fluorescence') 
#+
# stat_compare_means(label = 'p.signif', comparisons = list(c('1', '5')), face = 'bold') 


ggsave("raw_baseline-spon.png", width = 15, height = 15, units = "cm")


shapiro.test(baseline$raw_baseline)

a <- aov(raw_baseline~age, data = baseline)
summary(a)
TukeyHSD(a)



#raw basleines (spon) - barrel
ggplot(baseline, aes(age,barrel_baselines)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Baseline fluorescence') 
#+
# stat_compare_means(label = 'p.signif', comparisons = list(c('1', '5')), face = 'bold') 


ggsave("raw_barrel-baseline-spon.png", width = 15, height = 15, units = "cm")

shapiro.test(baseline$barrel_baselines)

a <- aov(barrel_baselines~age, data = baseline)
summary(a)
TukeyHSD(a)
