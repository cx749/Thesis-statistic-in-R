.libPaths("O:/paths") #set library path
library(tidyverse)
library(PMCMR)

kin <- read_csv("spon-stim_kinetics.csv")

kin <- kin %>%
  mutate(age = factor(age), group = as.factor(group))

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


ggplot(kin, aes(group,max_dvdt)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(y = 'Max rate of rise (df/f%/s)') 


ggsave("spon-stim_rise-time.png", width = 15, height = 15, units = "cm")


ggplot(kin, aes(group,min_dvdt)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(NA, 0)) + 
  labs(y = 'Max rate of fall (df/f%/s)') 

ggsave("spon-stim_fall-time.png", width = 15, height = 15, units = "cm")


ggplot(kin, aes(group,amp)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(y = 'Event amplitude (df/f %)') 


ggsave("spon-stim_amp.png", width = 15, height = 15, units = "cm")


#max values

kin <- read_csv("spon-stim_kinetics-max.csv")

kin <- kin %>%
  mutate(age = factor(age), group = as.factor(group))

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


ggplot(kin, aes(group,max_max_dvdt)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(y = 'Max rate of rise (df/f%/s)') 


ggsave("spon-stim_rise-time-max.png", width = 15, height = 15, units = "cm")


ggplot(kin, aes(group,max_min_dvdt)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(NA, 0)) + 
  labs(y = 'Max rate of fall (df/f%/s)') 

ggsave("spon-stim_fall-time-max.png", width = 15, height = 15, units = "cm")


ggplot(kin, aes(group,max_amp)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(y = 'Event amplitude (df/f %)') 


ggsave("spon-stim_amp-max.png", width = 15, height = 15, units = "cm")


#STATS?????
shapiro.test(kin$max_amp)
hist(kin$max_amp)

a<-aov(max_amp ~ age + group, data = kin)
summary(a)
TukeyHSD(a)
pairwise.wilcox.test(kin$max_amp, kin$group, p.adjust.method = 'BH')

kruskal.test(max_amp ~ age, data = whisk_stim)
posthoc.kruskal.nemenyi.test(still.peak ~ age, data = whisk_stim, dist="Tukey")

friedman.test(max_amp~age|group, data = kin)
