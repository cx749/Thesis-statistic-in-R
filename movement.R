.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)



#import csv file of results
movement <- read_csv("time-moving.csv")

movement <- movement %>%
  mutate(age = as.factor(age), session = as.factor(session))

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


#time moving with age (spon)
ggplot(movement, aes(session,time_moving)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Time moving (%)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold') +
  facet_grid(~age)


ggsave("time-moving_spon.png", width = 15, height = 15, units = "cm")


#trimmed


#import csv file of results
movement_t <- read_csv("trimmed-time-moving.csv")

movement_t <- movement_t %>%
  mutate(age = as.factor(age), session = as.factor(session), group = as.factor(group))

#create default trimmed boxplot function
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
                               scale_fill_brewer(palette='Dark2'))

#time moving
ggplot(movement_t, aes(age,time.moving, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Time moving (%)')

ggsave("trimmed-time-moving.png", width = 15, height = 15, units = "cm")


movement_tw <- read_csv("trimmed-time-moving-whisk.csv")

movement_tw <- movement_tw %>%
  mutate(age = as.factor(age), group = as.factor(group))

ggplot(movement_tw, aes(age,still, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Percentage still stimulations')

ggsave("trimmed-time-moving-whisk.png", width = 15, height = 15, units = "cm")

shapiro.test(movement_tw$still)

a <- aov(still ~ group + age, data = movement_tw)
summary(a)