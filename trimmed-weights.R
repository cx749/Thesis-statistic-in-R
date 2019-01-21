.libPaths("O:/paths") #set library path
library(tidyverse)

dfw<-read_csv("whisk-trimmed-weights.csv") #load csv


dfw <- dfw %>%
  mutate(weight = as.numeric(weight), perc_gain = as.numeric(perc_gain), age = as.factor(age),
         group = as.factor(group), ID = as.factor(ID))

#create default boxplot function
default.single.boxplot <- list(geom_boxplot(aes(fill = group)),
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
                               scale_fill_brewer(palette='Purples'))


ggplot(dfw, aes(x = age, y = weight, alpha = group)) +
 default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Body weight (g)') 

ggsave("trimmed-weights.png", width = 15, height = 15, units = "cm")


ggplot(dfw, aes(x = age, y = perc_gain, alpha = group)) +
  default.single.boxplot


a <- aov(weight ~ age + group + Error(ID), data = dfw)
summary(a)

aperc <- aov(perc_gain ~ age + group, data = dfw)
summary(aperc)


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
                               scale_fill_brewer(palette='Dark2'))


brain<-read_csv("trimmed-brain-weights1.csv") #load csv

brain <- brain %>%
  mutate(weight = as.numeric(weight), age = as.factor(age),
         grouo = as.factor(grouo))

ggplot(brain, aes(x = age, y = weight, alpha = grouo)) +
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Brain weight (mg)') 

ggsave("trimmed-brain-weights.png", width = 15, height = 15, units = "cm")


a <- aov(weight ~ age + grouo, data = brain)
summary(a)

