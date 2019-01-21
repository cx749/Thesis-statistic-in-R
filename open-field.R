.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)


##load data
dfop<-read_csv("open-field.csv") #load csv

dfop <- dfop %>%
  mutate(litter_size = as.numeric(litter_size), group = as.factor(group), 
         sex = as.factor(sex)) #convert variables to numeric


#distance
ggplot(dfop, aes(x=group, y=distance, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Distance travelled (cm)') +
  stat_compare_means(label = 'p.signif', label.y = 2300) 


ggsave("OP-distance.png", width = 15, height = 15, units = "cm")

hist(dfop$distance[dfop$group == 'preterm'])
hist(dfop$distance[dfop$group == 'vehicle'])

shapiro.test(dfop$distance[dfop$group == 'preterm'])
shapiro.test(dfop$distance[dfop$group == 'vehicle'])

t.test (distance ~ group, data = dfop) 


#speed
ggplot(dfop, aes(x=group, y=speed, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Average speed (cm/sec)') +
  stat_compare_means(label = 'p.signif', label.y = 8) 


ggsave("OP-speed.png", width = 15, height = 15, units = "cm")

hist(dfop$speed[dfop$group == 'preterm'])
hist(dfop$speed[dfop$group == 'vehicle'])

shapiro.test(dfop$speed[dfop$group == 'preterm'])
shapiro.test(dfop$speed[dfop$group == 'vehicle']) 

t.test (speed ~ group, data = dfop) 

#thigmotaxis
ggplot(dfop, aes(x=group, y=thigmo, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Thigmotaxic ratio') +
  stat_compare_means(label = 'p.signif', label.y = 1) 


ggsave("OP-thigmotaxis.png", width = 15, height = 15, units = "cm")

hist(dfop$thigmo[dfop$group == 'preterm'])
hist(dfop$thigmo[dfop$group == 'vehicle'])

shapiro.test(dfop$thigmo[dfop$group == 'preterm'])
shapiro.test(dfop$thigmo[dfop$group == 'vehicle'])

qqnorm(dfop$thigmo[dfop$group == 'vehicle'])

wilcox.test (thigmo ~ group, data = dfop) 

t.test (thigmo ~ group, data = dfop) 

a <- aov(thigmo ~ group + sex, data = dfop)
summary(a)
