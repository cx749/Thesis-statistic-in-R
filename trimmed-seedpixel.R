.libPaths("O:/paths") #set library path
library('tidyverse') #load tidyverse
library(gridExtra) #load gridExtra
library(ggpubr)
library(ggsignif)
library(reshape2)

## spontaneous frequencies

#import csv file of results
df1 <- read_csv("trimmed-Seedpixel_0-1hz_frequency-animal-ave.csv")


df1 <- df1 %>%
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
                               scale_fill_brewer(palette='Dark2'))


#whole brain frequency
ggplot(df1, aes(age,whole, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 
  

stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 25)

ggsave("whole-brain_freq.png", width = 15, height = 15, units = "cm")

#global stats
shapiro.test(df1$whole)
hist(df1$whole)

a <- aov(whole ~ age + group, data = df1)
summary(a)
TukeyHSD(a)


#seedpixel freq
#summaries data
df1_mean <- df1 %>%
  group_by (age, group) %>%
  summarise_all (funs(mean))

#summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

df1_mean <- df1_mean %>%
  mutate (age = as.numeric(as.character(age)))

df1_melt = melt(df1_mean, id=c('age', 'group'))

df1_melt = slice(df1_melt, 13:n())

#summary plots

ggplot(df1_melt, aes(x = age, y = value, colour = variable)) +
  geom_line(size = 1) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        legend.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=18, face='bold'),
        axis.text = element_text(size = 16, face = 'bold', colour = 'black'),
        strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +  
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal age', y = 'Frequency (events/min)') +
  facet_grid(~group)

shapiro.test(df1$LB)
hist(df1$RB)

a <- aov(RB ~ age + group, data = df1)
summary(a)
TukeyHSD(a)

df1roi <- read_csv("trimmed-ROI_frequency-animal-ave.csv")

df1roi <- df1roi %>%
  mutate(age = factor(age), group = as.factor(group)) 


#contralateral to trimmed (R) barrel frequency
ggplot(df1roi, aes(age,RB_roi, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 



shapiro.test(df1roi$RB_roi)
hist(df1roi$RB_roi)

a <- aov(RB_roi ~ age + group, data = df1roi)
summary(a)
TukeyHSD(a)

