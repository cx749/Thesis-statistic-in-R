.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)
library(PMCMR)


#import csv file of results
whisk_stim <- read_csv("whisker-stimulation-animal-ave.csv")

colnames(whisk_stim) <- make.names(colnames(whisk_stim))

#make age factor (for color in plot)?
whisk_stim <- whisk_stim %>%
  mutate(age = factor(age))


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


#peaks
ggplot(whisk_stim, aes(age,still.peak)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Amplitude (df/f %)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', position = 70)


ggsave("whisk-sing_amp.png", width = 15, height = 15, units = "cm")


shapiro.test(whisk_stim$still.peak)
hist(whisk_stim$still.peak)

kruskal.test(still.peak ~ age, data = whisk_stim)
posthoc.kruskal.nemenyi.test(still.peak ~ age, data = whisk_stim, dist="Tukey")


#rise time
ggplot(whisk_stim, aes(age,still.max_dvdt)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Rise-time (df/d%/s)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold')

ggsave("whisk-sing_max-dvdt.png", width = 15, height = 15, units = "cm")

shapiro.test(whisk_stim$still.max_dvdt)
hist(whisk_stim$still.max_dvdt)

kruskal.test(still.max_dvdt ~ age, data = whisk_stim)
posthoc.kruskal.nemenyi.test(still.max_dvdt ~ age, data = whisk_stim, dist="Tukey")


#fall time
ggplot(whisk_stim, aes(age,still.min_dvdt)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(NA, 0)) + 
  labs(x = 'Postnatal day', y = 'Fall-time (df/d%/s)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 0)

ggsave("whisk-sing_min-dvdt.png", width = 15, height = 15, units = "cm")

shapiro.test(whisk_stim$still.min_dvdt)
hist(whisk_stim$still.min_dvdt)

kruskal.test(still.min_dvdt ~ age, data = whisk_stim)
posthoc.kruskal.nemenyi.test(still.min_dvdt ~ age, data = whisk_stim, dist="Tukey")

#area
ggplot(whisk_stim, aes(age,area)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Area of activation (% of cortex)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold')

ggsave("whisk-sing_area.png", width = 15, height = 15, units = "cm")


shapiro.test(whisk_stim$area)
hist(whisk_stim$area)

a<-aov(area ~ age, data = whisk_stim)
summary(a)


kruskal.test(area ~ age, data = whisk_stim)
posthoc.kruskal.nemenyi.test(area ~ age, data = whisk_stim, dist="Tukey")


#still vs moving peaks

sVm <- read_csv("whisk_still-moving-animal-ave.csv")

#make age and state factor (for color in plot)?
sVm <- sVm %>%
  mutate(age = factor(age), state = as.factor(state))


ggplot(sVm, aes(age,peak, alpha = 'state')) + 
default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Amplitude (df/f %)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', position = 70)

ggplot(sVm, aes(age,peak, fill = age, alpha = state)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Amplitude (df/f %)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', position = 70)

ggsave("whisk-still-moving+peaks.png", width = 15, height = 15, units = "cm")



a <- aov(peak ~ state + age, data = sVm)
summary(a)
TukeyHSD(a)

#adaption to stimulation 

ada <- read_csv("whisk-stim-adaption-animal-ave.csv")

ada <- ada %>%
  mutate(age = factor(age))

library(reshape2)

#summaries data
ada_mean <-  ada %>%
  group_by (age) %>%
  summarise_all (funs(mean))

ada_sd <-  ada %>%
  group_by (age) %>%
  summarise_all (sd)



ada_melt = melt(ada_mean, id=c('age'))

ada_melt_sd = melt(ada_sd, id=c('age'))

ada_melt = slice(ada_melt, 6:n())

ada_melt_sd = slice(ada_melt_sd, 6:n())

ada_melt <- ada_melt %>%
  mutate (variable = as.numeric(variable), age = as.factor(age))


ada_melt_sd <- ada_melt_sd %>%
  mutate (variable = as.numeric(variable), age = as.factor(age))

ada_melt_sd <- ada_melt_sd %>%
select(std = value)


df <- cbind(ada_melt, ada_melt_sd)

#plot
ggplot(df, aes(x = variable, y = value, colour = age)) +
  geom_line(size = 1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        legend.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=28, face='bold'),
        axis.text = element_text(size = 24, face = 'bold', colour = 'black')) + 
  labs(x = 'Stimulation repeats', y = 'Percentage of response') +
  scale_color_brewer(palette = 'Set1') + 
  geom_ribbon(aes(ymin = value + std, ymax = value - std, fill = age), alpha = 0.2, colour = NA)

ggsave("whisk-stim-adaption.png", width = 15, height = 15, units = "cm")

#stats
shapiro.test(ada_melt$value[ada_melt$age == 7])
hist(ada_melt$value[ada_melt$age == 1])


a <- aov(value ~ variable, data = ada_melt, subset = (age == 1))
summary(a)
