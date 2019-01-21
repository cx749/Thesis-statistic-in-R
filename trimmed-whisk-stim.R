.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)
library(PMCMR)


#whisker array stimulation
array <- read_csv("trimmed-whisker-stimulation-animal-ave.csv")

colnames(array) <- make.names(colnames(array)) #change - to . in names

array <- array %>%
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



#peaks
ggplot(array, aes(age,still.peak, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Amplitude (df/f %)') 


ggsave("trimmed-whisk-sing_amp.png", width = 15, height = 15, units = "cm")


shapiro.test(array$still.peak)
hist(array$still.peak)

a <- aov(still.peak ~ age + group, data = array)
summary(a)
TukeyHSD(a)

#area
ggplot(array, aes(age,area, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Area of activation (% of cortex)') 


ggsave("trimmed-whisk-sing_area.png", width = 15, height = 15, units = "cm")


shapiro.test(array$area)
a <- aov(area ~ age + group, data = array)
summary(a)
TukeyHSD(a)


t.test(area ~ group, data = array, subset = (age == 7))
p.adjust(0.01838, method = 'bonferroni', n = 2)

t.test(area ~ age, data = array, subset = (group == 'not'))
p.adjust(0.007564, method = 'bonferroni', n = 2)


#rise
ggplot(array, aes(age,still.max_dvdt, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Rise-time (df/f%/s)')


ggsave("trimmed-whisk-sing_rise-time.png", width = 15, height = 15, units = "cm")



#fall
ggplot(array, aes(age,still.min_dvdt, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(NA, 0)) + 
  labs(x = 'Postnatal day', y = 'Fall-time (df/f%/s)')


ggsave("trimmed-whisk-sing_fall-time.png", width = 15, height = 15, units = "cm")



#single whisker stimulation
sing <- read_csv("trimmed-single_whisker-stimulation-animal-ave-pooled.csv")

colnames(sing) <- make.names(colnames(sing)) #change - to . in names

sing <- sing %>%
  mutate(age = factor(age), group = as.factor(group), subject = as.factor(subject))

#peaks
ggplot(sing, aes(age,still.peak, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Amplitude (df/f %)') 

  facet_grid(~whisker) +
  theme(strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
                            strip.background = element_rect(fill="white"))


ggsave("trimmed-single-whiskers_amp-animal-ave.png", width = 30, height = 15, units = "cm")




#max peaks
ggplot(sing, aes(age,still.max, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Max amplitude (df/f %)') +
  theme(strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white"))


ggsave("trimmed-single-whiskers_max-amp-animal-ave.png", width = 15, height = 15, units = "cm")


shapiro.test(sing$still.max)
a <- aov(still.max ~ age + group + Error(subject), data = sing)
summary(a)
TukeyHSD(a)

a <- aov (still.max ~ group + age, data = sing, subset = (whisker == 'delta'))
summary(a)
TukeyHSD(a)

#area
ggplot(sing, aes(age,area, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Area of activation (% of cortex)') +
  theme(strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white"))

ggsave("trimmed-single-whiskers_area-animal-ave.png", width = 15, height = 15, units = "cm")



sing <- read_csv("trimmed-single_whisker-stimulation-animal-ave.csv")

colnames(sing) <- make.names(colnames(sing)) #change - to . in names

sing <- sing %>%
  mutate(age = factor(age), group = as.factor(group), subject = as.factor(subject))

shapiro.test(sing$area)

a <- aov(still.peak ~ age + group + Error(subject), data = sing)
summary(a)


TukeyHSD(a)

t.test(area ~ group, data = sing, subset = (age == '7'))
p.adjust(0.004675, method = 'bonferroni', n = 6)

