.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)


dfcorr <-  read_csv("trimmed-correlations_seedpixel-animal-ave.csv")


dfcorr <- dfcorr %>%
  mutate(age = factor(age), group = as.factor(group))


#summaries data
dfcorr_mean <- dfcorr %>%
  group_by (age, group) %>%
  summarise_all (funs(mean),na.rm = TRUE)

#summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

dfcorr_mean <- dfcorr_mean %>%
  mutate (age = as.numeric(as.character(age)))

dfcorr_melt = melt(dfcorr_mean, id=c('age', 'group'))

dfcorr_melt = slice(dfcorr_melt, 5:n())

#summary plots

ggplot(dfcorr_melt, aes(x = age, y = value, colour = variable)) +
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
  labs(x = 'Postnatal age', y = 'Seedpixel correlation') +
  facet_grid(~group)



shapiro.test(dfcorr$V)
a <- aov(V ~ age + group, data = dfcorr)
summary(a)

#Matched peaks

dfcorr <-  read_csv("trimmed-matched-events_seedpixel-animal-ave.csv")


dfcorr <- dfcorr %>%
  mutate(age = factor(age), group = as.factor(group))


#summaries data
dfcorr_mean <- dfcorr %>%
  group_by (age, group) %>%
  summarise_all (funs(mean),na.rm = TRUE)

#summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

dfcorr_mean <- dfcorr_mean %>%
  mutate (age = as.numeric(as.character(age)))

dfcorr_melt = melt(dfcorr_mean, id=c('age', 'group'))

dfcorr_melt = slice(dfcorr_melt, 5:n())

#summary plots

ggplot(dfcorr_melt, aes(x = age, y = value, colour = variable)) +
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
  labs(x = 'Postnatal age', y = 'Coordinated activity (%)') +
  facet_grid(~group)



shapiro.test(dfcorr$RS)
a <- aov(RS ~ age + group, data = dfcorr)
summary(a)



#ROI correlations
dfcorr <-  read_csv("trimmed-correlations_roi-animal-ave.csv")

dfcorr <- dfcorr %>%
  mutate(age = factor(age), group = as.factor(group))


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



#peak match
ggplot(dfcorr, aes(age,barrel_matched, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Coordinated activity (%)') 


ggsave("trimmed-barrel-roi_macthed.png", width = 15, height = 15, units = "cm")


shapiro.test(dfcorr$barrel_matched)
hist(dfcorr$barrel_matched)

a <- aov(barrel_matched ~ age + group, data = dfcorr)
summary(a)

#correlation
ggplot(dfcorr, aes(age,barrel, alpha = group)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Correlation coefficient') 


ggsave("trimmed-barrel-roi_macthed.png", width = 15, height = 15, units = "cm")

shapiro.test(dfcorr$barrel)
hist(dfcorr$barrel)

a <- aov(barrel ~ age + group, data = dfcorr)
summary(a)
