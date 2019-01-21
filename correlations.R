.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)


dfcorr <-  read_csv("correlations_seedpixel-animal-ave.csv")


dfcorr <- dfcorr %>%
  mutate(age = factor(age))


#summaries data
dfcorr_mean <- dfcorr %>%
  group_by (age) %>%
  summarise_all (funs(mean),na.rm = TRUE)

#summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

dfcorr_mean <- dfcorr_mean %>%
  mutate (age = as.numeric(as.character(age)))

dfcorr_melt = melt(dfcorr_mean, id=c('age'))

#summary plots

ggplot(dfcorr_melt, aes(x = age, y = value, colour = variable)) +
  geom_line(size = 1) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        legend.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=24, face='bold'),
        axis.text = element_text(size = 20, face = 'bold', colour = 'black'), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        plot.margin = unit(c(1,0,1,0), "cm")) +  
  scale_y_continuous(limits = c(0, 1)) +  
  scale_x_discrete(limits = c(1, 3, 5, 7, 9)) +
  labs(x = 'Postnatal age', y = 'Seedpixel correlation') 


ggsave("seedpixel_correlation.png", width = 15, height = 15, units = "cm")

shapiro.test(dfcorr$A)
a <- aov(A ~ age, data = dfcorr)
summary(a)

#Matched peaks
dfcorr <-  read_csv("matched-events_seedpixel-animal-ave.csv")


dfcorr <- dfcorr %>%
  mutate(age = factor(age))


#summaries data
dfcorr_mean <- dfcorr %>%
  group_by (age) %>%
  summarise_all (funs(mean),na.rm = TRUE)

#summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

dfcorr_mean <- dfcorr_mean %>%
  mutate (age = as.numeric(as.character(age)))

dfcorr_melt = melt(dfcorr_mean, id=c('age'))

#summary plots

ggplot(dfcorr_melt, aes(x = age, y = value, colour = variable)) +
  geom_line(size = 1) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        legend.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=24, face='bold'),
        axis.text = element_text(size = 20, face = 'bold', colour = 'black'), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        plot.margin = unit(c(1,0,1,0), "cm")) +  
  scale_y_continuous(limits = c(0, NA)) +  
  scale_x_discrete(limits = c(1, 3, 5, 7, 9)) +
  labs(x = 'Postnatal age', y = 'Coordinated activity (%)') 


ggsave("seedpixel-matched-events.png", width = 15, height = 15, units = "cm")

shapiro.test(dfcorr$B)
a <- aov(A ~ age, data = dfcorr)
summary(a)


#ROI correlations
dfcorr <-  read_csv("correlations_roi.csv")

dfcorr <- dfcorr %>%
mutate(age = factor(age), ID = as.factor(ID), session = as.factor(session))

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

#R:L hemisphere correlation
ggplot(dfcorr, aes(age,R_L_corr)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 1.1)) + 
  labs(x = 'Postnatal day', y = 'Hemisphere correlation') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 1.1)


ggsave("R-L_hem_correlation.png", width = 15, height = 15, units = "cm")



#R:L barrel correlation
ggplot(dfcorr, aes(age,barrel)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 1.1)) + 
  labs(x = 'Postnatal day', y = 'Barrel cortex correlation') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 1.1)


ggsave("barrel-roi_correlation.png", width = 15, height = 15, units = "cm")


shapiro.test(dfcorr$barrel)

kruskal.test(barrel~ age, data = dfcorr)
posthoc.kruskal.nemenyi.test(barrel~ age, data = dfcorr, dist="Tukey")


#R:L visual correlation
ggplot(dfcorr, aes(age,visual)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 1.1)) + 
  labs(x = 'Postnatal day', y = 'Visual cortex correlation') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 1.1)


ggsave("visual-roi_correlation.png", width = 15, height = 15, units = "cm")


shapiro.test(dfcorr$visual)

kruskal.test(visual~ age, data = dfcorr)
posthoc.kruskal.nemenyi.test(visual~ age, data = dfcorr, dist="Tukey")


#R:L RS correlation
ggplot(dfcorr, aes(age,rs)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 1.1)) + 
  labs(x = 'Postnatal day', y = 'Retrosplenial cortex correlation') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 1.1)


ggsave("RS-roi_correlation.png", width = 15, height = 15, units = "cm")


shapiro.test(dfcorr$rs)

kruskal.test(rs ~ age, data = dfcorr)
posthoc.kruskal.nemenyi.test(rs~ age, data = dfcorr, dist="Tukey")


#seedpixel correlations

#create default grid boxplot function
default.boxplot <- list(geom_boxplot(aes(fill = age)),
                        # geom_point(aes(colour = age)),
                        theme(plot.title = element_text(size=11, face='bold',hjust=0.5),
                              #axis.title.x = element_blank(),
                              axis.title.x = element_text(size=16, face='bold'),
                              axis.title.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = unit(c(1,0,1,0), "cm"),
                              panel.spacing = unit(0, "lines"),
                              panel.border = element_blank(),
                              panel.background = element_rect(fill = "white"),
                              panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
                              legend.position = 'none'),
                        scale_fill_brewer(palette='Set1'))



#ROI correlations
dfcorrs <-  read_csv("correlations_seedpixel.csv")

dfcorrs <- dfcorrs %>%
  mutate(age = factor(age), ID = as.factor(ID), session = as.factor(session))

# regional correlations                  
#boxplot for barrel correaltion
BC <- ggplot(dfcorrs, aes(age,B)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'BC') 

#boxplot for FL correaltion
FL <- ggplot(dfcorrs, aes(age,FL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'FL') 

#boxplot for HL correaltion
HL <- ggplot(dfcorrs, aes(age,HL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'HL') 

#boxplot for M1 correaltion
M1 <- ggplot(dfcorrs, aes(age,M1)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'M1') 

#boxplot for M2 correaltion
M2 <- ggplot(dfcorrs, aes(age,M2)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'M2') 

#boxplot for V correaltion
V <- ggplot(dfcorrs, aes(age,V)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'V1') 

#boxplot for RS correaltion
RS <- ggplot(dfcorrs, aes(age,RS)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'RS') 

#boxplot for PPC correaltion
PPC <- ggplot(dfcorrs, aes(age,PPC)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'PPC') 

#boxplot for A1 correaltion
A1 <- ggplot(dfcorrs, aes(age,A)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'A1') 

grid.arrange(BC, FL, HL, M1, M2, V, RS, PPC, A1, nrow = 1)


#trimmed correlations

default.trimmed.single.boxplot <- list(geom_boxplot(aes(fill = age)),
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

#import csv file of results
dfcorrt <- read_csv("trimmed-correlations_roi.csv")


#make age and group into factors
dfcorrt <- dfcorrt %>%
  mutate(age = factor(age), group = factor(group)) 

#R:L barrel correlation
ggplot(dfcorrt, aes(age,barrel, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'Postnatal day', y = 'Barrel cortex correlation') 

ggsave("trimmed-barrel-roi_correlation.png", width = 15, height = 15, units = "cm")

#R:L visualcorrelation
ggplot(dfcorrt, aes(age, visual, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'Postnatal day', y = 'Visual cortex correlation') 

ggsave("trimmed-visual-roi_correlation.png", width = 15, height = 15, units = "cm")


#R:L RS correlation
ggplot(dfcorrt, aes(age, rs, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'Postnatal day', y = 'Retrosplenial cortex correlation') 

ggsave("trimmed-RS-roi_correlation.png", width = 15, height = 15, units = "cm")
