.libPaths("O:/paths") #set library path
library('tidyverse') #load tidyverse
library(gridExtra) #load gridExtra
library(ggpubr)
library(ggsignif)
library(reshape2)

## spontaneous frequencies

#import csv file of results
df1 <- read_csv("Seedpixel_0-1hz_frequency-animal-ave.csv")


df1 <- df1 %>%
  mutate(age = factor(age)) 

df1 <- df1 %>%
  na.omit(age)

ggplot() + 
  geom_point(data = df1, aes(R_L_corr, whole, colour = age))

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

#summaries data
df1_mean <- df1 %>%
  group_by (age) %>%
  summarise_all (funs(mean))
  
  #summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

df1_mean <- df1_mean %>%
  mutate (age = as.numeric(as.character(age)))

df1_melt = melt(df1_mean, id=c('age'))

df1_melt = slice(df1_melt, 36:n())

#summary plots

ggplot(df1_melt, aes(x = age, y = value, colour = variable)) +
  geom_line(size = 1) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        legend.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=18, face='bold'),
        axis.text = element_text(size = 16, face = 'bold', colour = 'black')) +  
  scale_y_continuous(limits = c(0, 25)) +  
  scale_x_discrete(limits = c(1, 3, 5, 7, 9)) +
  labs(x = 'Postnatal age', y = 'Frequency (events/min)') 




#get legend
ggplot(df1, aes(age, whole, fill = age)) +
  geom_boxplot () +
  scale_fill_brewer(palette='Set1')


#whole brain frequency
ggplot(df1, aes(age,whole)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 25)

ggsave("whole-brain_freq.png", width = 15, height = 15, units = "cm")

#global stats
shapiro.test(df1$whole)
hist(df1$whole)

a <- aov(whole ~ age, data = df1)
summary(a)
TukeyHSD(a)

#Hem correlations
ggplot(df1, aes(age,R_L_corr)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'Postnatal day', y = 'Hemispheric correlation') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 1)

ggsave("hemisphere-correlation.png", width = 15, height = 15, units = "cm")

shapiro.test(df1$R_L_corr)

a <- aov(R_L_corr ~ age, data = df1)
summary(a)
TukeyHSD(a)

#Hem macthed events
ggplot(df1, aes(age,R_L_matched)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 100)) + 
  labs(x = 'Postnatal day', y = 'Coordinated activity (%)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 100)

ggsave("hemisphere-matched.png", width = 15, height = 15, units = "cm")


shapiro.test(df1$R_L_matched)

kruskal.test(R_L_matched ~ age, data = df1)


#freq different for subsets of sessions?

df1 <- read_csv("Seedpixel_0-1hz_frequency.csv")


df1 <- df1 %>%
  mutate(age = factor(age)) 

a <- aov(whole ~ session, data = df1, subset = (age == 7))
summary(a)
TukeyHSD(a)



# subset activity by session

df1 <- df1 %>%
  mutate(session = as.numeric(session)) %>%
  mutate(wh_sub = whole) %>%
  mutate(wh_sub = replace(wh_sub, which(session > 3), NA)) %>%
  mutate(session = as.factor(session))#make age. ID and session into factors


ggplot(df1, aes(session,wh_sub)) + 
  default.single.boxplot +
  scale_x_discrete(limits = c('1', '2', '3')) + 
  labs(x = 'session', y = 'Frequency (events/min)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '3')), face = 'bold') +
  facet_grid(~age) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
                         strip.background = element_rect(fill="white"), 
        axis.title.y = element_text(size=18, face='bold', color = 'black'))


#session sig diff for each age?
a <- aov(wh_sub ~ session, data = df1,subset = (age == 3))
summary(a)
TukeyHSD(a)
a <- aov(wh_sub ~ session + age, data = df1)

p.adjust(0.0299, method = 'bonferroni', n = 5)

#

#hemispheric freq

dfhem <- read_csv("hemisphere-freq.csv")


dfhem <- dfhem %>%
  mutate(age = factor(age),Side = factor(Side))

ggplot(dfhem, aes(age,freq, alpha = Side)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') +
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold', label.y = 25)

ggsave("hemisphere_freq.png", width = 15, height = 15, units = "cm")


dfhem <- read_csv("hemisphere-freq-animal-ave.csv")


dfhem <- dfhem %>%
  mutate(age = factor(age),Side = factor(Side))

#global stats
shapiro.test(dfhem$freq)
hist(dfhem$freq)

a <- aov(freq ~ age + Side, data = dfhem)
summary(a)
TukeyHSD(a)


#regional stats
shapiro.test(df1$LB)
hist(df1$RB)

a <- aov(RA ~ age, data = df1)
summary(a)
TukeyHSD(a)





# regional frequency                  
#boxplot for right barrel peak frequency
bc.f <- ggplot(df1, aes(age,RB)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'BC') 

#boxplot for right forelimb peak frequency
fp.f <- ggplot(df1, aes(age,LFL)) + 
 default.boxplot +
scale_y_continuous(limits = c(0, 25)) + 
 labs(x = 'FL')

#boxplot for right hindlimb peak frequency
hp.f <- ggplot(df1, aes(age,RHL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'HL')

#boxplot for right M1 peak frequency
m1.f <- ggplot(df1, aes(age,RM1)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'M1')

#boxplot for right M2 peak frequency
m2.f <- ggplot(df1, aes(age,RM2)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'M2')

#boxplot for right V1 peak frequency
v.f <- ggplot(df1, aes(age,RV)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'V1')

#boxplot for right RS peak frequency
rs.f <- ggplot(df1, aes(age,RRS)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'RS')

#boxplot for right PPC peak frequency
ppc.f <- ggplot(df1, aes(age,RPPC)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'PPC')

#boxplot for right hindlimb peak frequency
a.f <- ggplot(df1, aes(age,RA)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'A1')

#graph with no y axis (do manually)
grid.arrange(bc.f, fp.f, hp.f, m1.f, m2.f, v.f, rs.f, ppc.f, a.f, nrow = 1)
    
            
## normalised frequencies
#import csv file of results
df2 <- read_csv("Seedpixel_0-1hz_frequency_normalised.csv")

#make age. ID and session into factors
df2 <- df2 %>%
  mutate(age = factor(age), ID = factor(ID)) 

#summaries data
df2_mean <- df2 %>%
  group_by (age) %>%
  summarise_all (funs(mean), id = 'age')

#summarise_all (funs(mean,sd,se=sd(.)/sqrt(n())))

df2_mean <- df2_mean %>%
  mutate (age = as.numeric(as.character(age)))


library(reshape2)
df2_melt = melt(df2_mean, id=c('age'))

df2_melt = slice(df2_melt, 13:n())

df2_melt <- df2_melt %>%
  na.omit(age)

#summary plots

ggplot(df2_melt, aes(x = age, y = value, colour = variable)) +
  geom_line(size = 1) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        legend.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=18, face='bold'),
        axis.text = element_text(size = 16, face = 'bold', colour = 'black')) +  
  scale_y_continuous(limits = c(0, 1)) +  
  scale_x_discrete(limits = c(1, 3, 5, 7, 9)) +
  labs(x = 'Postnatal age', y = 'Normalised frequency') 






#boxplot for right barrel peak frequency
bc.f2 <- ggplot(df2, aes(age,RB)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'BC')

#boxplot for right forelimb peak frequency
fp.f2 <- ggplot(df2, aes(age,RFL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'FL')

#boxplot for right hindlimb peak frequency
hp.f2 <- ggplot(df2, aes(age,RHL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'HL')

#boxplot for right M1 peak frequency
m1.f2 <- ggplot(df2, aes(age,RM1)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'M1')

#boxplot for right M2 peak frequency
m2.f2 <- ggplot(df2, aes(age,RM2)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'M2')

#boxplot for right V1 peak frequency
v.f2 <- ggplot(df2, aes(age,RV)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'V1')

#boxplot for right RS peak frequency
rs.f2 <- ggplot(df2, aes(age,RRS)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'RS')

#boxplot for right PPC peak frequency
ppc.f2 <- ggplot(df2, aes(age,RPPC)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'PPC')

#boxplot for right hindlimb peak frequency
a.f2 <- ggplot(df2, aes(age,RA)) + 
  default.boxplot +
  labs(x = 'A1') 


#graph with no y axis (do manually)
grid.arrange(bc.f2, fp.f2, hp.f2, m1.f2, m2.f2, v.f2, rs.f2, ppc.f2, a.f2, nrow = 1)

a <- aov(LM2 ~ age, data = df2)
summary(a)

## max_dvdt
#import csv file of results
df3 <- read_csv("Seedpixel_0-1hz_max-dvdt.csv")


#make age. ID and session into factors
df3 <- df3 %>%
  mutate(age = factor(age), ID = factor(ID)) 

#stats
a <- aov(RA ~ age, data = df3)
summary(a)
TukeyHSD(a)

#boxplot for right barrel peak frequency
bc.f3 <- ggplot(df3, aes(age,RB)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'BC')

#boxplot for right forelimb peak frequency
fp.f3 <- ggplot(df3, aes(age,RFL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'FL')

#boxplot for right hindlimb peak frequency
hp.f3 <- ggplot(df3, aes(age,RHL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'HL')

#boxplot for right M1 peak frequency
m1.f3 <- ggplot(df3, aes(age,RM1)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'M1')

#boxplot for right M2 peak frequency
m2.f3 <- ggplot(df3, aes(age,RM2)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'M2')

#boxplot for right V1 peak frequency
v.f3 <- ggplot(df3, aes(age,RV)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'V1')

#boxplot for right RS peak frequency
rs.f3 <- ggplot(df3, aes(age,RRS)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'RS')

#boxplot for right PPC peak frequency
ppc.f3 <- ggplot(df3, aes(age,RPPC)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'PPC')

#boxplot for right hindlimb peak frequency
a.f3 <- ggplot(df3, aes(age,RA)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'A1') 


#graph with no y axis (do manually)
grid.arrange(bc.f3, fp.f3, hp.f3, m1.f3, m2.f3, v.f3, rs.f3, ppc.f3, a.f3, nrow = 1)


## min_dvdt
#import csv file of results
df4 <- read_csv("Seedpixel_0-1hz_min-dvdt.csv")


#make age. ID and session into factors
df4 <- df4 %>%
  mutate(age = factor(age), ID = factor(ID)) 


#boxplot for right barrel peak frequency
bc.f4 <- ggplot(df4, aes(age,RB)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) + 
  labs(x = 'BC')

#boxplot for right forelimb peak frequency
fp.f4 <- ggplot(df4, aes(age,RFL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) +  
  labs(x = 'FL')

#boxplot for right hindlimb peak frequency
hp.f4 <- ggplot(df4, aes(age,RHL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) +  
  labs(x = 'HL')

#boxplot for right M1 peak frequency
m1.f4 <- ggplot(df4, aes(age,RM1)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) + 
  labs(x = 'M1')

#boxplot for right M2 peak frequency
m2.f4 <- ggplot(df4, aes(age,RM2)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) + 
  labs(x = 'M2')

#boxplot for right V1 peak frequency
v.f4 <- ggplot(df4, aes(age,RV)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) + 
  labs(x = 'V1')

#boxplot for right RS peak frequency
rs.f4 <- ggplot(df4, aes(age,RRS)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) + 
  labs(x = 'RS')

#boxplot for right PPC peak frequency
ppc.f4 <- ggplot(df4, aes(age,RPPC)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) + 
  labs(x = 'PPC')

#boxplot for right hindlimb peak frequency
a.f4 <- ggplot(df4, aes(age,RA)) + 
  default.boxplot +
  scale_y_continuous(limits = c(-2, 0)) +  
  labs(x = 'A1') 


#graph with no y axis (do manually)
grid.arrange(bc.f4, fp.f4, hp.f4, m1.f4, m2.f4, v.f4, rs.f4, ppc.f4, a.f4, nrow = 1)

## min_dvdt
#import csv file of results
df5 <- read_csv("Seedpixel_0-1hz_amp.csv")


#make age. ID and session into factors
df5 <- df5 %>%
  mutate(age = factor(age), ID = factor(ID)) 

a <- aov(RRS ~ age, data = df5)
summary(a)
TukeyHSD(a)

#boxplot for right barrel peak frequency
bc.f5 <- ggplot(df5, aes(age,RB)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'BC')

#boxplot for right forelimb peak frequency
fp.f5 <- ggplot(df5, aes(age,RFL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'FL')

#boxplot for right hindlimb peak frequency
hp.f5 <- ggplot(df5, aes(age,RHL)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) +  
  labs(x = 'HL')

#boxplot for right M1 peak frequency
m1.f5 <- ggplot(df5, aes(age,RM1)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'M1')

#boxplot for right M2 peak frequency
m2.f5 <- ggplot(df5, aes(age,RM2)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) +  
  labs(x = 'M2')

#boxplot for right V1 peak frequency
v.f5 <- ggplot(df5, aes(age,RV)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'V1')

#boxplot for right RS peak frequency
rs.f5 <- ggplot(df5, aes(age,RRS)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'RS')

#boxplot for right PPC peak frequency
ppc.f5 <- ggplot(df5, aes(age,RPPC)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'PPC')

#boxplot for right hindlimb peak frequency
a.f5 <- ggplot(df5, aes(age,RA)) + 
  default.boxplot +
  scale_y_continuous(limits = c(0, 12)) + 
  labs(x = 'A1') 


#graph with no y axis (do manually)
grid.arrange(bc.f5, fp.f5, hp.f5, m1.f5, m2.f5, v.f5, rs.f5, ppc.f5, a.f5, nrow = 1)

#change data table from wide to long format

df2_long <- gather(df2, region, activity, LB:RA, factor_key=TRUE)

a <- aov (activity ~ age + region, data = df2_long)

a <- aov (RV ~ age, data = df2)

summary(a)

TukeyHSD(a)

# 1. Homogeneity of variances
plot(a, 1)
# 2. Normality
plot(a, 2)

a_residuals <- residuals(object = a )
# Run Shapiro-Wilk test
shapiro.test(x = a_residuals )
shapiro.test(df2$LB)
hist(df2$LB)

#trimmed frequencies

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


### ROI freq
#import csv file of results
df1t <- read_csv("trimmed-ROI_frequency.csv")


#make age and group into factors
df1t <- df1t %>%
  mutate(age = factor(age), group = factor(group), session = as.factor(session)) 


#whole brain frequency
ggplot(df1t, aes(age,whole, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-whole-brain_freq.png", width = 15, height = 15, units = "cm")



#session frequency
ggplot(df1t, aes(session,whole, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') +
facet_grid(~age) +
  scale_x_discrete(limits = c('1', '2')) +
stat_compare_means(label = 'p.signif', comparisons = list(c('1', '2')), face = 'bold') +
theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white"), 
        axis.title.y = element_text(size=18, face='bold', color = 'black'))

ggsave("trimmed-freq_over-session.png", width = 15, height = 15, units = "cm")



#LB frequency
ggplot(df1t, aes(age,LB, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-LB-brain_freq.png", width = 15, height = 15, units = "cm")

#RB frequency
ggplot(df1t, aes(age,RB, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-RB-brain_freq.png", width = 15, height = 15, units = "cm")

#RV frequency
ggplot(df1t, aes(age,RV, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-RV-brain_freq.png", width = 15, height = 15, units = "cm")

#LV frequency
ggplot(df1t, aes(age,LV, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-LV-brain_freq.png", width = 15, height = 15, units = "cm")

#LRS frequency
ggplot(df1t, aes(age,LRS, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-LRS-brain_freq.png", width = 15, height = 15, units = "cm")

#RRS frequency
ggplot(df1t, aes(age,RRS, alpha = group)) + 
  default.trimmed.single.boxplot +
  scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)') 

ggsave("trimmed-RRS-brain_freq.png", width = 15, height = 15, units = "cm")
