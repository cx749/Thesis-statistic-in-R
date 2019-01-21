.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
##load data

dfminis<-read_csv("minis.csv") #load csv


dfminis <- dfminis %>%
  mutate(age = as.factor(age), brain_w = as.numeric(brain_w), body_w = as.numeric(body_w), 
         ampa_time = as.numeric(ampa_time), nmda_time = as.numeric(nmda_time), ampa = as.numeric(ampa), 
         nmda = as.numeric(nmda), AN_ratio = as.numeric(AN_ratio), ampa_amp = as.numeric(ampa_amp), 
         nmda_amp = as.numeric(nmda_amp))


mycomparisons <- list(c('22', '28'))
#data visualisation

#ampa frequencies
ggplot(dfminis, aes(x=age, y = ampa, fill = group)) +
  geom_boxplot() + 
  ggtitle('AMPA') +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'),
        plot.title = element_text(hjust = 0.5, size=22, face = 'bold', colour = 'black'), 
        legend.position='none') + 
  labs(x = 'Age (post-conception day)', y = 'Freqency (events/min)') +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 
 



ggsave("ampa-freq.png", width = 15, height = 15, units = "cm")


#nmda frequencies
ggplot(dfminis, aes(x=age, y = nmda, fill = group)) +
  geom_boxplot() + 
  ggtitle('NMDA') +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'),
        plot.title = element_text(hjust = 0.5, size=22, face = 'bold', colour = 'black'),
        legend.position='none') + 
  labs(x = 'Age (post-conception day)', y = 'Freqency (events/min)') +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


ggsave("nmda-freq.png", width = 15, height = 15, units = "cm")



#ampa amplitudes
ggplot(subset(dfminis, age=="24" | age =='28'), aes(x=age, y = ampa_amp, fill = group)) +
  geom_boxplot() + 
  ggtitle('AMPA') +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'),
        plot.title = element_text(hjust = 0.5, size=22, face = 'bold', colour = 'black'),
        legend.position = 'none') + 
        labs(x = 'Age (post-conception day)', y = 'Amplitude (pA)')  + 
  stat_compare_means(label = 'p.signif', comparisons = list(c('24', '28')), face = 'bold') 

ggsave("ampa-amp.png", width = 15, height = 15, units = "cm")

#nmda amplitudes
ggplot(subset(dfminis, age=="24" | age =='28'), aes(x=age, y = nmda_amp, fill = group)) +
  geom_boxplot() + 
  ggtitle('NMDA') +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'),
        plot.title = element_text(hjust = 0.5, size=22, face = 'bold', colour = 'black'),
        legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Amplitude (pA)')  + 
  stat_compare_means(label = 'p.signif', comparisons = list(c('24', '28')), face = 'bold')

ggsave("nmda-amp.png", width = 15, height = 15, units = "cm")

#ampa:nmda ratios
ggplot(subset(dfminis, age=="24" | age =='28'), aes(x=age, y = AN_ratio, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), 
        legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'AMPA:NMDA ratio') + 
  stat_compare_means(label = 'p.signif', comparisons = list(c('24', '28')), face = 'bold') 


ggsave("AN-ratio.png", width = 15, height = 15, units = "cm")

#2-way anova
res.aov <- aov(AN_ratio ~ age + group, data = dfminis, na.action = na.omit)
summary(res.aov)
windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(res.aov)

#mini amplitude cumulative frequency 
dfminicm<-read_csv("minis-cum-freq.csv") #load csv

#ampa PC28
ggplot(subset(dfminicm, receptor=="ampa"), aes(x=amp, y = percentage, colour = group)) +
  geom_line(size=1) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), 
        legend.position = 'none') + 
  labs(x = 'Amplitude (pA)', y = 'Cumulative frequency (%)') +
  geom_ribbon (aes(ymin = percentage + sem, ymax = percentage - sem, 
                            fill = group), alpha = 0.2, colour = NA) +
  facet_wrap(~age) + 
  theme(strip.text.x = element_text(size=18, face = 'bold', colour = 'black'),
                        strip.background = element_rect(fill="white")) 

ggsave("ampa-cum-freq.png", width = 15, height = 15, units = "cm")

#Kolmogorov-Smirnov Tests
ks.test(dfminicm$percentage[dfminicm$group == 'preterm' | dfminicm$receptor == 'ampa'],
         dfminicm$percentage[dfminicm$group == 'vehicle' | dfminicm$receptor == 'ampa'])

#nmda PC28
ggplot(subset(dfminicm, receptor=="nmda"), aes(x=amp, y = percentage, colour = group)) +
  geom_line(size=1) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), 
        legend.position = 'none') + 
  labs(x = 'Amplitude (pA)', y = 'Cumulative frequency (%)') +
  geom_ribbon (aes(ymin = percentage + sem, ymax = percentage - sem, 
                   fill = group), alpha = 0.2, colour = NA) +
  facet_wrap(~age) + 
  theme(strip.text.x = element_text(size=18, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  scale_x_continuous(breaks = c(0, 60, 120))


ggsave("nmda-cum-freq.png", width = 15, height = 15, units = "cm")

#Kolmogorov-Smirnov Tests
ks.test(dfminicm$percentage[dfminicm$group == 'preterm' | dfminicm$receptor == 'nmda'],
        dfminicm$percentage[dfminicm$group == 'vehicle' | dfminicm$receptor == 'nmda'])

