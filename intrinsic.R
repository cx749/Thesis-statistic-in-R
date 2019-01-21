.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)
library(ggsignif)

##load data
dfint<-read_csv("intrinsic.csv") #load csv


dfint <- dfint %>%
  mutate(AP_peak_first = as.numeric(AP_peak_first), AP_width_first = as.numeric(AP_width_first), 
         age = as.factor(age), AP_maxdvdt_first = as.numeric(AP_maxdvdt_first), 
         AP_thres_first = as.numeric(AP_thres_first), Ri = as.numeric(Ri),
         Vm = as.numeric(Vm), AP_height = as.numeric(AP_height), rheo_mV = as.numeric(rheo_mV),
         tau = as.numeric(tau), sag_fit = as.numeric(sag_fit), total_num_spikes = as.numeric(total_num_spikes),
         Cm = as.numeric(Cm), holding = as.factor(holding), group = as.factor(group))

dfint <- dfint %>%
  group_by(age)

mycomparisons <- list(c('22', '28'))

#AP peak
peak <- ggplot(dfint, aes(x=age, y = AP_peak_first, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'AP peak (mV)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
                     #label.y = c(22, 22, 22, 28), size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 

peak
ggsave("Intrinsic_graphs/AP-peak.png", plot = peak, width = 20, height = 20, units = "cm")

#AP height 
height <- ggplot(dfint, aes(x=age, y = AP_height, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'AP height (mV)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = c(22, 22, 22, 28), size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 

ggsave("Intrinsic_graphs/AP-height.png", plot = height, width = 20, height = 20, units = "cm")


#AP width
width <- ggplot(dfint, aes(x=age, y = AP_width_first, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'AP width (ms)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 

ggsave("Intrinsic_graphs/AP-width.png", plot = width, width = 20, height = 20, units = "cm")

#max_dvdt
rise <- ggplot(dfint, aes(x=age, y = AP_maxdvdt_first, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Max rise-time (V/s)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 

ggsave("Intrinsic_graphs/AP-risetime.png", plot = rise, width = 20, height = 20, units = "cm")

#threshold
thres <- ggplot(dfint, aes(x=age, y = AP_thres_first, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Threshold (mV)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 



ggsave("Intrinsic_graphs/AP-threshold.png", plot = thres, width = 20, height = 20, units = "cm")
#Ri

Input <- ggplot(dfint, aes(x=age, y = Ri, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Input resistance (MOhms)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


ggsave("Intrinsic_graphs/Input-resistance.png", plot = Input, width = 20, height = 20, units = "cm")


#tau
tau <- ggplot(dfint, aes(x=age, y = tau, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Tau (ms)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


ggsave("Intrinsic_graphs/Tau.png", plot = , width = 20, height = 20, units = "cm")

#sag
sag <- ggplot(dfint, aes(x=age, y = sag_fit, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Sag (mV)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


ggsave("Intrinsic_graphs/Sag.png", plot = sag, width = 20, height = 20, units = "cm")

#capacitance
Cm <- ggplot(dfint, aes(x=age, y = Cm, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Capacitance (ms/Mohm)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


Cm

ggsave("Intrinsic_graphs/capacitance.png", plot = Cm, width = 20, height = 20, units = "cm")

#Membrane potential
Vm <- ggplot(dfint, aes(x=age, y = Vm, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Membrane potential (mV)') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


ggsave("Intrinsic_graphs/Membrane-potential.png", plot = Vm, width = 20, height = 20, units = "cm")

#rheobase
rheo <- ggplot(dfint, aes(x=age, y = rheo_mV, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Rheobase (pA))') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  #stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 
  geom_signif(comparisons = list(c("22", "28")), 
              map_signif_level=TRUE)

rheo

ggsave("Intrinsic_graphs/Rheobase.png", plot = rheo, width = 20, height = 20, units = "cm")

#spike number
spikes <- ggplot(dfint, aes(x=age, y = total_num_spikes, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Number of spikes') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 


ggsave("Intrinsic_graphs/Total-spikes.png", plot = spikes, width = 20, height = 20, units = "cm")


#normalised spike number
spikes <- ggplot(dfint, aes(x=age, y = norm_total_num, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (post-conception day)', y = 'Number of spikes') +
  facet_wrap(~holding) + 
  theme(strip.text.x = element_text(size=24, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif', 
  #label.y = 5, size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons, face = 'bold') 

spikes

ggsave("Intrinsic_graphs/Total-spikes-norm.png", plot = spikes, width = 20, height = 20, units = "cm")


##############
#2-way anova group compare
res.aov <- aov(norm_total_num ~ age + group, data = dfint[dfint$holding == '-79'], na.action = na.omit)
summary(res.aov)
TukeyHSD(res.aov)
windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(res.aov)

res.aov <- aov(AP_width_first ~ age + group, data = dfint[dfint$holding == '-RMP'],na.action = na.omit)
summary(res.aov)


#anonva for holding compare
res.aov <- aov(AP_thres_first ~ holding + group + age, data = dfint, na.action = na.omit)
summary(res.aov)
TukeyHSD(res.aov, ordered = TRUE)

windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(res.aov)

#correlated RMP and threshold
ggplot (subset(dfint, holding %in% c('-79')), aes(x = Vm, y = AP_maxdvdt_first, colour = age)) +
  geom_point() +
  geom_smooth(method = 'lm', colour = 'darkgrey') + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none')  
  #labs(x = 'RMP (mV)', y = 'Total number of spikes') 

reg <- lm(Vm ~ total_num_spikes, data = subset(dfint, holding == 'RMP'))
summary(reg) #- slope = -0.0777, p < 0.001, R adjusted = 0.63
