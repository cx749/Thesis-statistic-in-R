## load libraries
.libPaths("O:/paths") #set library path
library(tidyverse)
library(ggpubr)


##load data
df4<-read_csv("litter-data.csv") #load csv

#tidy data
df4 <- df4%>%
  mutate(litter_size = as.numeric(litter_size), survival_fraction = as.numeric(survival_fraction),
         birth_weight = as.numeric(birth_weight)) #convert variables to numeric


#gestation length - vehicle - preterm


ggplot (subset(df4,group %in% c("vehicle","preterm")), aes(x=group, y=gestation, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Gestation length (hrs)') +
  stat_compare_means(label = 'p.signif', label.y = 445) 


ggsave("gestation-length.png", width = 15, height = 15, units = "cm")


#stats
hist(df4$gestation[df4$group == 'preterm'])
hist(df4$gestation[df4$group == 'vehicle'])
#not normal?

wilcox.test (gestation ~ group, data = df4, subset = group %in% c('preterm', 'vehicle')) 
 
 #summarise data
 df4 %>% 
   group_by(group) %>%
   summarise(mean = mean(gestation), n = n(), SD = sd(gestation), max(gestation), min(gestation))
 
 
 #gestation length - vehicle - natural


ggplot (subset(df4,group %in% c("vehicle","natural")), aes(x=group, y=gestation, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Gestation length (hrs)') +
  stat_compare_means(label = 'p.signif', label.y = 480) +
  scale_fill_manual(values=c('darkorchid3', '#00BFC4')) + 
  ylim(430, 500)


ggsave("natural_gestation-length-clims.png", width = 15, height = 15, units = "cm")

shapiro.test(df4$gestation[df4$group == 'preterm'])
shapiro.test(df4$gestation[df4$group == 'vehicle'])

wilcox.test (gestation ~ group, data = df4, subset = group %in% c('natural', 'vehicle')) 
             
#survival - preterm vehicle
ggplot (subset(df4,group %in% c("vehicle","preterm")), aes(x=group, y=survival_fraction, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') +
  labs(x = NULL, y = 'Survival fraction') 

ggsave("survival.png", width = 15, height = 15, units = "cm")

hist(df4$survival_fraction[df4$group == 'preterm'])
hist(df4$survival_fraction[df4$group == 'vehicle'])


wilcox.test (survival_fraction ~ group, data = df4, subset = group %in% c('preterm', 'vehicle'))

df4 %>% 
  group_by(group) %>%
  summarise(mean = mean(survival_fraction), n = n(), SD = sd(survival_fraction))


#survivial - vehicle - natural
ggplot (subset(df4,group %in% c("vehicle","natural")), aes(x=group, y=survival_fraction, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Survival fraction') +
  scale_fill_manual(values=c('darkorchid3', '#00BFC4'))


ggsave("natural_survival-fraction.png", width = 15, height = 15, units = "cm")

wilcox.test (survival_fraction ~ group, data = df4, subset = group %in% c('natural', 'vehicle'))
#birth weigt - vehcile - preterm
ggplot (subset(df4,group %in% c("vehicle","preterm")), aes(x=group, y=birth_weight, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') +
  labs(x = NULL, y = 'Birth weight (g)')  +
  stat_compare_means(label = 'p.signif', method = 't.test', label.y = 1.4) 


ggsave("Birth-weight.png", width = 15, height = 15, units = "cm")

hist(df4$birth_weight[df4$group == 'preterm'])

t.test(birth_weight~group, data = df4, subset = group %in% c('preterm', 'vehicle')) 

# birth weight - vehcile- natural
ggplot (subset(df4,group %in% c("vehicle","natural")), aes(x=group, y=birth_weight, fill=group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = NULL, y = 'Birth weight (g)') +
  scale_fill_manual(values=c('darkorchid3', '#00BFC4')) + 
  ylim(1.1, 1.8)


ggsave("natural_birthweight-clims.png", width = 15, height = 15, units = "cm")

t.test(birth_weight~group, data = df4, subset = group %in% c('natural', 'vehicle'))

## correlations
#gestation-litter size

ggplot (subset(df4, group %in% c('vehicle')), aes(x = gestation, y = litter_size)) +
  geom_point(colour = '#00BFC4') +
  geom_smooth(method = 'lm', colour = 'darkgrey') + 
theme(panel.background = element_rect(fill = "white"),
      axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
      axis.title = element_text(size=18, face='bold', colour = 'black'), 
      axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = 'Gestation length (hrs)', y = 'Litter size')

ggsave("corr_litter_size-gest.png", width = 15, height = 15, units = "cm")

reg1 <- lm(gestation ~ litter_size, data = subset(df4, group == 'vehicle'))
summary(reg1) #- slope = -6.989, p < 0.001, R adjusted = 0.5729
summary(reg1)$r.squared# 0.573
plot(reg1, which = 1)

#litter size -gestation - prterm
ggplot (subset(df4, group %in% c('preterm')), aes(x = gestation, y = litter_size)) +
  geom_point(colour = '#F8766D') +
  geom_smooth(method = 'lm', colour = 'darkgrey') + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = 'Gestation length (hrs)', y = 'Litter size')

ggsave("corr_preterm_litter_size-gest.png", width = 15, height = 15, units = "cm")

reg1p <- lm(gestation ~ litter_size, data = subset(df4, group == 'preterm'))
summary(reg1p) 
summary(reg1p)$r.squared
plot(reg1p, which = 1)

#litter_size - birthweight
ggplot (subset(df4, group %in% c('vehicle', 'natural')), aes(x = birth_weight, y = litter_size)) +
  geom_point(colour = '#00BFC4') +
  geom_smooth(method = 'lm', colour = 'darkgrey') + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  xlim(NA, 1.82) +
  labs(x = 'Birth weight (g)', y = 'Litter size') 

ggsave("corr_litter_size-birthweight.png", width = 15, height = 15, units = "cm")

reg2 <- lm(birth_weight ~ litter_size, data = subset(df4, group == 'vehicle'))
summary(reg2) #- slope = -0.0777, p < 0.001, R adjusted = 0.63

summary(reg2)$r.squared# 0.659
plot(reg2, which = 1)
#litter_size - birthweight - pretern
ggplot (subset(df4, group %in% c('preterm')), aes(x = birth_weight, y = litter_size)) +
  geom_point(colour = '#F8766D') +
  geom_smooth(method = 'lm', colour = 'darkgrey') + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') + 
  xlim(NA, 1.22) +
  labs(x = 'Birth weight (g)', y = 'Litter size') 




ggsave("corr_preterm_litter_size-birthweight.png", width = 15, height = 15, units = "cm")

reg2p <- lm(birth_weight ~ litter_size, data = subset(df4, group == 'preterm'))
summary(reg2p) #- slope = -0.0140, p < 0.01, R adjusted = 0.2.652

summary(reg2p)$r.squared# 0.300
plot(reg2p, which = 1)
