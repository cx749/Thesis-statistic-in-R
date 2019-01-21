.libPaths("O:/paths") #set library path
library('tidyverse') #load tidyverse


spon_in_stim <- read_csv("spon-in-stim.csv")

spon_in_stim <- spon_in_stim %>%
  mutate(age = factor(age), type = as.factor(type), folder = as.factor(folder))


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


ggplot(spon_in_stim, aes(age,VR)) + 
  default.single.boxplot +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = 'Postnatal day', y = 'Ratio of activity') +
  facet_grid(~type) + 
  theme(strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white"))

ggsave("spon-in-stim-VR.png", width = 15, height = 15, units = "cm")

#make summary of data in new tibble

library('plyr')

cdata <- ddply(spon_in_stim, c("age", 'type'), summarise,
               N    = length(RSR),
               mean = mean(RSR),
               sd   = sd(RSR),
               se   = sd / sqrt(N))


#barchart
ggplot(cdata, aes(x=age, y=mean, fill = age)) +
  geom_bar(position=position_dodge(), stat = 'identity') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width=.2) +
  labs (x = 'Postnatal age', y = 'RRS ratio of activity') +
  theme(plot.title = element_text(size=11, face='bold', hjust=0.5), legend.position="none", 
        axis.title = element_text(face='bold'),axis.text = element_text(face='bold')) +
  facet_grid(~type) + 
  scale_fill_brewer(palette='Set1') + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        plot.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=20, face='bold'),
        axis.text = element_text(size = 16, face = 'bold', colour = 'black'),
        strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) + 
  scale_y_continuous(limits = c(0, 1.5)) 


ggsave("spon-in-stim-RSR-bar.png", width = 15, height = 15, units = "cm")

a <- aov(LB ~ type + age + Error(folder), data = spon_in_stim)
summary(a)


fit <- lmer(LB ~ age + type + (1|folder), data = spon_in_stim)
anova(fit)
summary(glht(fit,linfct=mcp(type = "Tukey", age = "Tukey")))


#matched for stats??
matching <- read_csv("spon-in-stim-pairs.csv")

matching <- matching %>%
  mutate(age = factor(age), subject = as.factor(subject), type = as.factor(type))



shapiro.test(matching$value)

a <- aov(value ~ type + age + Error(subject), data = matching)
summary(a)
TukeyHSD(a, 'type')

TukeyHSD(a, 'type', conf.level=0.95)



a <- aov(value ~ type + age, data = matching)
summary(a)
TukeyHSD(a)

lme_velocity = lmer(Velocity ~ Material, data=scrd, random = ~1|Subject)
anova(lme_velocity)

require(multcomp)
summary(glht(lme_velocity, linfct=mcp(Material = "Tukey")), test = adjusted(type = "bonferroni"))


fit <- lmer(value ~ age + type + (1|subject), data = matching)
anova(fit)
summary(glht(fit,linfct=mcp(type = "Tukey", age = "Tukey")), test = adjusted(type = "bonferroni"))

t.test(value ~ type, data = matching, subset = (age == 5), paired = TRUE)






#matched for stats - sound
matching_s <- read_csv("spon-in-stim-pairs-sound.csv")

matching_s <- matching_s %>%
  mutate(age = factor(age), subject = as.factor(subject), type = as.factor(type))

shapiro.test(matching_s$rb)

a <- aov(vr ~ type + age + Error(subject), data = matching_s)
summary(a)

t.test(vr ~ type, data = matching_s, subset = (age == 9), paired = TRUE)


p.adjust(0.001, method = 'bonferroni', n = 4)



##trimmed

spon_in_stim_t <- read_csv("trimmed-spon-in-stim.csv")

spon_in_stim_t <- spon_in_stim_t %>%
  mutate(age = factor(age), group = as.factor(group))

cdata <- ddply(spon_in_stim_t, c("age", 'group'), summarise,
               N    = length(rsr),
               mean = mean(rsr),
               sd   = sd(rsr),
               se   = sd / sqrt(N))


#barchart
ggplot(cdata, aes(x=age, y=mean, fill = age, alpha = group)) +
  geom_bar(position=position_dodge(), stat = 'identity', colour = 'black') +
  geom_errorbar(position = position_dodge(0.9), aes(ymin = mean-se, ymax = mean+se), width=.2) +
  labs (x = 'Postnatal age', y = 'LRS ratio of activity') +
  theme(plot.title = element_text(size=11, face='bold', hjust=0.5), legend.position="none", 
        axis.title = element_text(face='bold'),axis.text = element_text(face='bold')) +
  scale_fill_brewer(palette='Dark2') + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
        plot.title = element_blank(),
        legend.text = element_text(face='bold'),
        axis.title = element_text(size=20, face='bold'),
        axis.text = element_text(size = 16, face = 'bold', colour = 'black'),
        strip.text.x = element_text(size=20, face = 'bold', colour = 'black'),
        strip.background = element_rect(fill="white")) + 
  scale_y_continuous(limits = c(0, 1.5)) 

ggsave("trimmed-spon-in-stim-RSL-bar.png", width = 15, height = 15, units = "cm")

