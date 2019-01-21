## load libraries
.libPaths("O:/paths") #set library path
library(tidyverse)

##load data
df5<-read_csv("biometry.csv") #load csv


df5 <- df5 %>%
  mutate(age = as.factor(age), brain_w = as.numeric(brain_w), body_w = as.numeric(body_w))

ggplot(df5, aes(x=body_w, y = brain_w, colour = group)) +
  geom_point()

#brain-weight

ggplot (df5, aes(x=age, y=brain_w, fill=group)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white"),
      axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
      axis.title = element_text(size=20, face='bold', colour = 'black'), 
      axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  labs(x = 'Age (Post-conception day)', y = 'Brain weight (g)') +
  #stat_compare_means(aes(group = group), method = 'aov',label = 'p.signif') +
  #label.y = c(22, 22, 22, 28), size = 5) +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons_co, face = 'bold') 

ggsave("brain-weight.png", width = 15, height = 15, units = "cm")

res.aov1 <- aov(brain_w ~ age + group, data = df5)
summary(res.aov1)
plot(res.aov1)


#brain-weight

ggplot (df5, aes(x=age, y=body_w, fill=group)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=12, face='bold', colour = 'grey30'), 
        axis.text = element_text(size=12, face='bold', colour = 'grey30')) + 
  labs(x = 'Age', y = 'Body weight (g)') +
  scale_y_continuous(limits = c(0, 8))

ggsave("body-weight.png", width = 15, height = 15, units = "cm")

res.aov2 <- aov(brain_w ~ age + group, data = df5)
summary(res.aov2)
plot(res.aov2)


#####
df5 <- df5 %>%
  mutate(age = as.numeric(age))
         
model1<- lmer(brain_w ~ age + group + # fixed effects of group and age
                (age|dam), # random effects of ID (nested in group) and litter size(intercept and slope)
              data = df5)

sresid <- resid(model1, type = "pearson")  # Extract the standardised residuals
hist(sresid) # check residuals for normality - look good
qqnorm(sresid);qqline(sresid) # alternative normality check - also looks good
plot(sresid) # check redisuals - poor, model not accounting for all of variability 
AIC(model1) 

drop1(model1, ~., test="Chisq") 
summary(model1)

df1 <- df1 %>%
  mutate(pred = fitted(model2)) # get predicted values from model

#visualise model prediction
ggplot(df1, aes(x = age)) +
  geom_point(aes(y=weight)) +
  geom_line(aes(y = pred, colour=ID))
 