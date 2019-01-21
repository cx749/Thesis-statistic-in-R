## load libraries
.libPaths("O:/paths") #set library path
library(nlme)  # loading up the nlme library
library(lme4)
library(mgcv)
library(tidyverse)

##load data
df1<-read_csv("weight.csv") #load weight.csv

## tidy data 

df1 <- df1%>%
  mutate(weight = as.numeric(weight), litter_size = as.numeric(litter_size), dev = as.numeric(dev)) # weight and litter size to numeric


df1 <- df1 %>% arrange(dev) # arrange in order of dev column

df1 <- df1%>%
  mutate(group = factor(group), ID = factor(ID), litter = factor(litter), 
         sex = factor(sex)) # variables to factor

df1 <- df1 %>%
  na.omit(weight) # get rid of rows with na in weight column


## visualisation of raw data 

#weight with age
ggplot(df1, aes(x = age,y =  weight)) +
  geom_point(aes(colour = group))

#dev day with age
ggplot(df1, aes(x = age,y =  dev)) +
  geom_point(aes(colour = group))



#############
#glmm 
model2<- lmer(weight ~ age + group + # fixed effects of group and age
                (age|litter/ID) + (age|litter_size), # random effects of ID (nested in group) and litter size(intercept and slope)
              data = df1)

sresid2 <- resid(model2, type = "pearson")  # Extract the standardised residuals
hist(sresid2) # check residuals for normality - look good
qqnorm(sresid2);qqline(sresid2) # alternative normality check - also looks good
plot(sresid2) # check redisuals - poor, model not accounting for all of variability 
AIC(model2) 

drop1(model2, ~., test="Chisq") 
summary(model2)

df1 <- df1 %>%
  mutate(pred = fitted(model2)) # get predicted values from model

#visualise model prediction
ggplot(df1, aes(x = age)) +
  geom_point(aes(y=weight)) +
  geom_line(aes(y = pred, colour=ID))  # prediction are linear and original data is not - model is poor fit
#not improved by removing effects

#########
## try a GAMM to account for non linearity of data
modeladd2 <- gam(weight~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
             s(litter, bs = 're') + s(ID, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df1)

summary(modeladd2)

a <- aov(weight ~ age + group, data = df1, na.action = na.omit)
summary(a)

modeladd2 <- gamm(weight ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                  random = list(ID = ~1, litter = ~1), #random effects  (ID (repeated measures))
                  family = gaussian (link = 'identity'),  
                  na.action=na.omit, data = df1)

windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modeladd2$gam) #plot shape of model
sresid1 <- modeladd2$gam$residuals     # Extract the standardised residuals
hist(sresid1)  
qqnorm(sresid1);qqline(sresid1)#check normality -#check normality - looks good#check normality - looks good
fitted.glmm2 <- modeladd2$gam$fitted              # Extract the fitted (predicted) values
plot(sresid1 ~ fitted.glmm2)                    # Check for homoscedasticity - looks ok (improve??)

plot(predict(modeladd2$gam) ~ na.omit(df1$weight))

wei <- summary(modeladd2$gam) #summary of GAM results
capture.output(wei, file = 'weight-summary.txt')

summary(modeladd2$gam)

summary(modeladd2$lme) 

#predictions from GAMM
pdat <- expand.grid(age=seq(20,39,1), group=c('preterm', 'vehicle'))
pred <- predict(modeladd2$gam, newdata = pdat, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame <- data.frame(pdat, weight = pred$fit, se = pred$se.fit)
pred.frame$ci <- pred.frame$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = weight, colour = group)) +
  geom_point(data = df1) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  xlim(20, 40) +
  geom_line(data = pred.frame, size = 1) +
  geom_ribbon (data = pred.frame, aes(ymin = weight + ci, ymax = weight - ci, 
                                      fill = group), alpha = 0.2, colour = NA) + 
  labs(x = 'Age (post-conception day)', y = 'Weight (g)') 

ggsave("growth-curve.png", width = 15, height = 15, units = "cm")
#DEV modelling

#glmm 


model3 <- gam(dev ~ s(age, by = group) + group   #fixed effect age (with default spline) and group
                 , #random factors (all indluced, then sig checked and removed if over 0.05)
               family = poisson(link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df1)
summary(model3)



modeladd3 <- gamm(dev ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                  random = list(ID = ~1), #random effect of ID (repeated measures)
                  family = poisson (link = 'sqrt'), 
                  na.action = na.omit, data = df1)

windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modeladd3$gam) #plot shape of model
sresid3 <- modeladd3$gam$residuals     # Extract the standardised residuals
hist(sresid3)  #check normality
qqnorm(sresid3);qqline(sresid3) #check normality 
fitted.glmm3 <- modeladd3$gam$fitted              # Extract the fitted (predicted) values
plot(sresid3 ~ fitted.glmm3)                    # Check for homoscedasticity - looks ok 

plot(predict(modeladd3$gam) ~ na.omit(df1$dev))

d <- summary(modeladd3$gam) #summary of GAM results
capture.output(d, file = 'dev-day-summary.txt')
summary(modeladd3$gam)
#predictions from GAMM
pdatdev <- expand.grid(age=seq(20,39,1), group=c('preterm', 'vehicle'))
preddev <- predict(modeladd3$gam, newdata = pdatdev, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.dev <- data.frame(pdatdev, dev = preddev$fit, se = preddev$se.fit)
pred.frame.dev$ci <- pred.frame.dev$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = dev, colour = group)) +
  geom_point(data = df1) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  geom_line(data = pred.frame.dev, size = 1) +
  geom_ribbon (data = pred.frame.dev, aes(ymin =dev + ci, ymax = dev - ci, 
                                      fill = group), alpha = 0.2, colour = NA) +
labs(x = 'Age (post-conception day)', y = 'Developmental day') +
  xlim(20, 40)

ggsave("dev-curve.png", width = 15, height = 15, units = "cm")


#stop at PC35d?
df1 <- df1 %>%
mutate(dev_short = dev) %>%
  mutate(dev_short = replace(dev_short, which (age > 35), NA))


modeladd3 <- gamm(dev_short ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                  random = list(ID = ~1), #random effect of ID (repeated measures)
                  family = poisson (link = 'sqrt'), 
                  na.action = na.omit, data = df1)

summary(modeladd3$gam)

pdatdev <- expand.grid(age=seq(20,35,1), group=c('preterm', 'vehicle'))
preddev <- predict(modeladd3$gam, newdata = pdatdev, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.dev <- data.frame(pdatdev, dev_short = preddev$fit, se = preddev$se.fit)
pred.frame.dev$ci <- pred.frame.dev$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = dev_short, colour = group)) +
  geom_point(data = df1) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=24, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  geom_line(data = pred.frame.dev, size = 1) +
  geom_ribbon (data = pred.frame.dev, aes(ymin =dev_short + ci, ymax = dev_short - ci, 
                                          fill = group), alpha = 0.2, colour = NA) +
  labs(x = 'Age (post-conception day)', y = 'Developmental day') +
  xlim(20, 35)

ggsave("dev-curve-short.png", width = 15, height = 15, units = "cm")
