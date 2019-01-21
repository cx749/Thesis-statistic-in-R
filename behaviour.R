## load libraries
.libPaths("O:/paths") #set library path
library(nlme)  # loading up the nlme library
library(mgcViz)
library(tidyverse)

##load data
df2<-read_csv("behaviour.csv") #load weight.csv

#tidy data
df2 <- df2%>%
  mutate(righting_reflex = as.numeric(righting_reflex), 
         cliff = as.numeric(cliff), geotaxis = as.numeric(geotaxis), tactile = as.numeric(tactile),
         grasping = as.numeric(grasping), whisking = as.numeric(whisking), 
         whisk_stim = as.numeric(whisk_stim), group = as.factor(group), ID = as.factor(ID),
         sex = as.factor(sex), litter = as.factor(litter), litter_size = as.numeric(litter_size)) # variables to numeric


#visualisations

#righting reflex
ggplot(df2, aes(x = age,y =  righting_reflex)) +
  geom_point(aes(colour = group))

#cliff avoidance
ggplot(df2, aes(x = age,y =  cliff)) +
  geom_point(aes(colour = group))

#geotaxis
ggplot(df2, aes(x = age,y =  geotaxis)) +
  geom_point(aes(colour = group))

#tactile responses
ggplot(df2, aes(x = age,y =  tactile)) +
  geom_point(aes(colour = group))


# statistical modelling
########################
#tactile responses

#gamm

modeltac2 <- gam(tactile ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                s(litter, bs = 're') + s(ID, bs = 're'),  #random factors (all indluced, then sig checked and removed if over 0.05)
                 family = poisson(link = 'sqrt'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)

summary(modeltac2)




windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modeltac2$gam) #plot shape of model
sresid2 <- modeltac2$gam$residuals     # Extract the standardised residuals
hist(sresid2)            
qqnorm(sresid2);qqline(sresid2)#check normality -#check normality - looks good
fitted.glmm2 <- modeltac2$gam$fitted              # Extract the fitted (predicted) values
plot(sresid2 ~ fitted.glmm2)                    # Check for homoscedasticity - looks ok (improve??)

plot(predict(modeltac2$gam) ~ na.omit(df2$tactile))

 #summary of GAM results
tac <- summary(modeltac2$gam) #summary of GAM results
capture.output(tac, file = 'tactile-summary.txt')

#visualise

modeltac2 <- gamm(tactile ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                  random = list(ID = ~1, litter = ~1), #random effect of ID (repeated measures)
                  family = poisson(link = 'sqrt'), # zero inflatec count - family??
                  na.action=na.omit, data = df2)

summary(modeltac2$gam)

a <- aov(tactile ~ age + group, data = df2)

summary(a)

#predictions from GAMM
pdattac <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predtac <- predict(modeltac2$gam, newdata = pdattac, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.tac <- data.frame(pdattac, tactile= predtac$fit, se = predtac$se.fit)
pred.frame.tac$ci <- pred.frame.tac$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = tactile, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  geom_line(data = pred.frame.tac, size = 1) +
  geom_ribbon (data = pred.frame.tac, aes(ymin = tactile + ci, ymax = tactile - ci, 
                                      fill = group), alpha = 0.2, colour = NA) +
  labs(x = 'Age (post-conception day)', y = 'Responses to tactile stimulation') 

ggsave("tactile.png", width = 15, height = 15, units = "cm")
###################
#whisker response - glmm

# modelws<- lmer(whisk_stim ~ age + group + # fixed effects of group and age
#                   (age|group/ID) + (1|gestation),  # random effects of ID (nested in group) 
#                 na.action=na.omit, data = df2)
# 
# 
# sresid <- resid(modelws, type = "pearson") 
# plot(sresid ~  na.omit(df2$whisk_stim)) 
# 
# predws <- select(df2, age, group, whisk_stim, ID) 
# 
# predws <- predws %>%
#   na.omit(whisk_stim)
# 
# predws <- predws %>%
#   mutate(pred = fitted(modelws)) # get predicted values from model
# 
# plot(predws$pred ~ predws$whisk_stim) 
# 
# predws.summary <- predws %>%
#   group_by(age) %>%
#   group_by(group) %>%
#   summarise(mean = mean(pred))
# 
# 
# #visualise model prediction
# ggplot(predws, aes(x = age)) +
#   geom_point(aes(y=whisk_stim)) +
#   geom_line(aes(y = predws, colour=ID))  # prediction are linear and original data is not - model is poor fit
# 
# pdatws <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
# predws <- predict(modelws, newdata = pdatws, na.rm = T , type = 'response')
# pred.frame.ws <- data.frame(pdatws, whisk_stim = predws$fit, se = predws$se.fit)
# pred.frame.ws$ci <- pred.frame.ws$se*1.96
# 
# predict(modelws)
# 
# fitted(modelws)
# 
# #
# pdat <- expand.grid(Height = seq(-1.5,2.5,0.1))
# pdat
# pred <- predict (model4, newdata = pdat, level =0, # level = zero for the population fitted line
#                  na.action = na.exclude, type= "response")
# pred
# 
# 
# # Step 3: combine the predictions with the predictors, 
# #         into a final dataframe (predframe)
# predframe <- data.frame (pdat, preds = pred)
# predframe
# 
# # Step 4: plot some graphs of predicted values of y vs x
# 
# lines (predframe$preds ~ predframe$Height, col="red", lwd = 2)  
# 
# 
# 
# 
# sresidws <- resid(modelws, type = "pearson")  # Extract the standardised residuals
# hist(sresidws) # check residuals for normality - look good
# qqnorm(sresidws);qqline(sresidws) # alternative normality check - also looks good
# plot(sresidws) # check redisuals - poor, model not accounting for all of variability 
# 
# drop1(modelws)
# 
# summary(modelws)
# library(MuMIn)
# r.squaredGLMM(modelws)

##whisker response gam

modelws <- gam(whisk_stim ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                 s(litter_size, bs = 're') + s(ID, bs = 're') +
                  s(repeat., bs = 're') + s(sex, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = poisson (link = 'sqrt'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)

summary(modelws)


#repeat matters, more repsonsive on first 
#use only first stim
df2 <- df2 %>%
  mutate(whisk_first = whisk_stim) %>%
  mutate(whisk_first = ifelse(repeat. == 2, 'na', whisk_first)) %>%
  mutate(whisk_first = as.numeric(whisk_first))


modelwsf <- gam(whisk_first ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                 s(litter_size, bs = 're')  + s(sex, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = poisson (link = 'sqrt'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)



summary(modelwsf)
plot(modelwsf)

###



ws <- summary(modelws) #summary of GAM results
capture.output(ws, file = 'whisker-stim-summary.txt')


#visualise

modelws <- gamm(whisk_stim ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                random = list(sex = ~1, ID = ~age, repeat.= ~1, litter_size = ~age), #random effect of ID (repeated measures)
                family = poisson (link = 'sqrt'), # zero inflatec count - family??
                na.action=na.omit, data = df2)

summary(modelws$gam)

windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modelws) #plot shape of model
sresidws<- modelws$gam$residuals     # Extract the standardised residuals
hist(sresidws)            
qqnorm(sresidws);qqline(sresidws)#check normality -#check normality - looks good
fitted.glmmws <- modelws$gam$fitted              # Extract the fitted (predicted) values
plot(sresidws ~ fitted.glmmws)                    # Check for homoscedasticity - looks ok (improve??)

plot(predict(modelws$gam) ~ na.omit(df2$whisk_stim))


#predictions from GAMM
pdatws <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predws <- predict(modelws$gam, newdata = pdatws, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.ws <- data.frame(pdatws, whisk_stim= predws$fit, se = predws$se.fit)
pred.frame.ws$ci <- pred.frame.ws$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = whisk_stim, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  geom_line(data = pred.frame.ws, size = 1) +
  geom_ribbon (data = pred.frame.ws, aes(ymin = whisk_stim + ci, ymax = whisk_stim - ci, 
                                          fill = group), alpha = 0.2, colour = NA) + 
  labs(x = 'Age (post-conception day)', y = 'Responses to whisker stimulation') 

ggsave("whisker_stim.png", width = 15, height = 15, units = "cm")

a <- aov(whisk_stim ~ age + group, data = df2)

summary(a)

model1 <- glmer (whisk_stim ~ age + group + (1|sex) + (age|ID) + (1|repeat.) + (1|litter_size),   # Main effects
               family = poisson (link=sqrt),
               na.action = na.exclude,                        # what to do with any missing values
               data = df2) 

               
anova(model1)
plot(model1)

#####################################
#righting reflex gamm

modelrr <- gam(righting_reflex ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                  s(litter_size, by = group, bs = 're') + s(litter, bs = 're') + s(ID, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
                family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
                na.action=na.omit, data = df2)

summary(modelrr)

windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modelrr) #plot shape of model
sresidrr <- modelrr$residuals     # Extract the standardised residuals
hist(sresidrr)   
qqnorm(sresidrr);qqline(sresidrr)#check normality -
fitted.glmmrr <- modelrr$fitted              # Extract the fitted (predicted) values
plot(sresidrr ~ fitted.glmmrr)                    # Check for homoscedasticity -


rr <- summary(modelrr) #summary of GAM results
capture.output(rr, file = 'RR-summary.txt')

#to plot with ggplot us gamm function
modelrr <- gamm(righting_reflex ~ s(age, by = group) + group,   #fixed effect age (with default spline) and group
                random = list(ID = ~1, litter_size = ~1, litter = ~1),  
                family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
                na.action=na.omit, data = df2)

#predictions from GAMM
pdatrr <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predrr <- predict(modelrr$gam, newdata = pdatrr, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.rr <- data.frame(pdatrr, righting_reflex = predrr$fit, se = predrr$se.fit)
pred.frame.rr$ci <- pred.frame.rr$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = righting_reflex, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  geom_line(data = pred.frame.rr, size = 1) +
  geom_ribbon (data = pred.frame.rr, aes(ymin = righting_reflex + ci, ymax = righting_reflex - ci, 
                                          fill = group), alpha = 0.2, colour = NA) + 
  labs(x = 'Age (post-conception day)', y = 'Time to righting (s)') 


ggsave("righing-reflex.png", width = 15, height = 15, units = "cm")

#try without censored observations
df2 <- df2 %>%
  mutate(right_success = righting_reflex) %>%
  mutate(right_success = replace(right_success, which (right_success == 30), NA))



modelrr <- gam(right_success ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                 s(litter_size, bs = 're') + s(litter, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)

summary(modelrr)

modelrr <- gamm(right_success ~ s(age, by = group) + group,   #fixed effect age (with default spline) and group
                random = list(litter_size = ~1, litter = ~1),  
                family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
                na.action=na.omit, data = df2)



#predictions from GAMM
pdatrr <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predrr <- predict(modelrr$gam, newdata = pdatrr, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.rr <- data.frame(pdatrr, right_success = predrr$fit, se = predrr$se.fit)
pred.frame.rr$ci <- pred.frame.rr$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = right_success, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') + 
  geom_line(data = pred.frame.rr, size = 1) +
  geom_ribbon (data = pred.frame.rr, aes(ymin = right_success + ci, ymax = right_success - ci, 
                                         fill = group), alpha = 0.2, colour = NA) + 
  labs(x = 'Age (post-conception day)', y = 'Time to righting (s)') 


ggsave("righing-reflex_no-censored.png", width = 15, height = 15, units = "cm")#####################################

#geotaxis gamm

modelgeo <- gam(geotaxis ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                 s(litter, bs = 're') + s(sex, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)

summary(modelgeo)

windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
sresidgeo <- modelgeo$gam$residuals     # Extract the standardised residuals
hist(sresidgeo)   
qqnorm(sresidgeo);qqline(sresidgeo)#check normality -
fitted.glmmgeo <- modelgeo$gam$fitted              # Extract the fitted (predicted) values
plot(sresidgeo ~ fitted.glmmgeo)                    # Check for homoscedasticity -
plot(predict(modelgeo$gam) ~ na.omit(df2$geotaxis)) # poor fit - just data variability?

plot(modelgeo$gam) #plot shape of model

summary(modelgeo)
geo <- summary(modelgeo$gam) #summary of GAM results
capture.output(geo, file = 'geo-summary.txt')

#visualise
modelgeo <- gamm(geotaxis ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                 random = list( sex = ~1, litter = ~1), #random effect of ID (repeated measures)
                 family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
                 na.action=na.omit, data = df2)

#predictions from GAMM
pdatgeo <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predgeo <- predict(modelgeo$gam, newdata = pdatgeo, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.geo <- data.frame(pdatgeo, geotaxis = predgeo$fit, se = predgeo$se.fit)
pred.frame.geo$ci <- pred.frame.geo$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = geotaxis, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') +
  labs(x = 'Age (post-conception day)', y = 'Time to geotaxis (s)') +
  geom_line(data = pred.frame.geo, size = 1) +
  geom_ribbon (data = pred.frame.geo, aes(ymin = geotaxis + ci, ymax = geotaxis - ci, 
                                         fill = group), alpha = 0.2, colour = NA)

ggsave("geotaxis.png", width = 15, height = 15, units = "cm")
  
#compare individual ages
#PC 29 and 30 different?

t.test(geotaxis ~ group, df2, subset = age %in% c(30))
# adjust for multiple comparisons?
p.adjust(0.01487, method = 'bonferroni' , n = 14) 

####################################
#cliff avoidance gamm

modelcl <- gam(cliff ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                 s(litter, group, bs = 're') + s(repeat., bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)


summary(modelcl)


windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modelcl$gam) #plot shape of model
sresidcl <- modelcl$residuals     # Extract the standardised residuals
hist(sresidcl)   
qqnorm(sresidcl);qqline(sresidcl)#check normality -
fitted.glmmcl <- modelcl$fitted              # Extract the fitted (predicted) values
plot(sresidcl ~ fitted.glmmcl)                    # Check for homoscedasticity -


plot(predict(modelcl$gam) ~ na.omit(df2$cliff))

cl <- summary(modelcl) #summary of GAM results
capture.output(cl, file = 'cliff-summary.txt')


#
modelcl <- gamm(cliff ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
random = list(repeat. = ~1, litter = ~1), #random effect of ID (repeated measures)
family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
na.action=na.omit, data = df2)

#predictions from GAMM
pdatcl <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predcl <- predict(modelcl$gam, newdata = pdatcl, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.cl <- data.frame(pdatcl, cliff = predcl$fit, se = predcl$se.fit)
pred.frame.cl$ci <- pred.frame.cl$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = cliff, colour = group)) +
  geom_count(data = df2, alpha = 0.2) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') +
  labs(x = 'Age (post-conception day)', y = 'Time to cliff avoidance (s)') +
  geom_line(data = pred.frame.cl, size = 1) +
  geom_ribbon (data = pred.frame.cl, aes(ymin = cliff + ci, ymax = cliff - ci, 
                                         fill = group), alpha = 0.2, colour = NA)

ggsave("cliff-avoidance.png", width = 15, height = 15, units = "cm")

## removed failure/censored observation

df2 <- df2 %>%
  mutate(cliff_success = cliff) %>%
  mutate(cliff_success = replace(cliff_success, which (cliff_success == 30), NA))

modelcl <- gam(cliff_success ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                  s(litter, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)

summary(modelcl)

modelcl <- gamm(cliff_success ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                random = list(litter = ~1), #random effect of ID (repeated measures)
                family = gaussian (link = 'identity'), # continous - bounded at 30 - family = gamm?
                na.action=na.omit, data = df2)

#predictions from GAMM
pdatcl <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predcl <- predict(modelcl$gam, newdata = pdatcl, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.cl <- data.frame(pdatcl, cliff_success = predcl$fit, se = predcl$se.fit)
pred.frame.cl$ci <- pred.frame.cl$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = cliff_success, colour = group)) +
  geom_count(data = df2, alpha = 0.2) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') +
  labs(x = 'Age (post-conception day)', y = 'Time to cliff avoidance (s)') +
  geom_line(data = pred.frame.cl, size = 1) +
  geom_ribbon (data = pred.frame.cl, aes(ymin = cliff_success + ci, ymax = cliff_success - ci, 
                                         fill = group), alpha = 0.2, colour = NA)

ggsave("cliff-avoidance_no-censored.png", width = 15, height = 15, units = "cm")

####################
# grasping gamm

modelgr <- gam(grasping ~ s(age, by = group) + group +  #fixed effect age (with default spline) and group
                 s(litter, bs = 're') + s(sex, bs = 're'), #random factors (all indluced, then sig checked and removed if over 0.05)
               family = poisson (link = 'identity'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)


  
summary(modelgr)
plot(modelgr)




windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modelgr$gam) #plot shape of model
sresidgr <- modelgr$gam$residuals     # Extract the standardised residuals
hist(sresidgr)   
qqnorm(sresidgr);qqline(sresidgr)#check normality -
fitted.glmmgr <- modelgr$gam$fitted              # Extract the fitted (predicted) values
plot(sresidgr ~ fitted.glmmgr)                    # Check for homoscedasticity -

plot(predict(modelgr$gam) ~ na.omit(df2$grasping))


gr <- summary(modelgr$gam) #summary of GAM results
capture.output(gr, file = 'grasping-summary.txt')

#visualise

modelgr <- gamm(grasping ~ s(age, by = group) + group, #fixed effect age (with default spline) and group
                random = list(litter = ~1, sex = ~1), #random effect of ID (repeated measures)
                family = poisson (link = 'identity'), # continous - bounded at 30 - family = gamm?
                na.action=na.omit, data = df2)

summary(modelgr$gam)

#predictions from GAMM
pdatgr <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predgr <- predict(modelgr$gam, newdata = pdatgr, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.gr <- data.frame(pdatgr, grasping = predgr$fit, se = predgr$se.fit)
pred.frame.gr$ci <- pred.frame.gr$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = grasping, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') +
  labs(x = 'Age (post-conception day)', y = 'Grasping score') +
  geom_line(data = pred.frame.gr, size = 1) +
  geom_ribbon (data = pred.frame.gr, aes(ymin = grasping + ci, ymax = grasping - ci, 
                                         fill = group), alpha = 0.2, colour = NA)

 

ggsave("grasping.png", width = 15, height = 15, units = "cm")

#try gamV

df2 <- df2 %>%
  mutate(grasping_gam = grasping + 1)

modelgr2 <- gamV(grasping_gam ~ s(age, by = group) + group + s(ID, group, bs = 're'), 
                 family = ocat(R=8),
                 data = df2, 
                 aViz = list(nsim = 50))


check1D(modelgr2, "ID") + l_gridCheck1D(mean)
check1D(modelgr2, "group") + l_gridCheck1D(mean)
check1D(modelgr2, "age") + l_gridCheck1D(mean)

summary(modelgr2)
print(plot(modelgr2, allTerms = TRUE), pages = 1)
check(modelgr2)

###############
# whisking gamm
modelwh <- gam(whisking ~ s(age, by = group) + group,  #fixed effect age (with default spline) and group
                  #random factors (all indluced, then sig checked and removed if over 0.05)
               family = poisson (link = 'log'), # continous - bounded at 30 - family = gamm?
               na.action=na.omit, data = df2)

summary(modelwh)




windows (6,6); par (mfrow = c (2,2)) ## or pop them all in a single window
plot(modelwh$gam) #plot shape of model
sresidwh <- modelwh$gam$residuals     # Extract the standardised residuals
hist(sresidwh)   
qqnorm(sresidwh);qqline(sresidwh)#check normality -
fitted.glmmwh <- modelwh$gam$fitted              # Extract the fitted (predicted) values
plot(sresidwh ~ fitted.glmmwh)                    # Check for homoscedasticity -

plot(predict(modelwh$gam) ~ na.omit(df2$whisking))

summary(modelwh$gam)
wh <- summary(modelwh$gam) #summary of GAM results
capture.output(wh, file = 'whisking-summary.txt')

#visualise

modelwh <- gamm(whisking ~ s(age) + group, #fixed effect age (with default spline) and group
                random = list(ID = ~1), #random effect of ID (repeated measures)
                family = poisson (link = 'log'), # continous - bounded at 30 - family = gamm?
                na.action=na.omit, data = df2)

#predictions from GAMM
pdatwh <- expand.grid(age=seq(22,35,1), group=c('preterm', 'vehicle'))
predwh <- predict(modelwh$gam, newdata = pdatwh, na.rm = T , type = 'response', se.fit = TRUE)
pred.frame.wh <- data.frame(pdatwh, whisking = predwh$fit, se = predwh$se.fit)
pred.frame.wh$ci <- pred.frame.wh$se*1.96

#plot model over data 
ggplot(NULL, aes(x = age, y = whisking, colour = group)) +
  geom_count(data = df2, alpha = 0.5) + 
  theme(panel.background = element_rect(fill = "white", colour = 'black'),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position = 'none') +
  labs(x = 'Age (post-conception day)', y = 'Whisking score') +
  geom_line(data = pred.frame.wh, size = 1) +
  geom_ribbon (data = pred.frame.wh, aes(ymin = whisking + ci, ymax = whisking - ci, 
                                         fill = group), alpha = 0.2, colour = NA) +
  ylim(0, 2.5) +
  scale_y_continuous(breaks = c(0, 1, 2))

ggsave("whisking.png", width = 15, height = 15, units = "cm")

#try gamV
df2 <- df2 %>%
  mutate(whisking_gam = whisking + 1)

modelwh2 <- gamV(whisking_gam ~ s(age, by = group) + group + s(ID, group, bs = 're'), 
                 family = ocat (R = 6),
                 data = df2, 
                 aViz = list(nsim = 50))



check1D(modelwh2, "ID") + l_gridCheck1D(mean)
check1D(modelwh2, "group") + l_gridCheck1D(mean)
check1D(modelwh2, "age") + l_gridCheck1D(mean)

summary(modelwh2)
print(plot(modelwh2, allTerms = TRUE), pages = 1)
check(modelwh2)

