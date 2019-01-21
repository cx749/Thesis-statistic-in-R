.libPaths("O:/paths") #set library path
library(tidyverse)

##load data
dfCO<-read_csv("CO.csv") #load csv

dfCO <- dfCO %>%
  mutate(age = as.factor(age))

mycomparisons_co <- list(c('22', '24'))

give.n <- function(x){
  return(c(y = -0.001, label = length(x))) }
  
#BFI-CO
ggplot(dfCO, aes(x=age, y = BFI, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = 'Age (post-conception day', y = 'BFI-CO') +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons_co, face = 'bold') 


ggsave("BFI-CO.png", width = 15, height = 15, units = "cm")

#width
ggplot(dfCO, aes(x=age, y = width, fill = group)) +
  geom_boxplot() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=20, face='bold', colour = 'black'), 
        axis.text = element_text(size=20, face='bold', colour = 'black'), legend.position='none') + 
  labs(x = 'Age (post-conception day)', y = 'Width (µm)')  +
  stat_compare_means(label = 'p.signif', comparisons = mycomparisons_co, face = 'bold') 


ggsave("cortex-width.png", width = 15, height = 15, units = "cm")

res.aov1 <- aov(width ~ age + group, data = dfCO)
summary(res.aov1)
plot(res.aov1)
