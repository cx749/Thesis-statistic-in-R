## load libraries
.libPaths("O:/paths") #set library path
library(tidyverse)

##load data
df3<-read_csv("eye-opening.csv") #load csv


ggplot (df3, aes(x = group, y = eye_opening, fill=group)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = 'solid', colour = "black"),
        axis.title = element_text(size=18, face='bold', colour = 'black'), 
        axis.text = element_text(size=18, face='bold', colour = 'black'), legend.position='none') +
  labs(x = NULL, y = 'Day of eye opening (PC)') +
  stat_compare_means(label = 'p.signif', label.y = 36.5) 


ggsave("eye-opening.png", width = 15, height = 15, units = "cm")


hist(df3$eye_opening) 

wilcox.test (eye_opening ~ group, data=df3) #p-value = 0.6563

df3 %>% 
  group_by(group) %>%
  summarise(mean = mean(eye_opening), n = n(), SD = sd(eye_opening))
