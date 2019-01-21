
max_p <- read_csv("max_pixel.csv")


max_p <- max_p %>%
  mutate(age = factor(age))


ggplot(max_p, aes(age,max_pixel)) + 
  default.single.boxplot + 
  labs(x = 'Postnatal day', y = 'Max fluorescence (df/f%)') + 
  stat_compare_means(label = 'p.signif', comparisons = list(c('1', '9')), face = 'bold') 


ggsave("max_pixel.png", width = 15, height = 15, units = "cm")


shapiro.test(max_p$max_pixel)

a <- aov(max_pixel ~ age, data = max_p)
summary(a)
