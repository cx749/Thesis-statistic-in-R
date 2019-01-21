lido <- read_csv("lidocaine.csv")

lido <- lido %>%
  mutate(type = as.factor(type))

lido$ordered <- factor(lido$type, levels = rev(levels(factor(lido$type))))

#create default boxplot function
default.single.line <- list(geom_line(size = 1),
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
                               scale_fill_brewer(palette='Greys'))

#whole brain frequency
ggplot(lido, aes(ordered,rsr, fill = type, group = ID)) + 
 default.single.line +
scale_y_continuous(limits = c(0, 25)) + 
  labs(x = 'Postnatal day', y = 'Frequency (events/min)')

ggsave("lidocaine-rsr-line.png", width = 10, height = 15, units = "cm")


lidos <- read_csv("lido-stats.csv")

shapiro.test(lidos$after)

t.test(lidos$before, lidos$after, paired = TRUE)

