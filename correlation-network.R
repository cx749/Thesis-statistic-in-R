.libPaths("O:/paths") #set library path
install.packages('corrplot')
library(corrplot)
install.packages("heatmaply")
library(heatmaply)
library('tidyverse') #load tidyverse



corr_7 <- read_csv("corrP7.csv")

Nm <- as.matrix(corr_7)

Nm <- as.matrix(corr_7)
Nm[Nm<0.3]=0
diag(Nm) <- 0

g <-network(Nm, directed = FALSE)

ggnet2(g, label = TRUE)



##

ord <- corrMatOrder(Nm, order="hclust")

corrplot(Nm, method = 'color',cl.lim = c(0,1))

corrplot(Nm, method = 'color',cl.lim = c(0,1), order = 'hclust', addrect = 4)

heatmaply_cor(Nm)

M <-cor(mtcars)


Nm <- as.matrix(mat_9)

corrplot(Nm, method = 'color')

install.packages('network')
library(network)
install.packages('sna')
library(sna)
install.packages("GGally")
library(GGally)


Nm <- as.matrix(corr_9)
Nm[Nm<0.39]=0
diag(Nm) <- 0

g <-network(Nm, directed = FALSE)

ggnet2(g, label = TRUE)

summary(g)
plot(g)

ggsave("corr-network-P9.png", width = 15, height = 15, units = "cm")

#example for network
m <- matrix(rbinom(25,1,.4),5,5)
diag(m) <- 0
g <- network(m, directed=FALSE)
ggnet2(g)
summary(g)

install.packages('igraph')
library(igraph)

Nm <- as.matrix(corr_9)
Nm[Nm<0.35]=0
diag(Nm) <- 0

network=graph_from_adjacency_matrix(Nm, weighted=T, mode="undirected", diag=F)
plot(network)
