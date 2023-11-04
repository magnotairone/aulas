# Aprendizagem Estatistica de Maquina II
# Aula 4
# hclust - marcas de whisky

library(tidyverse)
library(factoextra)

X <- read.csv("whisky.csv") %>% 
  as.matrix()

View(X)

n <- nrow(X) # pessoas
m <- ncol(X) # marcas

# matriz de zeros
D <- matrix(0, nrow = m, ncol = m) 

View(D)

# nomeia as linhas e colunas de D
dimnames(D) <- list(colnames(X), colnames(X)) 

# calculo da distancia de Jacard
for (j in 1:m) {
  for (k in 1:j) {
    if (j != k) {
      
      D[j, k] <- 1 - sum(X[, j] * X[, k]) / 
        (sum(X[, j]) + sum(X[, k]) - sum(X[, j] * X[, k]))
      
      D[k, j] <- D[j, k] # propriedade de simetria da distancia
    }
  }
}

View(D)

# forma de obter a matriz de distancia de Jaccard usando uma funcao
D1  <- proxy::dist(X, by_rows = FALSE, method = "Jaccard")
class(D1)
View(as.matrix(D1))

# algoritmo hierachical cluster
hc <- hclust(as.dist(D), method = "single")
# hc <- hclust(D1)

# para obter a qual cluster cada observacao pertence
nclusters <- 5 
cutree(hc, k = nclusters)

# visualizacao do dendrograma
fviz_dend(hc, 
          cex = 1.2, 
          k = nclusters,
          main = "Scotch whisky", 
          labels_track_height = 0.5,
          k_colors = RColorBrewer::brewer.pal(nclusters, 
                                              "Dark2"),
          horiz = TRUE)

# como visualizar essas marcas em um grafico bidimensional?

