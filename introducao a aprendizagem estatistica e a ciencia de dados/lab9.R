library(ggplot2)
library(GGally)
library(ggrepel)

?USArrests # dados usados na analise

str(USArrests) # estrutura dos dados

ggpairs(USArrests) # visualizacao dos dados

# Analise de componentes principais ---------------------------------------

X <- scale(USArrests, center = TRUE, scale = TRUE)

pca <- prcomp(X)

(Phi <- pca$rotation) # matriz Phi de rotacao

Z <- pca$x # novas coordenadas dos dados: X \times Phi

head(Z)

biplot(pca, scale = 0, cex = 0.75,
       xlab = "PC1",
       ylab = "PC2")

pca$rotation <- -pca$rotation
pca$x <- -pca$x

Z <- pca$x

biplot(pca, scale = 0, cex = 0.75,
       xlab = "PC1",
       ylab = "PC2")

library(factoextra)

# biplot com o pacote factoextra
fviz_pca_biplot(pca, repel = TRUE)


# quais nomes poderiamos dar para as PC1 e PC2?

# proporcao da variancia explicada
(pve <- cumsum(pca$sdev^2) / sum(pca$sdev^2))

# k-medias ----------------------------------------------------------------

dados <- data.frame(ESTADO = rownames(Z),
                    PC1 = Z[,1],
                    PC2 = Z[,2], row.names = NULL)

# grafico de dispersao das duas componentes principais
ggplot(dados, aes(x = PC1, y = PC2, label = ESTADO))+
  geom_point()+
  geom_text_repel()

k <- 2 # numero de clusters

set.seed(1234)

# estimacao do algoritmo k-medias
k_means <- kmeans(dados[,2:3], centers = k, nstart = 100)

k_means

# inicio do grafico com resultado
plot(dados[,2:3], col = k_means$cluster,
     pch = 20, cex = 1.2)

points(k_means$centers, col = rownames(k_means$centers),
       pch = 10, cex = 2)
# fim do grafico com resultado

# mesmo grafico anterior feito no ggplot
ggplot(data.frame(dados, cluster = factor(k_means$cluster)),
       aes(x = PC1, y = PC2, color = cluster))+
  geom_point()+
  geom_point(aes(x = PC1, y = PC2, color = cluster),
             data.frame(k_means$centers, cluster = factor(1:k)),
             shape = 10, size=4)+
  theme_bw()

# escolha do valor de k
?kmeans

k_range <- 2:15 # numero de clusters considerado

W <- numeric(length(k_range))

set.seed(1234)

for (i in 1:length(k_range)) {
  k_means <- kmeans(dados[,2:3], centers = k_range[i], nstart = 100)
  W[i] <- k_means$tot.withinss
}

# grafico da medida de dispersao total intra-clusters
plot(k_range, W, type = "b", lwd = 2, col = "dark green", #yaxt = "n",
     xlab = "Número de clusters", ylab = "Dispersão intra-clusters")

# versao ggplot do grafico acima
ggplot(data.frame(k = k_range, W = W), aes(x = k, y = W)) +
  geom_line(col = "dark green") +
  geom_point(col = "dark green") +
  labs(x = "Número de clusters", y = "Dispersão intra-clusters")+
  theme_bw()


k <- 4 # numero de clusters escolhido

set.seed(1234)

# estimacao do algoritmo k-medias
k_means <- kmeans(dados[,2:3], centers = k, nstart = 100)

ggplot(data.frame(dados, cluster = factor(k_means$cluster)),
       aes(x = PC1, y = PC2, color = cluster))+
  geom_point()+
  geom_point(aes(x = PC1, y = PC2, color = cluster),
             data.frame(k_means$centers, cluster = factor(1:k)),
             shape = 10, size=4)+
  labs(x = "Criminalidade", y = "Urbanização", color = "Cluster")+
  theme_bw()

# Analise do resultado

# valor medio das variaveis por cluster
aggregate(USArrests, list(Cluster = k_means$cluster), mean)

# desvio padrao das variaveis por cluster
aggregate(USArrests, list(Cluster = k_means$cluster), sd)
