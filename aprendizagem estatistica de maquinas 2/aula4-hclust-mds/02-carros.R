# Aprendizagem Estatistica de Maquina II
# Aula 4
# hclust - mercado automobilistico

library(tidyverse)
library(ggrepel)
library(factoextra)

# carregar dados
D <- read_csv("carros.csv")

View(D)

# preparação da matriz 

nomes_linhas <- D$...1
nomes_cols <- colnames(D)[-1] # porque o [-1]?

D <- as.matrix(D[,-1])

dimnames(D) <- list(nomes_linhas, nomes_cols)

# MDS com número máximo de dimensões
mds <- cmdscale(D, k = nrow(D) - 1, eig = TRUE)

# os dois primeiros autovalores são consideravalmente maiores do que os demais
mds$eig %>% 
  round(4)

# Isto indica que um grafico em duas dimensoes pode representar a dissimilaridade dos
# dados de maneira satisfatoria
cumsum(mds$eig) / sum(mds$eig)

# visualizacao em duas dimensoes
df <- tibble(mod = rownames(mds$points),
             coord1 = mds$points[,1],
             coord2 = mds$points[,2])

df %>% 
  ggplot(aes(x = coord1, y = coord2, label = mod)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_text_repel(size = 4) + 
  labs(x = "", y = "")

tema <- theme_classic()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

df %>% 
  ggplot(aes(x = coord1, y = coord2, label = mod)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_text_repel(size = 4) + 
  labs(x = "", y = "") +
  tema

# analise de agrupamento hierarquico de modelos de carros
hc <- hclust(dist(mds$points[, 1:2]), method = "complete") 

# visualizacao do dendrograma gerado
fviz_dend(hc, 
          k = 4,
          main = "Mercado automobilístico",
          k_colors = RColorBrewer::brewer.pal(4, "Dark2"),
          horiz = TRUE) + 
  theme_void()

# TAREFA: varie o metodo de agrupamento e analise os resultados

# considerando 4 grupos
df <- df %>% 
  mutate(cluster = as.factor(cutree(hc, k = 4)))


df %>% 
  ggplot(aes(x = coord1, y = coord2, label = mod)) +
  geom_point(aes(color = cluster), 
             alpha = 0.55, size = 4, 
             show.legend = FALSE) +
  geom_text_repel(size = 5) + 
  labs(x = "", y = "") + 
  tema

# TAREFA: varie o numero de grupos (k) e discuta os resultados