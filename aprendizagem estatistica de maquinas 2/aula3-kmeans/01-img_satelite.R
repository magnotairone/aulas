# Aprendizagem Estatistica de Maquina II
# Aula 3
# k-means - imagens de satelites

# Pacotes que serao utilizados --------------------------------------------
library(tidyverse)
library(patchwork)
library(jpeg)
library(grid)
library(gridExtra)

imagem_88 <- readJPEG("sat_1988.jpg") # carrega arquivo jpg
imagem_18 <- readJPEG("sat_2018.jpg")

class(imagem_88) # verifica a classe da variavel

glimpse(imagem_88) # visualiza de forma resumida o conteudo da variavel

grid.raster(imagem_88) # plota a imagem
dev.off() # remove o plot

grid.raster(imagem_18) # plota a imagem
dev.off() # remove o plot

# funcao que converte o array da imagem em tibble
converte <- function(imagem) {
    tibble(x = rep(1:dim(imagem)[2], 
                   each = dim(imagem)[1]), # coordenada x do pixel
           y = rep(dim(imagem)[1]:1, 
                   dim(imagem)[2]), # coordenada y do pixel
           R = as.vector(imagem[,, 1]), # red
           G = as.vector(imagem[,, 2]), # green
           B = as.vector(imagem[,, 3])) # blue
}

dados_88 <- converte(imagem_88) # converte imagem para tibble
dados_18 <- converte(imagem_18)


# Visualizacao dos dados --------------------------------------------------

# imprimir imagem usando ggplot
dados_88 %>% 
  mutate(rgb = rgb(R, G, B)) %>% # converte os valores de r g e b na cor hexadecimal
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = rgb)) + 
  theme_bw() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")


# Analise de clusters -----------------------------------------------------

k <- 2 # numero de clusters 

# Clusters para imagem de 1988
set.seed(1)
kmeans_88 <- kmeans(dados_88 %>% select(R:B),
                    centers = k)

as.factor(kmeans_88$cluster) %>% 
  levels()

as.factor(kmeans_88$cluster) %>% 
  levels()


# fct_infreq(): muda a ordem dos fatores de acordo com a frequencia de cada nivel (mais frequente primeiro)
dados_88 <- dados_88 %>% 
              mutate(grupo = fct_infreq(as.factor(kmeans_88$cluster)))

dados_88$grupo %>% 
  levels()

kmeans_88$cluster %>% table()

# Clusters para imagem de 2018
kmeans_18 <- kmeans(dados_18 %>% select(R:B),
                    centers = k)

kmeans_18$centers

dados_18 <- dados_18 %>% 
              mutate(grupo = fct_infreq(as.factor(kmeans_18$cluster)))

# calcula procentagem de pixels em cada grupo em 1988
dados_88 %>% 
  count(grupo) %>% 
  mutate(porcentagem = n/sum(n))

# calcula procentagem de pixels em cada grupo em 2018
dados_18 %>% 
  count(grupo) %>% 
  mutate(porcentagem = n/sum(n))

# empilha os dois tibbles acima
dados_88 %>% 
  mutate(ano = 1988) %>% 
  bind_rows(dados_18 %>% 
              mutate(ano = 2018)) %>% 
  group_by(ano) %>% 
  count(grupo) %>% 
  mutate(porcentagem = n/sum(n))


# reconstruindo as imagens usando apenas os clusters

# grafico para 1988
g1 <- dados_88 %>% 
  ggplot(aes(x, y, color = grupo)) + 
  geom_point(size = 0.1, show.legend = FALSE) +
  labs(subtitle = "1988", x = NULL, y = NULL) + 
  scale_color_manual(values = c("#7be391", "#825404")) + # define manualmente as cores
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
          
# grafico para 2018
g2 <- dados_18 %>% 
  ggplot(aes(x, y, color = grupo)) + 
  geom_point(size = 0.1, show.legend = FALSE) +
  labs(subtitle = "2018", x = NULL, y = NULL)  + 
  scale_color_manual(values = c("#7be391", "#825404")) + 
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# os dois graficos lado a lado
g1 + g2
