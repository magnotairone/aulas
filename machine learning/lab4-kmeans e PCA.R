# Machine Learning - 2022-1 - Laboratório 04
# Modelos não supervisionados: 
# k-médias, PCA


# PCA ---------------------------------------------------------------------


# fonte dos dados
# http://www.ssp.sp.gov.br/Estatistica/Pesquisa.aspx
# Acesse a página de dados estatísticos do Secretaria de Segurança Pública de São Paulo 
# para detalhes sobre a produtividade policial e faça o download dos dados para o ano de 2019

# ocorrências de porte de entorpecentes
# ocorrências de tráfico de entorpecentes
# ocorrências de apreensão de entorpecentes(1)
# ocorrências de porte ilegal de arma
# nº de armas de fogo apreendidas
# nº de flagrantes lavrados
# nº de infratores apreendidos em flagrante
# nº de infratores apreendidos por mandado
# nº de pessoas presas em flagrante
# nº de pessoas presas por mandado
# nº de prisões efetuadas
#(1) São computadas somente as ocorrências em que houve apenas apreensão de entorpecentes (sem pessoas envolvidas).


library(tidyverse)
library(readxl)
dados <- read_xlsx("indicadores.xlsx")  %>% 
  mutate(natureza = str_to_lower(natureza))

# obter media mensal
dados %>% 
  select(-total) %>% 
  pivot_longer(names_to = "mes", 
               values_to="total_mes", 
               col=c(-regiao,-natureza)) %>% 
  group_by(regiao, natureza) %>% 
  summarise(media = mean(total_mes)) %>%
  pivot_wider(names_from = "natureza", values_from=media) %>% 
  View()

# obter somente o total anual
dados <- dados %>% 
  select(-(jan:dez)) %>% 
  pivot_wider(names_from = natureza, values_from = total)

write_csv2(dados, "indicadores2.csv")


# inicio ------------------------------------------------------------------

library(factoextra)
library(ggrepel)

dados <- read_csv2("indicadores2.csv")


# PCA ---------------------------------------------------------------------

pca <- dados %>% 
  select(-regiao) %>% 
  prcomp(scale = TRUE, center = TRUE)

# comando equivalente
pca <- prcomp(dados[,which(colnames(dados)!="regiao")],
              scale = TRUE, center = TRUE)

pca

pca$x        # os valores nas novas coordenadas

pca$rotation # loadings/cargas

# sum(pca$rotation[,1]^2) # note que somam 1

options(scipen=999)
round(100 * pca$sdev^2 / sum(pca$sdev^2), 3) # variância explicada

# porcetangem da variancia explicada por cada componente
fviz_eig(pca, addlabels = TRUE)

# Primeira componente (traço vermelho indica contribuição uniforme) -------
# This reference line corresponds to the expected value if the contribution where uniform
pca %>% 
  fviz_contrib(choice = "var", axes = 1, sort.val = "asc",
               fill = "steelblue", color = "black") + 
  labs(x = "") + 
  coord_flip()
# violencia geral

# Segunda Componente ------------------------------------------------------

pca %>% 
  fviz_contrib(choice = "var", axes = 2, sort.val = "asc",
               fill = "steelblue", color = "black") + 
  labs(x = "") + 
  coord_flip()
# entorpecentes

# Projeção em duas dimensões ----------------------------------------------

dados_pca <- data.frame(nome = dados$regiao, 
                        pc1 = pca$x[,1], 
                        pc2 = pca$x[,2]) 

dados_pca %>% 
  ggplot(aes(x = pc1, y = pc2, label = nome)) + 
  geom_point(size = 3) + 
  geom_text_repel()+
  theme_bw()+
  labs(x="Violencia geral", y="Entorpecentes")

plot(dados_pca$pc1, dados_pca$pc2, pch = 19)

# Tarefa: implementar k-means para agrupar cidades parecidas

# RESPOSTA -----------------------
k_range <- 2:11
W <- numeric(length(k_range))

for(i in 1:length(k_range)){
  k_means <- kmeans(pca$x[,1:2], k_range[i], 
                    nstart=10, iter.max = 20)
  
  W[i] <- k_means$tot.withinss
}

ggplot(data.frame(k_range, W), aes(k_range, W)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "k", y = "Dispersão intra-clusters")

k_escolhido <- 4

set.seed(1)

dados_pca <- dados_pca %>% 
  mutate(cluster = factor(kmeans(pca$x[,1:2], 
                                 centers = k_escolhido)$cluster))


ggplot(dados_pca, aes(pc1, pc2, label=nome, col=cluster)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             alpha = 0.5) +
  geom_point(size = 3) + 
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Violencia geral", y = "Entorpecentes")

# FIM RESPOSTA -------------------


# Dados Mall Customers ----------------------------------------------------
library(tidyverse)
library(janitor)

dados <- read_csv("Mall_Customers.csv")

dados <- clean_names(dados)

glimpse(dados)

# relação entre *escore de gasto* e a *renda anual*
dados %>% 
  ggplot(aes(spending_score_1_100, annual_income_k)) +
  geom_point() +
  labs(x = "Escore de gastos", y = "Renda anual")

# *escore de gasto* e a *idade*.
dados %>% 
  ggplot(aes(spending_score_1_100, age)) +
  geom_point() +
  labs(x = "Escore de gastos", y = "Idade")

# e o genero?
g1 <- dados %>% 
  ggplot(aes(annual_income_k, spending_score_1_100, col=gender)) +
  geom_point(show.legend = FALSE) +
  labs(x = "Renda anual", y = "Escore de gastos")

g2 <- dados %>% 
  ggplot(aes(spending_score_1_100, age, col=gender)) +
  geom_point() +
  labs(x = "Escore de gastos", y = "Idade", col="Genero")

library(patchwork)
g1 + g2

# qual a conclusao?


# Vamos usar o método k-médias para segmentar 
# os clientes e definir grupos homogêneos de clientes
dados_pad <- dados %>% 
  select(-customer_id, -gender) %>% 
  scale()

k_range <- 2:20
W <- numeric(length(k_range))

set.seed(1)
for(i in 1:length(k_range)){
  k_means <- kmeans(dados_pad, k_range[i], 
                    nstart=10, iter.max = 20)
  
  W[i] <- k_means$tot.withinss
}

ggplot(data.frame(k_range, W), aes(k_range, W)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "k", y = "Dispersão intra-clusters")

# k = 5 --------------
k_escolhido <- 5

set.seed(1)

dados_cluster <- dados %>% 
  select(-customer_id) %>% 
  mutate(cluster = factor(kmeans(dados_pad, 
                                 centers = k_escolhido)$cluster))

library(plotly)
plot_ly(x = dados_cluster$annual_income_k, 
        y = dados_cluster$spending_score_1_100, 
        z = dados_cluster$age,
        type = "scatter3d", mode = "markers", size = 2,
        color = dados_cluster$cluster, 
        colors = RColorBrewer::brewer.pal(5, "Dark2")) %>% 
  layout(
    scene = list(
      xaxis = list(title = "Renda anual"),
      yaxis = list(title = "Escore de gastos"),
      zaxis = list(title = "Idade")
    ))

# tabela
dados_cluster %>%
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), 
                   list(media = mean)),
            n_clientes = n())