library(tidyverse)
library(patchwork)
library(rpart)
library(rpart.plot)

# Geração de dados --------------------------------------------------------

set.seed(42)

n <- 30
dados <- tibble(x1 = sort(sample(seq(10,30, by=0.5), n)),
             x2 = sample(seq(10,30, by=0.5), n),
             y = ifelse((x2 < 13) | (x1 > 20 & x2 > 22),
                      "Usou", "Não usou"))

dados %>% ggplot(aes(x1, x2, color=y)) +
  geom_point(size=2)

# definicao de valores de cortes na variavel x1 
cortes_x1 <- unique(dados$x1)
cortes_x1 <- (cortes_x1[-1] + cortes_x1[-length(cortes_x1)])/2

# plot de todos os cortes de x1
dados %>% 
  ggplot(aes(x1, x2, color=y)) +
  geom_point(size=2) +
  geom_vline(xintercept = cortes_x1, 
             linetype="dashed")


# definicao de valores de cortes na variavel x2
cortes_x2 <- sort(unique(dados$x2))
cortes_x2 <- (cortes_x2[-1] + cortes_x2[-length(cortes_x2)])/2

# plot de todos os cortes de x2
dados %>% 
  ggplot(aes(x1, x2, color=y)) +
  geom_point(size=2) +
  geom_hline(yintercept = cortes_x2, 
             linetype="dashed")

# funcao que calcula o erro para cada valor de corte de x1
funcao_x1 <- function(c){
  n1 <- sum(dados$x1 < c)
  n2 <- sum(dados$x1 >= c)
  
  (1-max(table(dados$y[dados$x1 <= c])/n1)) + 
    (1-max(table(dados$y[dados$x1 > c])/n2))
}

resultados_x1 <- data.frame(corte_x1 = cortes_x1, erro = NA)

(resultados_x1 <- resultados_x1 %>% 
  mutate(erro = map_dbl(corte_x1, funcao_x1)))


# funcao que calcula o erro para cada valor de corte de x2
funcao_x2 <- function(c){
  n1 <- sum(dados$x2 < c)
  n2 <- sum(dados$x2 >= c)
  
  (1-max(table(dados$y[dados$x2 <= c])/n1)) + 
    (1-max(table(dados$y[dados$x2 > c])/n2))
}

resultados_x2 <- data.frame(corte_x2 = cortes_x2, v = NA)

(resultados_x2 <- resultados_x2 %>% 
    mutate(erro = map_dbl(corte_x2, funcao_x2)))

# plot com o melhor corte para x1
g1 <- dados %>% 
  ggplot(aes(x1, x2, color=y)) +
  geom_point(size=2) +
  geom_vline(xintercept = resultados_x1$corte_x1[which.min(resultados_x1$erro)],
             color = "red")+
  labs(title = round(min(resultados_x1$erro), 2))+
  theme(legend.position = "none")

# plot com o melhor corte para x2
g2 <- dados %>% 
  ggplot(aes(x1, x2, color=y)) +
  geom_point(size=2) +
  geom_hline(yintercept = resultados_x2$corte_x2[which.min(resultados_x2$erro)],
             color = "red") +
  labs(title = round(min(resultados_x2$erro), 2))

g1 + g2

# Portanto, o corte deve ser em x2 >= 13
resultados_x2 %>% filter(erro == min(erro))

# Funcao que obtem a arvore -----------------------------------------------
ctree <- rpart(y ~ . , data = dados,
               control = rpart.control(minsplit = 1, cp = 0))

?rpart.control

rpart.plot(ctree)

?rpart.plot(ctree)

rpart.plot(ctree, extra=104)
