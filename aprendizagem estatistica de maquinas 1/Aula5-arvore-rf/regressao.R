library(MASS)
library(rpart.plot)
library(randomForest)
library(modeldata)
library(pROC)
library(ranger)
library(leaflet)
library(rsample)
library(tidyverse)


# Codigo que verifica proporcao out-of-bag --------------------------------

sample(10, 10, replace = TRUE)

## Abordagem 1 - utilizando a estrutura for

dados <- tibble(B = 1:5000, out = NA) 

pop <- 1:500

indicadora <- function(x, populacao) {
  amostra <- sample(populacao, length(populacao), replace = TRUE)
  return(!any(amostra == x))
}


for(i in 1:nrow(dados)) {
  dados$out[i] <- indicadora(15, pop) #verifica se o elemento 15 esta na amostra
}

dados %>% 
  mutate(prop_out = cumsum(out)/B) %>% 
  ggplot(aes(B, prop_out)) + 
  geom_hline(yintercept = 1/exp(1), 
             linetype = "dashed", color = "red", size = 1) + 
  geom_line(size = 1.2, color = "blue") + 
  ylim(0,1) +
  ylab("Proporção out-of-bag") + 
  theme_bw()


## Abordagem 2 - utilizando programacao funcional

pop <- 1:500

dados <- tibble(B = 1:5000, out = NA) %>% 
  mutate(out = map_lgl(B, ~ !(15 %in% sample(pop, length(pop), replace = TRUE))))

dados %>% 
  mutate(prop_out = cumsum(out)/B) %>% 
  ggplot(aes(B, prop_out)) + 
  geom_hline(yintercept = 1/exp(1), 
             linetype = "dashed", color = "red", size = 1) + 
  geom_line(size = 1.2, color = "blue") + 
  ylim(0,1) +
  ylab("Proporção out-of-bag") + 
  theme_bw()



# Dados Boston ------------------------------------------------------------


# Bagging -----------------------------------------------------------------

data(Boston)

set.seed(123)

par(mfrow = c(2, 3))

for(i in 1:6) {
  
  amostra <- sample(nrow(Boston), size = nrow(Boston), 
                    replace = TRUE)
  
  arvore <- rpart(medv ~ ., data = Boston[amostra, ], 
                  control = rpart.control(cp = 0))
  
  rpart.plot(arvore, roundint = FALSE)
}


# Bagging usando o pacote randomForest ------------------------------------

(bagging <- randomForest(medv ~ ., data = Boston, 
                         mtry = ncol(Boston)-1,
                         ntree = 1500))

# gráfico do MSE (OOB)
tibble(arvore = 1:length(bagging$mse), 
       mse = bagging$mse) %>% 
  ggplot(aes(arvore, mse)) + 
  geom_line(color = "blue", size = 1.2) + 
  ylab("MSE (OOB)") + 
  xlab("Número de Árvores") + theme_bw()


# Floresta aleatoria ------------------------------------------------------

set.seed(123)

(rf <- randomForest(medv ~ ., data = Boston))


# gráfico do MSE (OOB)
tibble(arvore = 1:length(rf$mse), 
       mse = rf$mse) %>% 
  ggplot(aes(arvore, mse)) + 
  geom_line(color = "blue", size = 1.2) + 
  ylab("MSE (OOB)") + 
  xlab("Número de Árvores") + 
  theme_bw()


# Tarefa: tentar diferentes numeros de variaveis
# para serem consideradas a cada split

?randomForest #veja os argumentos da funcao


# Importancia das variaveis -----------------------------------------------

varImpPlot(rf, pch = 19)
