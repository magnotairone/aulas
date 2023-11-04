# Aprendizagem Estatistica de Maquina II
# Aula 6
# Estimacao de rede neural multicamada
# Dados: Boston

# Bibliotecas -------------------------------------------------------------

library(MASS)
library(tidyverse)
library(rsample)
library(keras)
library(yardstick)

conflicted::conflict_prefer("select", "dplyr")

# Dados -------------------------------------------------------------------

set.seed(1234)

split <- initial_split(Boston, prop = 0.7)

training <- training(split)
test <- testing(split)

View(training)
View(test)

# Preparando os dados para a rede neural ----------------------------------

X_trn <- training %>% 
  select(-medv) %>% 
  as.matrix()

View(X_trn)

X_tst <- test %>% 
  select(-medv) %>% 
  as.matrix()

X_trn <- scale(X_trn)

X_tst <- scale(X_tst,
               center = attr(X_trn, "scaled:center"),
               scale = attr(X_trn, "scaled:scale"))


net <- keras_model_sequential() %>% 
  layer_dense(units = 32, 
              activation = "relu", 
              input_shape = ncol(X_trn)) %>% 
  # layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  # layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 1)

net <- compile(net, loss = "mse",
               optimizer = "adam", 
               metrics = "mse")    # alternativa: "mae"

summary(net)

history <- fit(net, X_trn, training$medv,
               batch_size = 16, epochs = 50, # defaults: 32, 10
               validation_split = 0.2)


y_hat_net <- predict(net, X_tst)

RMSE_net <- rmse_vec(test$medv, as.numeric(y_hat_net))

(RMSE_net <- sqrt(mean((y_hat_net - test$medv)^2)))


# Regressao linear --------------------------------------------------------

linear <- lm(medv ~ ., training)

y_hat_lm <- predict(linear, test)

(RMSE_lm <- sqrt(mean((y_hat_lm - test$medv)^2)))


# Floresta aleatória ------------------------------------------------------
library(randomForest)

rf <- randomForest(medv ~ ., training)

y_hat_rf <- predict(rf, test)

(RMSE_rf <- sqrt(mean((y_hat_rf - test$medv)^2)))


# Comparativo -------------------------------------------------------------

RMSE_net
RMSE_lm
RMSE_rf