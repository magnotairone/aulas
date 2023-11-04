# Machine Learning - 2022-1 - Laboratório 03 
# Modelos baseados em árvores de decisão: 
# bagging, random forest e boosting

# Carregar dados ----------------------------------------------------------
# descrição dos dados: 
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
library(AmesHousing)

ames <- make_ames()
?make_ames


# Análise descritiva ------------------------------------------------------

dim(ames)

View(ames)

class(ames)

str(ames)


# Dados qualitativos ------------------------------------------------------
# um pouco de manipulacao de dados
library(tidyverse)

obter_variaveis_categoricas_ames <- function(n_categorias){
  ames %>% 
    select(where(is.factor)) %>% 
    pivot_longer(cols = MS_SubClass:Sale_Condition) %>% 
    count(name, value) %>% 
    count(name) %>% 
    filter(n <= n_categorias) %>% 
    select(name) %>% 
    pull()
}

obter_variaveis_nao_categoricas_ames <- function(){
  ames %>% 
    dplyr::select(where(~ !is.factor(.x))) %>% 
    names()
}

dados <- ames[c(obter_variaveis_categoricas_ames(10), 
                obter_variaveis_nao_categoricas_ames())]

dim(dados)

# alternativa
dados <- ames %>% 
  select(c(obter_variaveis_categoricas_ames(10), 
                obter_variaveis_nao_categoricas_ames()))

dim(dados)

# Separação dos dados em treino e teste -----------------------------------
set.seed(123)

idx <- sample(1:nrow(dados), round(0.7 * nrow(dados)))

training <- dados[idx,]
test <- dados[-idx,]

nrow(training)
nrow(test)

resultados <- data.frame(modelo = c("Linear", "Bagging", 
                                    "Random Forest", 
                                    "Boosting"), 
                         rmse = NA)

# Modelo linear -----------------------------------------------------------

linear <- lm(Sale_Price ~ ., data = training)

y_hat_linear <- predict(linear, newdata = test)

# e agora?
table(dados$Electrical)
table(training$Electrical)
table(test$Electrical)


# Tratando fatores --------------------------------------------------------

library(forcats)
tmp <- dados$Electrical
table(tmp)

tmp2 <- fct_lump(tmp, n = 3, other_level = "Outro")
table(tmp2)

tmp3 <- fct_lump(tmp, p = 0.05, other_level = "Outro")
table(tmp3)

dados_lump <- ames %>% 
  # mutate(across(obter_variaveis_categoricas_ames(10),
  mutate(across(where(is.factor),
                fct_lump, p = 0.01, other_level = "Outro")) 

# dados_lump %>% group_by(Neighborhood) %>% count()

training <- dados_lump[idx,]
test <- dados_lump[-idx,]



# Linear - novamente ------------------------------------------------------


linear <- lm(Sale_Price ~ ., data = training)

y_hat_linear <- predict(linear, newdata = test)

resultados[resultados$modelo == "Linear", 2] <- 
  sqrt(mean((y_hat_linear - test$Sale_Price)^2))

# Bagging -----------------------------------------------------------------
library(randomForest)

set.seed(1)
bag <- randomForest(Sale_Price ~ ., mtry = ncol(training) - 1, 
                   data = training)

y_hat_bag <- predict(bag, newdata = test)


resultados[resultados$modelo == "Bagging", 2] <- 
  sqrt(mean((y_hat_bag - test$Sale_Price)^2))

varImpPlot(bag)

library(vip)

vip(bag)

# Random forest -----------------------------------------------------------
set.seed(1)
rf <- randomForest(Sale_Price ~ ., data = training)

y_hat_rf <- predict(rf, newdata = test)

resultados[resultados$modelo == "Random Forest", 2] <- 
  sqrt(mean((y_hat_rf - test$Sale_Price)^2))

varImpPlot(rf)
vip(rf)

plot(rf$mse)

data.frame(arvore = 1:length(rf$mse), mse = rf$mse) %>%
  ggplot(aes(arvore, mse)) +
  geom_line() +
  labs(x = "Número de árvores", y = "MSE (OOB)") +
  theme_bw()


# Boosting ----------------------------------------------------------------
library(gbm)

set.seed(1)
boost <- gbm(Sale_Price ~ ., data = training,
             distribution = "gaussian",
             n.trees = 5000,
             interaction.depth = 1, # maximum depth of each tree
             shrinkage = 0.001) 

y_hat_boost <- predict(boost, newdata = test, n.trees = 5000)


resultados[resultados$modelo == "Boosting", 2] <- 
  sqrt(mean((y_hat_boost - test$Sale_Price)^2))



# Comparativo -------------------------------------------------------------


resultados %>% 
  arrange(rmse)

resultados %>% 
  ggplot(aes(x = modelo, y = rmse, fill = modelo)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Modelo", y = "RMSE") +
  theme_bw()
