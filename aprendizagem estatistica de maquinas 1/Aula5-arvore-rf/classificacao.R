library(rpart.plot)
library(randomForest)
library(modeldata)
library(pROC)
library(ranger)
library(rsample)
library(tidyverse)

# Churn -------------------------------------------------------------------

data(mlc_churn) 

mlc_churn$churn <- factor(mlc_churn$churn, levels = c("no", "yes"))

set.seed(105) 

splits <- initial_split(mlc_churn, prop = .9, strata = "churn")

treino <- training(splits); teste <- testing(splits)

(rf <- randomForest(churn ~ ., data = treino))

# Erro de classificação OOB

tibble(arvore = 1:nrow(rf$err.rate), 
       oob = rf$err.rate[,1]) %>% 
  ggplot(aes(arvore, oob)) + 
  geom_line(color = "blue", size = 1.2) + 
  labs(x = "Número de Árvores", y = "Erro de Classificação (OOB)") + 
  theme_bw()

# Diferentes valores de mtry

rf    <- randomForest(churn ~ ., data = treino)
rf_8  <- randomForest(churn ~ ., mtry =  8, data = treino)
rf_11 <- randomForest(churn ~ ., mtry = 11, data = treino)
rf_15 <- randomForest(churn ~ ., mtry = 15, data = treino)
rf_19 <- randomForest(churn ~ ., mtry = 19, data = treino)

resultados <- tibble(mtry = 4, arvore = 1:nrow(rf$err.rate), 
                     oob = rf$err.rate[,1]) %>% 
  bind_rows(tibble(mtry = 8, arvore = 1:nrow(rf_8$err.rate), 
                   oob = rf_8$err.rate[,1])) %>%
  bind_rows(tibble(mtry = 11, arvore = 1:nrow(rf_11$err.rate), 
                   oob = rf_11$err.rate[,1])) %>% 
  bind_rows(tibble(mtry = 15, arvore = 1:nrow(rf_15$err.rate), 
                   oob = rf_15$err.rate[,1])) %>% 
  bind_rows(tibble(mtry = 19, arvore = 1:nrow(rf_19$err.rate), 
                   oob = rf_19$err.rate[,1]))

# Resultados

resultados %>% 
  mutate(mtry = factor(mtry)) %>% 
  ggplot(aes(arvore, oob, group = mtry, color = mtry)) + 
  geom_line( size = 1.2) + 
  labs(x = "Número de Árvores", y = "Erro de Classificação (OOB)") + 
  theme_bw()

# Variando número de árvores

rf <- randomForest(churn ~ ., ntree = 2500, data = treino)

tibble(arvore = 1:nrow(rf$err.rate), oob = rf$err.rate[,1]) %>% 
  ggplot(aes(arvore, oob)) + 
  geom_line(color = "blue", size = 1.2) + 
  labs(x = "Número de Árvores", y = "Erro de Classificação (OOB)") + 
  theme_bw()


# Importância das variáveis

varImpPlot(rf, pch = 19)

vip::vip(rf)

# Previsão 

head(predict(rf, teste, type = "prob"))

head(predict(rf, teste))


# Matriz de confusão

pred_rf <- predict(rf, teste)

table(observado = teste$churn, predito = pred_rf)


# Medidas de desempenho

desempenho <- roc(response = teste$churn, 
                  predictor = predict(rf, teste, type = "prob")[,2])

coords(desempenho, ret = c("1-accuracy", "sensitivity",               
                           "specificity", "ppv", "npv"))

coords(desempenho, .5, ret = c("1-accuracy", "sensitivity",               
                               "specificity", "ppv", "npv"))


# Modelo arvore - para comparacao -----------------------------------------

arvore <- rpart(churn ~ ., data = treino)

pred_arvore <- predict(arvore, teste)

desempenho_arvore <- roc(teste$churn, pred_arvore[,2])

roc(teste$churn, predict(rf, teste, type = "prob")[,2], 
    plot = TRUE, print.auc=TRUE, col = "blue", legacy.axes = TRUE)
roc(teste$churn, pred_arvore[,2], plot = TRUE, add = TRUE, 
    col = "red", print.auc=TRUE, print.auc.y = .4)
legend("bottomright",
       legend = c("Floresta Aleatória","Árvore"),
       col = c("blue", "red"), lwd = 2)


# Utilizando a biblioteca ranger

ranger(churn ~ ., data = treino)
