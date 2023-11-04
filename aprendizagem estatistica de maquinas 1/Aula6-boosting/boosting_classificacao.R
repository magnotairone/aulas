# Problema de classificacao: prever o churn dos usuarios

# Carregar pacotes --------------------------------------------------------
library(skimr)
library(patchwork)
library(tidyverse)
library(gbm)
library(xgboost)
library(rsample)
library(pdp)
library(modeldata)
library(yardstick)
library(randomForest)

# Carregar e processar dados -----------------------------------------------
# carrega conjunto de dados telco: objetivo e' prever Churn
dados <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  dplyr::select(-customerID)

# obtem a dimensao dos dados (numero de linhas e colunas)
dim(dados)

# remove NA da base
dados <- na.omit(dados)

dim(dados)

# verifica a estrutura dos dados
glimpse(dados)

# transforma o que for character em factor
dados <- dados %>% 
  mutate_if(is.character, ~as.factor(.x))


# treinamento e teste -----------------------------------------------------
set.seed(123)

splits <- initial_split(dados, prop = .8, strata = Churn)

dados_tr   <- training(splits)
dados_test <- testing(splits)

# floresta aleatória ------------------------------------------------------
# ajuste da floresta aleatoria
rf <- randomForest(Churn ~ ., dados_tr)

# predicao das classes para o conjunto de teste
predito_rf <- predict(rf, dados_test, type = "class")

# calculo do erro de predicao
mean(predito_rf != dados_test$Churn)

# probabilidades de churn para cada observacao do conjunto de teste
prob_rf <- predict(rf, dados_test, type = "prob")

# tibble para armazenar a probabilidade predita
desempenho <- tibble(prob = prob_rf[,2], 
                     classes = dados_test$Churn, 
                     metodo = "random forest")


# Boosting ----------------------------------------------------------------
# preparacao dos dados para o boosting (tem que ser numerico)
dados_tr$Churn <- ifelse(dados_tr$Churn == "Yes", 1, 0)
dados_test$Churn <- ifelse(dados_test$Churn == "Yes", 1, 0)

# ajuste boosting
(fit_bst <- gbm(Churn ~ ., # formula
                distribution = "bernoulli", # para indicar problema de classificacao
                n.trees = 1000, # numero de arvores
                interaction.depth = 4, # profundidade maxima da arvore
                shrinkage = 0.05, # taxa de aprendizagem
                data = dados_tr)) #dados considerados

# obtem a importancia das variaveis
summary(fit_bst)

# ajuste boosting com valicadao cruzada
fit_cv <- gbm(Churn ~ ., data = dados_tr, cv.folds = 5,
              n.trees = 1000, interaction.depth = 4,
              distribution = "bernoulli",
              shrinkage = 0.05)

# obtem o numero de arvores otimo
(ntree <- gbm.perf(fit_cv, method = "cv"))

# obtem as probabilidade preditas para os dados de teste
prob_gbm <- predict(fit_cv, dados_test, n.trees = ntree, 
                    type = "response")

# faz a previsao do churn se prob >= 0.5
predito <- ifelse(prob_gbm >= .5, 1, 0)
# observacao, pode-se fazer uma escolha de corte de acordo com alguma metrica

# calcula erro de predicao
mean(dados_test$Churn != predito) 

# adiciona no tibble desempenho as probabilidades do metodo gbm
desempenho <- desempenho %>% 
  bind_rows(tibble(prob = prob_gbm, 
                   classes = ifelse(dados_test$Churn == 1, "Yes", "No"), 
                   metodo = "gbm"))

# logística ---------------------------------------------------------------
# ajuste regressao logistica
fit_log <- glm(Churn ~ ., data = dados_tr, family = "binomial")

# obtem as probabilidades de chunr para os dados de teste
prob_log <- predict(fit_log, newdata = dados_test, type = "response")

# adiciona no tibble desempenho as probabilidades da regressao logistica
desempenho <- desempenho %>% 
  bind_rows(tibble(prob = prob_log, 
                   classes = ifelse(dados_test$Churn == 1, "Yes", "No"), 
                   metodo = "logística"))

# Area sob a curva ROC ------------------------------------------
# comparativo dos metodos ajustados
desempenho %>% 
  mutate(classes = factor(classes)) %>% 
  group_by(metodo) %>% 
  roc_auc(classes, prob, event_level = "second") %>% 
  arrange(desc(.estimate))


