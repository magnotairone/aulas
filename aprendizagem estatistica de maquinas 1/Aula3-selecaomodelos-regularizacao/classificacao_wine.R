# Wine Quality Data Set - White

# O conjunto de dados pode ser obtido a partir do link [Wine Quality Data Set](https://archive.ics.uci.edu/ml/datasets/Wine+Quality) . Os dados são relativos a diversas características de vinho e a sua qualidade.

# The two datasets are related to red and white variants of the Portuguese “Vinho Verde” wine. 
# For more details, consult: [Web Link] or the reference [Cortez et al., 2009].

# Preditoras

# -- fixed acidity
# -- volatile acidity
# -- citric acid
# -- residual sugar
# -- chlorides
# -- free sulfur dioxide
# -- total sulfur dioxide
# -- density
# -- pH
# -- sulphates
# -- alcohol


# **Resposta**

# -- quality (score between 0 and 10)


library(tidyverse)
library(skimr)
library(GGally)
library(pROC)
library(class)

dados_raw <- read_csv2("winequality-white.csv")


# criar classes -----------------------------------------------------------

dados <- dados_raw %>% 
          mutate(quality = factor(ifelse(quality >= 6, "Alto", "Baixo"), 
                                  levels = c("Baixo", "Alto")))



# análise descritiva ------------------------------------------------------

skim(dados)

dados %>%
    sample_frac(0.03) %>% 
    ggpairs(aes(colour = quality)) 


dados %>%
  ggpairs(colour = quality)


# regressão logística ----------------------------------------------------

fit_logistica <- glm(quality ~ ., family = "binomial", data = dados)

summary(fit_logistica)



# medidas de desempenho ---------------------------------------------------

cortes <- seq(0.01, 0.99, 0.01)

resultados <- tibble(cortes = cortes, 
                     sensibilidade = NA, 
                     especificidade = NA, 
                     vpp = NA, 
                     vpn = NA)

probabilidade <- predict(fit_logistica, dados, type = "response")


for(i in 1:nrow(resultados)){

  resultados$sensibilidade[i] <- sum(probabilidade >= resultados$cortes[i] & dados$quality == "Alto") / sum(dados$quality == "Alto")
  
  resultados$especificidade[i] <- sum(probabilidade < resultados$cortes[i] & dados$quality == "Baixo") / sum(dados$quality == "Baixo")
  
  resultados$vpp[i] <- sum(probabilidade >= resultados$cortes[i] & dados$quality == "Alto") / sum(probabilidade >= resultados$cortes[i])
  
  resultados$vpn[i] <- sum(probabilidade < resultados$cortes[i] & dados$quality == "Baixo") / sum(probabilidade  < resultados$cortes[i])
  
}

resultados %>% 
  head()
    


# curva ROC ---------------------------------------------------------------

resultados %>% 
  mutate(falso_negativo = 1 - especificidade) %>% 
  ggplot(aes(falso_negativo, sensibilidade)) + 
    geom_step(direction = "vh", size = 1, color = "blue") + 
    geom_abline(intercept = 0, slope = 1, col = "grey", linetype = "dashed", size = 2) + 
    ylab("Sensibilidade") + xlab("1 - especificidade")


# medidas de desempenho ---------------------------------------------------

resultados %>% 
  pivot_longer(-cortes, values_to = "proporcao") %>% 
  ggplot(aes(cortes, proporcao, group = name, color = name)) + 
    geom_line(size = 1.2)




# treinamento e teste -----------------------------------------------------

set.seed(123)

treino <- sample(nrow(dados), .80*nrow(dados), replace = FALSE)

dados_tr   <- dados[ treino,]
dados_test <- dados[-treino,]


# modelo com dados de treinamento

fit_tr <- glm(quality ~ ., family = "binomial", data = dados_tr)

# probabilidades

prob_test <- predict(fit_tr, dados_test, type = "response")

# organizando em um tibble

estimativas_test <- tibble(observado = dados_test$quality, 
                           probabilidade = prob_test)


# curva ROC

roc_fit <- estimativas_test %>% 
            roc(response = observado, predictor = probabilidade)

roc_fit

plot(roc_fit)

# comparando os resultados para diferentes cortes 

coords(roc_fit, x = c(.60, .70), ret = "all", transpose = FALSE) %>% 
  mutate(metodo = "logistica")

?coords


# Modelo KNN --------------------------------------------------------------

# Segmentando a base em treino e teste

X_tr   <- dados_tr %>% 
            select(-quality)

X_test <- dados_test %>% 
            select(-quality)

y_tr   <- dados_tr$quality

y_test <- dados_test$quality


# ajustando o modelo

fit_knn <- knn(train = X_tr, 
               test = X_test, 
               cl = y_tr, 
               k = 150, 
               prob = TRUE)

# obtendo as probabilidades

prob_knn <- attr(fit_knn, "prob")


# criando uma base de dados com os resultados observados e as probabilidades associadas

estimativas_knn <- tibble(observado = dados_test$quality, 
                          probabilidade = prob_knn)


# criando a curva ROC

roc_knn <- estimativas_knn %>% 
             roc(response = observado, predictor = probabilidade)

roc_knn

plot(roc_knn, print.auc = TRUE, col = "red")




# Modelo KNN com medidas padronizadas -------------------------------------

# padronizando os dados

X_tr_pad   <- scale(X_tr)

X_test_pad <- scale(X_test, 
                    center = attr(X_tr_pad, "scaled:center"), 
                    scale = attr(X_tr_pad, "scaled:scale"))
 
# ajustando o modelo

fit_knnPad <- knn(train = X_tr_pad, 
                  test = X_test_pad, 
                  cl = y_tr, 
                  k = 150, 
                  prob = TRUE)

# obtendo as probabilidades

prob_knnPad <- attr(fit_knnPad, "prob")


# criando uma base de dados com os resultados observados e as probabilidades associadas

estimativas_knnPad <- tibble(observado = dados_test$quality, 
                             probabilidade = prob_knnPad)

# criando a curva ROC

roc_knnPad <- estimativas_knnPad %>% 
                    roc(response = observado, predictor = probabilidade)

roc_knnPad

plot(roc_knnPad, print.auc = TRUE, col = "purple")



## Comparação dos modelos

plot(roc_fit, print.auc = TRUE, print.auc.x = .5, print.auc.y = .4)
plot(roc_knn, add = TRUE, print.auc = TRUE, print.auc.x = .5, print.auc.y = .3, col = "red")
plot(roc_knnPad, add = TRUE, print.auc = TRUE, print.auc.x = .5, print.auc.y = .2, col = "purple")
legend("bottomright", c("Logística", "KNN", "KNN pad"), 
       lty = 1, lwd = 2, col = c("black", "red", "purple"))



plot(roc_fit, print.auc = TRUE, print.auc.x = .5, print.auc.y = .4, legacy.axes = TRUE)
plot(roc_knn, add = TRUE, print.auc = TRUE, print.auc.x = .5, print.auc.y = .3, col = "red", legacy.axes = TRUE)
plot(roc_knnPad, add = TRUE, print.auc = TRUE, print.auc.x = .5, print.auc.y = .2, col = "purple", legacy.axes = TRUE)
legend("bottomright", c("Logística", "KNN", "KNN pad"), 
       lty = 1, lwd = 2, col = c("black", "red", "purple"))


