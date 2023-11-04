# Problema de regressao: prever o valor medio das casas de Boston

# Carregar pacotes --------------------------------------------------------
library(MASS)
library(patchwork)
library(tidyverse)
library(gbm)
library(xgboost)
library(rsample)
library(pdp)


# Carregar dados ----------------------------------------------------------

# carrega conjunto de dados Boston: objetivo e' prever medv
data(Boston)

# Criacao de multiplos graficos ------------------------------------------
# pacote patchwork

?Boston #para ver descricao dos dados

# grafico de dispersao lstat e medv
g1 <- ggplot(Boston, aes(lstat, medv)) + 
  geom_point(color = "blue", size = 1.2) + 
  geom_smooth(color = "red", se = FALSE) + 
  theme_bw()

# grafico de dispersao rm e medv
g2 <- ggplot(Boston, aes(rm, medv)) + 
  geom_point(color = "blue", size = 1.2) + 
  geom_smooth(color = "red", se = FALSE) + 
  theme_bw()

# grafico de dispersao dis e medv
g3 <- ggplot(Boston, aes(dis, medv)) + 
  geom_point(color = "blue", size = 1.2) + 
  geom_smooth(color = "red", se = FALSE) + 
  theme_bw()

# veja as poossibilidades abaixo

g1 + g2 + g3

g1 / g2 / g3

g1 + g2 / g3

(g1 + g2) / g3



# Boosting ----------------------------------------------------------------

# definicao de semente aleatoria
set.seed(123)

# como faziamos antes
id <- sample(nrow(Boston), 0.8*nrow(Boston))
Boston[id,]

# define separacao dos dados em treino e teste
split <- initial_split(Boston, prop = .8)

# obtem conjunto de treino definido acima
treinamento <- training(split)

# obtem conjunto de teste definido acima
teste <- testing(split)

# ajusta o modelo boosting
(fit_bst <- gbm(medv ~ ., # formula
                distribution = "gaussian", # para indicar problema de regressao
                n.trees = 5000, # numero de arvores a serem construidas
                interaction.depth = 1, # profundidade maxima de cada arvore
                shrinkage = 0.1, # taxa de aprendizagem
                data = treinamento)) #dados

summary(fit_bst)


# Validacao cruzada -------------------------------------------------------
boston_cv <- gbm(medv ~ ., cv.folds = 5,
                 n.trees = 5000, interaction.depth = 1, 
                 shrinkage = 0.1, data = treinamento)

# obtem o numero otimo de arvores para o boosting
(ntrees <- gbm.perf(boston_cv, method = "cv"))

# equivalente ao acima, porem a funcao fornece mais informacoes
which.min(boston_cv$cv.error)


# Predicao ---------------------------------------------------------------

# predicao para os dados de teste
predito_bst <- predict(fit_bst, newdata = teste, 
                       n.trees = ntrees)

(erro_bst <- Metrics::mse(teste$medv, predito_bst))

# Partial dependence plot -------------------------------------------------
# vamos implementar manualmente o pdp para a variavel lstat

# precisamos dos valores minimo e maximo da variavel
range(treinamento$lstat)

# criar sequencia de valores possiveis de lstat
inter_lstat  <- seq(1, 40, 0.1)

# vamos salvar a media das previsoes de medv para cada valor de lstat
# no vetor abaixo
efeito_lstat <- numeric(length(inter_lstat))

# variavel auxiliar, pois vamos alterar os valores de lstat a cada iteracao
treinamento_pdp <- treinamento

# definicao de uma barra de progresso para acompanharmos a execucao do processo
pb <- txtProgressBar(min = 1, max = length(inter_lstat), 
                     style = 3, width = 50)

# para cada valor possivel de lstat:
for(i in 1:length(inter_lstat)){
  
  #altera o valor de lstat para todos dados de treinamento
  treinamento_pdp$lstat <- inter_lstat[i] 
  
  #calcula o valor medio das previsoes
  efeito_lstat[i] <- mean(predict(fit_bst, treinamento_pdp, 
                                  n.trees = 5000))
  
  # avanca a barra de progresso
  setTxtProgressBar(pb, i)
  
}

# encerra a barra de progresso
close(pb)

plot(inter_lstat, efeito_lstat, type = "l", col = "blue", lwd = 2)


# Partial dependence plot ja implementado ---------------------------------

# para o boosting, o pdp ja esta implementado no plot
# veja o pdp para a variavel lstat
plot(fit_bst, i = "lstat", lwd = 2)
plot(fit_bst, i = "rm", lwd = 2)

# Partial dependence plot para duas variáveis -----------------------------

plot(fit_bst, i = c("lstat", "rm"))
plot(fit_bst, i = c("lstat", "rm"), col.regions = viridis::inferno)


# Partial dependence plot com o pacote pdp --------------------------------

pdp::partial(fit_bst, pred.var = "lstat", n.trees = 5000) %>% 
  ggplot(aes(lstat, yhat)) + 
  geom_line(color = "blue", size = 1) + 
  geom_smooth(color = "red", size = 1.2, se = FALSE) + #adiciona uma linha de  suavizacao
  theme_bw()

pdp::partial(fit_bst, pred.var = "lstat", n.trees = 5000) %>% 
  ggplot(aes(lstat, yhat)) + 
  geom_line(color = "blue", size = 1) + 
  geom_smooth(color = "red", size = 1.2, se = FALSE) + #adiciona uma linha de  suavizacao
  geom_rug(aes(lstat, medv),data = Boston, sides = "b", alpha = 0.5) + #adiciona ticks no eixo x
  ylim(15, 32)+
  theme_bw()

# XGBoost -----------------------------------------------------------------

d_tr <- xgb.DMatrix(label = treinamento$medv, 
                    data = as.matrix(dplyr::select(treinamento, -medv)))

boston_xgb <- xgboost(data = d_tr, nrounds = 500, max_depth = 4, 
                      eta = 0.1, nthread = 3, verbose = FALSE,
                      objective="reg:squarederror")

importancia <- xgb.importance(model = boston_xgb)

xgb.plot.importance(importancia, rel_to_first = TRUE, 
                    xlab = "Relative Importance")


# erro de teste -----------------------------------------------------------

d_test <- xgb.DMatrix(label = teste$medv, 
                      data = as.matrix(select(teste, -medv)))

pred_xgb <- predict(boston_xgb, d_test)

# calcula manualmente o MSE
mean((pred_xgb - teste$medv)^2)  

# funcao que calcula o MSE
Metrics::mse(pred_xgb, teste$medv)
