# lab4 - agenda:
# best subset, forward stepwise, backward stepwise - selecao regressiva
# ridge, lasso, elastic-net
# 

# bibliotecas usadas na analise ----------------------------------------

library(leaps) # tem as funcoes de selecao de variaveis
library(glmnet) # tem as funcoes para lasso, ridge e elasticnet
library(ISLR) # tem o conjunto de dados Credit
library(ggplot2) # funcoes para criacao de graficos
library(plotmo) # funcoes para criar graficos com objetos do glmnet

# carregar e visualizar dados ------------------------------------------

data(Credit) # carrega os dados

?Credit

dim(Credit) # numero de linhas e colunas

View(Credit) # visualizar como planilha

# grafico de dispersao Income e Balance
ggplot(Credit, aes(x = Income, y = Balance))+
  geom_point(alpha = 0.5)

# grafico de dispersao Income e Balance,
# colorindo pela coluna Married
ggplot(Credit, aes(x = Income, y = Balance, 
                   color = Married))+
  geom_point()

# TAREFA: completar a análise descritiva para compreender
#         melhor os dados

# separacao dos dados em treino e teste -----------------------------------
set.seed(321)

ids <- sample(nrow(Credit),
              size = .75*nrow(Credit),
              replace = FALSE) # indice treinamento


# criacao do dataframe para armazenar resultados

resultados <- data.frame(modelo = c("best", "forward", 
                                    "backward", "ridge", 
                                    "lasso", "elastic-net",
                                    "linear"),
                         erro_dentro = NA,
                         erro_fora = NA)

resultados


# selecao de variaveis --------------------------------------------------

# ---- melhor subconjunto (best subset) -----
best <- regsubsets(Balance ~ ., # forumula
                   data = Credit[ids,-1], # dados
                   nvmax = ncol(Credit)-2, # numero max de variaveis
                   method = "exhaustive") # metodo

best_sum <- summary(best)

ggplot(data.frame(bic = best_sum$bic, n = 1:10), 
       aes(n, bic))+
  geom_point()+
  geom_vline(xintercept = which.min(best_sum$bic), 
             col = "red")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "BIC - best subset", 
       x = "Número de variáveis",
       y = "BIC")+
  theme_classic()

num_coef <- which.min(best_sum$bic) # numero de coeficientes escolhidos
best_coef <- coef(best, num_coef) # valores dos coeficientes estimados

# calculo do erro estimado dentro e fora da amostra
dados_matriz <- model.matrix(Balance ~ ., 
                             data = Credit[,-1])

best_erro_dentro <- mean((dados_matriz[ids, names(best_coef)] %*% 
                            best_coef - Credit$Balance[ids])^2)

best_erro_fora <- mean((dados_matriz[-ids, names(best_coef)] %*% 
                          best_coef - Credit$Balance[-ids])^2)

resultados[which(resultados$modelo=="best"), 2:3] = 
  c(best_erro_dentro, best_erro_fora)

resultados

# ---- selecao progressiva (forward stepwise) -----

fw <- regsubsets(Balance ~ ., # forumula
                 data = Credit[ids,-1], # dados
                 nvmax = ncol(Credit)-2, # numero max de variaveis
                 method = "forward") # metodo

fw_sum <- summary(fw)

ggplot(data.frame(bic = fw_sum$bic, n = 1:10), 
       aes(n, bic))+
  geom_point()+
  geom_vline(xintercept = which.min(fw_sum$bic), 
             col="red")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "BIC - forward", 
       x = "Número de variáveis",
       y = "BIC")+
  theme_classic()

num_coef <- which.min(fw_sum$bic) # numero de coeficientes escolhidos
fw_coef <- coef(fw, num_coef) # valores dos coeficientes estimados

# calculo do erro estimado dentro e fora da amostra
fw_erro_dentro <- mean((dados_matriz[ids, names(fw_coef)] %*% 
                          fw_coef - Credit$Balance[ids])^2)
fw_erro_fora <- mean((dados_matriz[-ids, names(fw_coef)] %*% 
                        fw_coef - Credit$Balance[-ids])^2)

resultados[which(resultados$modelo=="forward"), 2:3] = 
  c(fw_erro_dentro, fw_erro_fora)

resultados

# ---- selecao regressiva (backward stepwise) -----
bw <- regsubsets(Balance ~ ., 
                 data = Credit[ids,-1], 
                 nvmax = ncol(Credit)-2,
                 method = "backward")

bw_sum <- summary(bw)

ggplot(data.frame(bic = bw_sum$bic, n = 1:10), 
       aes(n, bic))+
  geom_point()+
  geom_vline(xintercept = which.min(bw_sum$bic), 
             col="red")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "BIC - backward", 
       x = "Número de variáveis",
       y = "BIC")+
  theme_classic()

num_coef <- which.min(bw_sum$bic) # numero de coeficientes escolhidos
bw_coef <- coef(bw, num_coef)

# calculo do erro estimado dentro e fora da amostra
bw_erro_dentro <- mean((dados_matriz[ids, names(bw_coef)] %*% 
                          bw_coef - Credit$Balance[ids])^2)
bw_erro_fora <- mean((dados_matriz[-ids, names(bw_coef)] %*% 
                        bw_coef - Credit$Balance[-ids])^2)

resultados[which(resultados$modelo=="backward"), 2:3] = 
  c(bw_erro_dentro, bw_erro_fora)

resultados


# regularizacao -----------------------------------------------------------

# preparacao de variaveis para os modelos usando glmnet

X <- model.matrix(Balance ~ .,
                  data = Credit[,-1])[,-1] # X deve ser uma matrix

class(X)

View(X)

y <- Credit$Balance

# ridge -------------------------------------------------------------------

ridge <- glmnet(X[ids,], y[ids], alpha = 0, 
                nlambda = 1000)

plot_glmnet(ridge, lwd = 2, xvar = "lambda")

ridge$beta

ridge$lambda

# ridge - validação cruzada
set.seed(1)
cv_ridge <- cv.glmnet(X[ids,], y[ids], alpha = 0)

plot(cv_ridge, cex.lab = 1.3)

cv_ridge$lambda
cv_ridge$lambda.min
cv_ridge$lambda.1se
# TAREFA: testar valores de lambda menores
# Dica: veja como informar valores de lambda: ?cv.glmnet

# retreina o modelo considerando o lambda.1se
ridge <- glmnet(X[ids,], y[ids], alpha = 0, 
                lambda = cv_ridge$lambda.1se)

y_ridge_dentro <- predict(ridge, newx = X[ids,],
                          s = cv_ridge$lambda.1se) # valor predito dentro da amostra

y_ridge_fora <- predict(ridge, newx = X[-ids,],
                        s = cv_ridge$lambda.1se) # valor predito fora da amostra

# calcula os erros
ridge_erro_dentro <- mean((y_ridge_dentro - y[ids])^2)

ridge_erro_fora <- mean((y_ridge_fora - y[-ids])^2)

resultados[which(resultados$modelo=="ridge"), 2:3] = 
  c(ridge_erro_dentro, ridge_erro_fora)

resultados

# LASSO -------------------------------------------------------------------

lasso <- glmnet(X[ids,], y[ids], alpha = 1, 
                nlambda = 1000)

plot_glmnet(lasso, lwd = 2, xvar = "lambda")

# LASSO - validação cruzada
set.seed(1)
cv_lasso <- cv.glmnet(X[ids,], y[ids], alpha = 1)

plot(cv_lasso, cex.lab = 1.3)

# retreina o modelo considerando o lambda.1se
lasso <- glmnet(X[ids,], y[ids], alpha = 1, 
                lambda = cv_lasso$lambda.1se)

y_lasso_dentro <- predict(lasso, newx = X[ids,],
                          s = cv_lasso$lambda.1se) # valor predito dentro da amostra

y_lasso_fora <- predict(lasso, newx = X[-ids,],
                        s = cv_lasso$lambda.1se) # valor predito fora da amostra

# calcula erros
lasso_erro_dentro <- mean((y_lasso_dentro - y[ids])^2)

lasso_erro_fora <- mean((y_lasso_fora - y[-ids])^2)

resultados[which(resultados$modelo=="lasso"), 2:3] = 
  c(lasso_erro_dentro, lasso_erro_fora)

resultados

# elastic-net -------------------------------------------------------------
elastic <- glmnet(X[ids,], y[ids], alpha = 0.75, 
                  nlambda = 500)

plot_glmnet(elastic, lwd = 2, xvar = "lambda")

set.seed(1)
cv_elastic <- cv.glmnet(X[ids,], y[ids], alpha = 0.75)

plot(cv_elastic, cex.lab = 1.3)

# retreina o modelo considerando o lambda.1se
elastic <- glmnet(X[ids,], y[ids], alpha = 0.75, 
                  lambda = cv_elastic$lambda.1se)

y_elastic_dentro <- predict(elastic, newx = X[ids,],
                            s = cv_elastic$lambda.1se) # valor predito dentro da amostra

y_elastic_fora <- predict(elastic, newx = X[-ids,],
                          s = cv_elastic$lambda.1se) # valor predito fora da amostra

# calcula erros
elastic_erro_dentro <- mean((y_elastic_dentro - y[ids])^2)

elastic_erro_fora <- mean((y_elastic_fora - y[-ids])^2)

resultados[which(resultados$modelo=="elastic-net"), 2:3] = 
  c(elastic_erro_dentro, elastic_erro_fora)
resultados


# regressao linear --------------------------------------------------------

linear <- lm(Balance ~., Credit[ids,-1])

y_linear_dentro <- predict(linear, Credit[ids,-1])
y_linear_fora <- predict(linear, Credit[-ids,-1])

linear_erro_dentro <- mean((y_linear_dentro - y[ids])^2)
linear_erro_fora <- mean((y_linear_fora - y[-ids])^2)

resultados[which(resultados$modelo=="linear"), 2:3] = 
  c(linear_erro_dentro, linear_erro_fora)
resultados

# comparativo final -------------------------------------------------------

resultados[order(resultados$erro_fora),]

# grafico de barras com o resultado
ggplot(resultados,
       aes(x = modelo, 
           y = erro_fora))+
  geom_col()+
  labs(x = "Modelo",
       y = "Erro fora da amostra (estimado)")+
  theme_classic()

# grafico de barras (modelos ordenados pelo erro fora)
ggplot(resultados,
       aes(x = reorder(modelo, erro_fora), 
           y = erro_fora,
           fill = modelo))+
  geom_col(show.legend = FALSE)+
  labs(x = "Modelo",
       y = "Erro fora da amostra (estimado)")+
  theme_classic()

# grafico de barras (modelos ordenados pelo erro fora)
# com zoom no eixo y
ggplot(resultados,
       aes(x = reorder(modelo, erro_fora), 
           y = erro_fora,
           fill = modelo))+
  geom_col(show.legend = FALSE)+
  coord_cartesian(ylim=c(10000, 13000)) +
  labs(x = "Modelo",
       y = "Erro fora da amostra (estimado)")+
  theme_classic()