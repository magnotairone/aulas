library(ISLR2) # dados do livro texto
library(glmnet) # para ajustar o LASSO
library(ggplot2) # confeccao de graficos
library(keras) # para ajustar rede neural
# guia de instalacao keras: https://web.stanford.edu/~hastie/ISLR2/keras-instructions.html

Hitters # dados de jogadores da Major League Baseball

dim(Hitters) # dimensao dos dados

Gitters <- na.omit(Hitters) # remover dados faltantes

(n <- nrow(Gitters)) # numero de jogadores considerados

# Separacao dos dados~ ----------------------------------------------------

set.seed(13)
(n_teste <- trunc(n/3))
ids_teste <- sample(1:n, n_teste)

# Regressao Linear --------------------------------------------------------

fit_lm <- lm(Salary ~ ., data = Gitters[-ids_teste, ])

# predicao
pred_lm <- predict(fit_lm , Gitters[ids_teste , ])

# calculo do erro absoluto medio
(erro_lm <- mean(abs(pred_lm -
                       Gitters$Salary[ids_teste])))

# dataframe com resultados
resultados <- data.frame(modelo = "Linear", erro = erro_lm)


# LASSO -------------------------------------------------------------------
x <- scale(model.matrix(Salary ~. - 1, data = Gitters))
y <- Gitters$Salary

set.seed(1)
# validacao cruzada para escolher valor da penalidade lambda
cv_fit <- cv.glmnet(x[-ids_teste , ], y[-ids_teste],
                   type.measure = "mae")

# reajuste do modelo com o valor de lambda escolhido (lambda.min)
lasso <- glmnet(x[-ids_teste , ], y[-ids_teste],
                lambda = cv_fit$lambda.min)

# predicao
pred_lasso <- predict(lasso, x[ids_teste , ])

# calculo do erro absoluto medio
(erro_lasso <- mean(abs(y[ids_teste] - pred_lasso)))

(resultados <- rbind(resultados,
                    data.frame(modelo = "LASSO",
                               erro = erro_lasso)))



# Rede Neural -------------------------------------------------------------

# primeiro passo: definir a estrutura que descreve a rede neural
# vamos considerar uma rede com uma unica camada oculta
# contendo 50 unidades e funcao de ativacao ReLU
# dropout layer de 40%
# a ultima camada tem somente uma unidade
# sem funcao de ativacao

modelo_rn <- keras_model_sequential()

modelo_rn <- layer_dense(modelo_rn,
                         units = 50, # numero de unidades
                         activation = "relu", # funcao de ativacao
                         input_shape = ncol(x))

modelo_rn <- layer_dropout(modelo_rn, rate = 0.4)

modelo_rn <- layer_dense(modelo_rn, units = 1)

modelo_rn

# segundo passo: especificacoes que controlam o algoritmo de estimacao
modelo_rn <- compile(modelo_rn, 
                     loss = "mse",
                     optimizer = optimizer_rmsprop(),
                     metrics = list("mean_absolute_error"))
# a funcao compile comunica as especificacoes 
# para a instancia python correspondente que foi criada

# terceiro passo: ajustar o modelo (estimar os parametros)
history <- fit(modelo_rn,
               x[-ids_teste,], # preditoras de treino
               y[-ids_teste],  # resposta de treino
               batch_size = 32, # observacoes escolhidas aleatoriamente
               # em cada passo do SGD
               epochs = 1500, # uma epoca e' numero de passos do SGD
               # para processar todos dados de treino
               # nesse caso, cada epoca tem
               # length(y[-ids_teste])/32 passos SGD
               # dados de validacao para avaliar o progresso do modelo
               validation_data = list(x[ids_teste,],
                                      y[ids_teste]))

# o grafico abaixo mostra o MAE
# para os dados de treino e teste
plot(history)

# predicao
pred_rn <- predict(modelo_rn, x[ids_teste, ])

# calculo do erro absoluto medio
(erro_rn <- mean(abs (y[ids_teste] - pred_rn)))
# atencao: os resultados variam um pouco pois
# os calculos sao feitos do SGD no python

(resultados <- rbind(resultados,
                     data.frame(modelo = "Rede Neural",
                                erro = erro_rn)))
