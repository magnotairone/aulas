# Lab5 - Modelos para classificação:
# regressão logística
# naive bayes
# knn

# bibliotecas usadas na analise
library(ggplot2) # criacao de graficos
library(e1071)  # funcoes de modelos de aprendizagem, inclusive de naive bayes
library(class) # funcao para o knn

# leitura dos dados
digitos <- read.table("features.train.txt")

# numero de linhas e colunas
dim(digitos)

# cabecalho dos dados
head(digitos)

# nomes das colunas
colnames(digitos)
colnames(digitos) <- c("digito", "intensidade", "simetria")

# visualizacao dos dados --------------------------------------------------

# visualizacao da relacao entre simetria e intensidade
ggplot(digitos, aes(x = intensidade,
                    y = simetria)) +
  geom_point(alpha = 0.2) +
  labs(x = "Intensidade",
       y = "Simetria") +
  theme_classic()


# visualizacao da relacao entre simetria e intensidade
ggplot(digitos, aes(x = intensidade,
                    y = simetria,
                    color = factor(digito))) +
  geom_point(alpha = 0.2) +
  labs(x = "Intensidade",
       y = "Simetria",
       color = "Dígito") +
  theme_classic()


# vamos trabalhar apenas com digitos 0 e 1 -------------------------------
digitos <- digitos[digitos$digito == 0 | digitos$digito == 1, ]

# visualizacao da relacao entre intensidade e simetria
ggplot(digitos, aes(x = intensidade,
                    y = simetria,
                    color = factor(digito))) +
  geom_point(alpha = 0.2) +
  labs(x = "Intensidade",
       y = "Simetria",
       color = "Dígito") +
  theme_classic()

# visualizacao da densidade da variavel intensidade
ggplot(digitos,
       aes(intensidade, fill = factor(digito))) +
  geom_density(alpha = 0.5) +
  labs(x = "Intensidade",
       y = "Densidade",
       fill = "Dígito") +
  theme_bw()

# visualizacao da densidade da variavel simetria
ggplot(digitos,
       aes(simetria, fill = factor(digito))) +
  geom_density(alpha = 0.5) +
  labs(x = "Simetria",
       y = "Densidade",
       fill = "Dígito") +
  theme_bw()

# treino e validacao ------------------------------------------------------
set.seed(1234)

ids <- sample(1:nrow(digitos), size = 0.8 * nrow(digitos))

digitos_tr <- digitos[ids,]
digitos_tst <- digitos[-ids,]


# Regressao Logistica -----------------------------------------------------

fit <- glm(digito ~ intensidade + simetria,
           data = digitos_tr,
           family = "binomial")

# probabilidades
pred_probs_tr <- predict(fit, digitos_tr, type = "response")

# labels
pred_labels_tr <- ifelse(pred_probs_tr > 0.5, 1, 0)

# comparativo: observado vs predito (dentro da amostra)
(tabela_tr <- table(predito = pred_labels_tr,
                    observado = digitos_tr$digito))

# precisao dentro da amostra
(prec_dentro <- (tabela_tr[1, 1] + tabela_tr[2, 2]) /
    nrow(digitos_tr))

# precisao dentro da amostra (calculo alternativo)
(prec_dentro <- mean(pred_labels_tr == digitos_tr$digito))

# probs fora da amostra
pred_probs_tst <- predict(fit, digitos_tst, type = "response")

# labels
pred_labels_tst <- ifelse(pred_probs_tst > 0.5, 1, 0)

# comparativo: observado vs predito (fora da amostra)
(tabela_tst <- table(predito = pred_labels_tst,
                     observado = digitos_tst$digito))

# precisao fora da amostra
(prec_fora <- (tabela_tst[1, 1] + tabela_tst[2, 2]) /
    nrow(digitos_tst))

# precisao dentro da amostra (calculo alternativo)
(prec_fora <- mean(pred_labels_tst == digitos_tst$digito))

# salvar resultados em um dataframe
resultados <- data.frame(modelo = c("Reg. Logística",
                                    "Naive Bayes",
                                    "kNN"),
                         prec_dentro = NA,
                         prec_fora = NA)

resultados[resultados$modelo == "Reg. Logística", 2:3] <-
  c(prec_dentro, prec_fora)

resultados

# boundary

grid <-
  expand.grid(intensidade = seq(min(digitos_tr$intensidade),
                                max(digitos_tr$intensidade),
                                length.out = 300),
              simetria = seq(min(digitos_tr$simetria),
                             max(digitos_tr$simetria),
                             length.out = 300))

grid$pred <- ifelse(predict(fit, grid, type = "response") > 0.5,
                    1,
                    0)

ggplot() +
  geom_tile(aes(intensidade,
                simetria,
                fill = factor(pred)),
            data = grid, alpha = 0.5,
            show.legend = FALSE) +
  geom_point(aes(intensidade,
                 simetria,
                 color = factor(digito)),
             data = digitos_tr,
             alpha = 0.5) +
  labs(x = "Intensidade",
       y = "Simetria",
       color = "Dígito",
       title = "Regressão logística") +
  theme_classic()

# Naive Bayes -------------------------------------------------------------

fit_nb <- naiveBayes(digito ~ intensidade + simetria,
                     data = digitos_tr)

# predicao dentro da amostra
class_nb_tr <- predict(fit_nb, digitos_tr)

# comparativo: observado vs predito (dentro da amostra)
(tabela_tr <- table(predito = class_nb_tr,
                    observado = digitos_tr$digito))

# precisao dentro da amostra
(prec_dentro <- (tabela_tr[1, 1] + tabela_tr[2, 2]) /
    nrow(digitos_tr))

# predicao fora da amostra
class_nb_tst <- predict(fit_nb, digitos_tst)

# comparativo: observado vs predito (fora da amostra)
(tabela_tst <- table(predito = class_nb_tst,
                     observado = digitos_tst$digito))

# precisao fora da amostra
(prec_fora <- (tabela_tst[1, 1] + tabela_tst[2, 2]) /
    nrow(digitos_tst))

resultados[resultados$modelo == "Naive Bayes", 2:3] <-
  c(prec_dentro, prec_fora)

resultados

# boundary
grid$pred <- predict(fit_nb, grid)

ggplot() +
  geom_tile(aes(intensidade,
                simetria,
                fill = factor(pred)),
            data = grid, alpha = 0.5,
            show.legend = FALSE) +
  geom_point(aes(intensidade,
                 simetria,
                 color = factor(digito)),
             data=digitos_tr,  alpha = 0.2) +
  labs(x = "Intensidade",
       y = "Simetria",
       color = "Dígito",
       title = "Naive Bayes") +
  theme_classic()


# kNN ---------------------------------------------------------------------

# normalizar dados de treino pela amplitude
knn_tr <-
  data.frame(digito = digitos_tr$digito,
             intensidade = (digitos_tr$intensidade - min(digitos_tr$intensidade))/
               (max(digitos_tr$intensidade) - min(digitos_tr$intensidade)),
             simetria = (digitos_tr$simetria - min(digitos_tr$simetria))/
               (max(digitos_tr$simetria) - min(digitos_tr$simetria)))

# normalizar dados de teste pela amplitude
knn_tst <-
  data.frame(digito = digitos_tst$digito,
             intensidade <- (digitos_tst$intensidade - min(digitos_tr$intensidade))/
               (max(digitos_tr$intensidade) - min(digitos_tr$intensidade)),
             simetria <- (digitos_tst$simetria - min(digitos_tr$simetria))/
               (max(digitos_tr$simetria) - min(digitos_tr$simetria)))

# predicao dentro da amostra
set.seed(1)
knn_pred <- knn(knn_tr[,-1], knn_tr[,-1], knn_tr$digito, k = 3)
tabela_tr <- table(predito = knn_pred, observado = knn_tr$digito)

(prec_dentro <- (tabela_tr[1, 1] + tabela_tr[2, 2]) /
    nrow(knn_tr))

# predicao fora da amostra
set.seed(1)
knn_pred <- knn(knn_tr[,-1], knn_tst[,-1], knn_tr$digito, k = 3)
tabela_tst <- table(predito = knn_pred, observado = knn_tst$digito)

(prec_fora <- (tabela_tst[1, 1] + tabela_tst[2, 2]) /
    nrow(knn_tst))

resultados[resultados$modelo == "kNN", 2:3] <-
  c(prec_dentro, prec_fora)


# boundary
grid2 <-
  data.frame(intensidade = (grid$intensidade - min(digitos_tr$intensidade))/
               (max(digitos_tr$intensidade) - min(digitos_tr$intensidade)),
             simetria = (grid$simetria - min(digitos_tr$simetria))/
               (max(digitos_tr$simetria) - min(digitos_tr$simetria)))

grid2$pred <- knn(knn_tr[,-1], grid2, knn_tr$digito, k = 3)

ggplot() +
  geom_tile(aes(intensidade,
                simetria,
                fill = factor(pred)),
            data = grid2, alpha = 0.5,
            show.legend = FALSE) +
  geom_point(aes(intensidade, simetria, color = factor(digito)),
             data = knn_tr, alpha = 0.2) +
  labs(x = "Intensidade", y = "Simetria",
       color = "Dígito", title = "kNN") +
  theme_classic()

# TAREFA: escolher o melhor valor de k para esse problema

