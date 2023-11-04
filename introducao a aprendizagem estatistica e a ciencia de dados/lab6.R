# Agenda
# arvore de classificacao, poda da arvore
# bagging, floresta aleatoria e boosting

library(modeldata) # contem os dados que vamos usar no lab
library(tree) # funcoes para estimar arvore de decisao
library(randomForest) # funcoes para estimar floresta aleatoria
library(gbm) # funcoes para estimar o boosting
library(ggplot2)

data(mlc_churn) # dados simulados de churn de clientes

View(mlc_churn) # visualizar base de dados como planilha
str(mlc_churn) # estrutura da base de dados

table(mlc_churn$churn) # numero de clientes em cada categoria

table(mlc_churn$churn) / nrow(mlc_churn) # mesmo acima mas em porcentagem

# trocar a ordem das categorias no factor
levels(mlc_churn$churn)

mlc_churn$churn <- factor(mlc_churn$churn, levels = c("no", "yes"))

levels(mlc_churn$churn)

# definicao das observacoes no conjunto de treino
set.seed(12)
ids <- sample(nrow(mlc_churn),
              size = 0.8 * nrow(mlc_churn),
              replace = FALSE)

# variaveis que serao consideradas na analise
# vamos desconsiderar a coluna state
variaveis <- colnames(mlc_churn) != "state"

# data frame com resultados dos modelos
resultados <- data.frame(modelo = c("arvore", "arvore podada",
                                    "bagging", "rf",
                                    "bst", "logistica"),
                         prec_fora = NA)


# Arvore de classificacao -------------------------------------------------
# estimacao
arvore <- tree(churn ~., data = mlc_churn[ids, variaveis])

arvore # ver estrutura da arvore estimada

plot(arvore) # ver arvore graficamente
text(arvore) # adiciona elementos de texto

# predicao fora da amostra
pred_fora <- ifelse(predict(arvore,
                            newdata = mlc_churn[-ids, variaveis])[,2] > 0.5,
                    "yes", "no")

# alternativa: usar a funcao predict com argumento type="class"
pred_fora <- predict(arvore,
                     newdata = mlc_churn[-ids, variaveis],
                     type = "class")

# tabela predito vs observado fora a amostra
table(pred = pred_fora,
      obs = mlc_churn$churn[-ids])

# calculo da precisao (acuracia) do modelo
precisao_fora_arv <- mean(pred_fora == mlc_churn$churn[-ids])

# atualizacao do dataframe de resultados
resultados[resultados$modelo=="arvore", 2] <- precisao_fora_arv

resultados


# Arvore podada -----------------------------------------------------------

# o valor de k pode ser escolhido por validação ou validação cruzada
arvore_poda <- prune.misclass(arvore, k = 20)

summary(arvore_poda) # resumo da arvoe podada

# TAREFA: fazer valicadao cruzada para escolher valor de k
# dica: ?cv.tree()

# predicao fora da amostra
pred_fora_poda <- predict(arvore_poda,
                            mlc_churn[-ids, variaveis],
                            type="class")

# tabela predito vs observado fora a amostra
table(pred = pred_fora_poda,
      obs = mlc_churn$churn[-ids])

# calculo da precisao
precisao_fora_poda <- mean(pred_fora_poda ==
                             mlc_churn$churn[-ids])

# atualizacao do dataframe de resultados
resultados[resultados$modelo=="arvore podada", 2] <-
  precisao_fora_poda

resultados

# Bagging -----------------------------------------------------------------

# estimacao
set.seed(1)
bag <- randomForest(churn ~ .,
                   data = mlc_churn[ids, variaveis],
                   mtry = ncol(mlc_churn[,variaveis])-1)

bag

ggplot(data.frame(ntree = 1:500,
           OOB = bag$err.rate[,1]),
       aes(ntree, OOB)) +
  geom_line() +
  labs(x = "Número de árvores",
       y = "Erro OOB") +
  theme_classic()
# TAREFA: reduzir o numero de arvores

# calculo da precisao (acuracia) do modelo
prec_fora_bag <- mean(predict(bag, mlc_churn[-ids, variaveis]) ==
                        mlc_churn$churn[-ids])

# atualizacao do dataframe de resultados
resultados[resultados$modelo=="bagging", 2] <- prec_fora_bag

resultados

# Floresta aleatoria ------------------------------------------------------
set.seed(1)
rf <- randomForest(churn ~ .,
                    data = mlc_churn[ids, variaveis])

ggplot(data.frame(ntree = 1:500,
                  OOB = rf$err.rate[,1]),
       aes(ntree, OOB)) +
  geom_line() +
  labs(x = "Número de árvores",
       y = "Erro OOB") +
  theme_classic()
# TAREFA: diminuir o numero de arvores do randomForest

# importancia das variaveis
importance(rf)
varImpPlot(rf)
# alternativa:
library("vip")
vip(rf)

# calculo da precisao (acuracia) do modelo
prec_fora_rf <- mean(predict(rf, mlc_churn[-ids, variaveis]) ==
                       mlc_churn$churn[-ids])


# atualizacao do dataframe de resultados
resultados[resultados$modelo=="rf", 2] <- prec_fora_rf

resultados

# Boosting ----------------------------------------------------------------
# preparacao dos dados para o boosting (tem que ser numerico)
mlc_churn$churn <- ifelse(mlc_churn$churn == "yes", 1, 0)
# atencao: o codigo acima altera a variavel original!!!

set.seed(1)
bst <- gbm(churn ~ ., # formula
           distribution = "bernoulli", # para indicar problema de classificacao
           n.trees = 100, # numero de arvores
           interaction.depth = 1, # profundidade maxima da arvore # 4
           shrinkage = 0.1, # taxa de aprendizagem # 0.01
           data = mlc_churn[ids, variaveis])

# importancia das variaveis
summary(bst)
vip(bst)

# grafico de dependencia parcial de total_day_minutes
plot(bst, i="total_day_minutes")

# grafico de dependencia parcial de number_customer_service_calls
plot(bst, i="number_customer_service_calls")

# calculo da precisao (acuracia) do modelo
prec_fora_bst <- mean(ifelse(predict(bst, mlc_churn[-ids, variaveis],
                                     n.trees = 100,
                                     type = "response") > 0.5, 1, 0) ==
                        mlc_churn$churn[-ids])

# atualizacao do dataframe de resultados
resultados[resultados$modelo=="bst", 2] <- prec_fora_bst

resultados

# logística ---------------------------------------------------------------
# ajuste regressao logistica
fit_log <- glm(churn ~ .,
               data = mlc_churn[ids,variaveis],
               family = "binomial")

# calculo da precisao (acuracia) do modelo
prec_fora_log <-
  mean(ifelse(predict(fit_log,
                      newdata = mlc_churn[-ids,variaveis],
                      type = "response") > 0.5, 1, 0) == mlc_churn$churn[-ids])

# atualizacao do dataframe de resultados
resultados[resultados$modelo == "logistica", 2] <- prec_fora_log

# Comparativo final -------------------------------------------------------

resultados[order(resultados$prec_fora,
                 decreasing = TRUE),]

