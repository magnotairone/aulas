library(ggplot2) # biblioteca para construcao de graficos

set.seed(1234) # define uma semente aleatoria

n_obs <- 50 # numero de observacoes que serao geradas

# geracao do vetor de valores de x, representando anos de estudo
anos_estudo <- runif(n = n_obs, min = 8, max = 18)

# geracao do vetor de valores de y, representando os salarios (em centenas de reais)
salario <- 45*tanh(anos_estudo/1.9 - 7) + 60 +
  rnorm(n = n_obs, mean = 0, sd = 4) # o erro aleatorio

# armazena os dados em um data frame
dados <- data.frame(x = anos_estudo, y = salario)

# visualizacao dos dados
ggplot(dados, aes(x = x, y = y)) + # define quais valores estarao nos eixos
  geom_point() + # grafico de pontos
  labs(x = "Anos de estudo", y = "Salário")

# calculo das estimativas do modelo de regressao linear manualmente -----

X <- matrix(c(rep(1, n_obs), # colunas de 1s (intercepto)
              dados$x), # colunas com os valores de X
            nrow = n_obs) # numero de linhas da matriz

Y <- dados$y

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y # solucao de minimos quadrados
beta_hat

# visualizacao do resultado
ggplot(dados, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) + # alpha indica a transparencia da geometria
  geom_abline(intercept = beta_hat[1], slope = beta_hat[2], # adiciona reta estimada
              color = "red", linetype = "dashed") +
  labs(x = "Anos de estudo", y = "Salário") # altera os labels do grafico

# calculo das estimativas do modelo de regressao linear com lm ----------
fit <- lm(y~x, dados)
fit

# avaliacao do modelo -----------------------------------------------------
pred <- X %*% beta_hat # predicao feita manualmente (obtemos uma matriz coluna)

pred2 <- predict(fit, dados) # predicao feita com a funcao predict

# armazena predicoes e valores observados em um dataframe
obs_pred <- data.frame(observado = dados$y,
                       predito = pred)

ggplot(obs_pred, aes(x = observado, y = predito)) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +# reta identidade
  geom_point() + # geometria de pontos
  labs(x = "Observado", y = "Predito")

# calulo do erro dentro da amostra - opcao 1
(eqm <- sum((obs_pred$observado - obs_pred$predito)^2) / n_obs)

# calulo do erro dentro da amostra - opcao 2
(eqm <- mean((obs_pred$observado - obs_pred$predito)^2))

# calulo do erro dentro da amostra - opcao 3
library(Metrics)
(eqm <- mse(obs_pred$observado, obs_pred$predito))

# predicao para um dado novo ------
dados_predicao <- data.frame(x = c(10, 15, 17))
predicao <- predict(fit, dados_predicao)

round(predicao, 3) # arrendonda os valores para 3 casas decimais


# Modelo com mais de uma preditora ----------------------------------------
set.seed(123)

# numero de observacoes que serao geradas
n_obs <- 50

# geracao do vetor de valores de x, representando anos de estudo
anos_estudo <- runif(n = n_obs, min = 8, max = 18)

# geracao do vetor de indicadoras para representar se fez o curso de aprendizagem
curso_aprendizagem <- sample(0:1, size = n_obs, replace = TRUE)

# geracao do vetor de valores de y, representando os salarios (em centenas de reais)
salario <- 45*tanh(anos_estudo/1.9 - 7) + 60 +
  curso_aprendizagem*40 +
  rnorm(n = n_obs, mean = 0, sd = 4) # o erro aleatorio

# armazena os dados em um data frame
dados <- data.frame(x1 = anos_estudo, x2 = curso_aprendizagem, y = salario)

# visualizacao dos dados
ggplot(dados, aes(x = x1, y = y, color = factor(x2))) +
  geom_point() +
  labs(x = "Anos de estudo", y = "Salário", color = "Fez curso \n aprendizagem")

# calculo das estimativas do modelo de regressao linear com lm ----------
fit <- lm(y ~ x1 + x2, dados)
fit

# TAREFA: obter as estimativas do modelo de regressao linear manualmente

# Filtrar um data frame
dados2 <- dados[dados$x2 == 1,] # somente observacoes com x2 = 1 (fez curso aprendizagem)
sum(dados$x2 == 1) # numero de observacoes em que x2 = 1 
nrow(dados2) # numero de linhas do dataframe filtrado

fit2 <- lm(y ~ x1, dados2)
fit2

# aplicando transformacao nas variaveis
dados2$raiz_x1 <- sqrt(dados2$x1)

fit3 <- lm(y ~ x1 + raiz_x1, dados2)
fit3

# aplicacao dados reais ---------------------------------------------------

# carregar dados que estao no arquivo csv
dados <- read.csv("sao-paulo-properties.csv",
                  fileEncoding = "UTF-8")

# filtar para considerar apenas imoveis disponiveis para aluguel
aluguel <- dados[dados$Negotiation.Type == "rent", ]

# grafico de preco versus tamanho do imovel
ggplot(aluguel, aes(x = Size, y = Price)) +
  geom_point(alpha = 0.1) # alpha define a transparencia da geometria (entre 0 e 1)

# ajuste modelo linear considerando tamanho e valor do condominio
fit <- lm(Price ~ Size + Condo, aluguel)
summary(fit)

# Pergunta: como estimar o erro neste caso?
