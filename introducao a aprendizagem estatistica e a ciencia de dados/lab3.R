library(ggplot2) # biblioteca para construcao de graficos

# funcao que gera dados para simulacao ------------------------------------

f <- function(x){
  return (45 * tanh(x/1.9 - 7) + 60)
}

gera_salarios <- function(anos_estudo, media_erro = 0, desvio_erro = 4){
  return (f(anos_estudo) + rnorm(n = length(anos_estudo),
                                 mean = media_erro, sd = desvio_erro))
}

gera_salarios(1)
gera_salarios(1)
gera_salarios(5:10)

# -------------------------------------------------------------------------
# Modelo linear -----------------------------------------------------------
set.seed(1) # semente aleatoria

n_obs <- 100 # numero de observacoes que serao geradas
anos_estudo <- runif(n = n_obs,
                     min = 8, max = 18) # valores para anos de estudo
salario <- gera_salarios(anos_estudo) # valores para salario

# cria o dataframe com todos os dados
dados <- data.frame(x = anos_estudo, y = salario)

fit1 <- lm(y ~ x, dados) # ajuste do modelo

summary(fit1) # sumario das estimativas do modelo

# grafico com resultados
ggplot(data = dados, aes(x, y)) +
  geom_point(alpha = 0.5) + # dados usados pelo modelo
  geom_abline(intercept = fit1$coefficients[1],
              slope = fit1$coefficients[2],
              color = "red") +
  theme_bw()

# erro dentro da amostra
y_hat <- predict(fit1, dados)
(erro_dentro <- mean((y_hat - dados$y)^2))

# -------------------------------------------------------------------------
# Modelo linear - treino e validação --------------------------------------
set.seed(123)

# seleciona aleatoriamente 80% dos dados para construir o
# conjunto de treinamento
ids_treino <- sample(1:nrow(dados),
                     size = 0.80 * nrow(dados),
                     replace = FALSE)

treino <- dados[ids_treino, ] # dados de treino
teste <- dados[-ids_treino, ] # dados de teste

fit2 <- lm(y ~ x, treino) # ajuste do modelo linear

summary(fit2) # sumario das estimativas do modelo

# erro dentro da amostra
(erro_dentro2 <- mean((predict(fit2, treino) - treino$y)^2))
# erro fora dentro da amostra
(erro_fora2 <- mean((predict(fit2, teste) - teste$y)^2))

# -------------------------------------------------------------------------
# Modelo linear - validacao cruzada ---------------------------------------
set.seed(1)

k <- 5 # numero de lotes na valicadao cruzada

# definicao dos lotes
lote <- sample(1:k, size = nrow(dados), replace = TRUE)

table(lote) # numero de observacoes em cada lote

erro_tmp <- numeric(k) # vetor que vai guardar estimativa dos
# erros em cada lote

# inicio da validacao cruzada
for(i in 1:k){
  treino <- dados[lote != i,] # dados de treino
  teste <- dados[lote == i,] # dados de teste

  fit_vc <- lm(y ~ x, treino) # estimacao do modelo

  # erro fora da amostra para o lote i
  erro_tmp[i] <- mean((predict(fit_vc, teste) - teste$y)^2)
}
# fim da validacao cruzada

# erro fora da amostra estimado pela validacao cruazada
(erro_vc <- mean(erro_tmp))


# Comparativo -------------------------------------------------------------
# comparativo do erro estimado em diferentes cenarios
data.frame(erro = c("Dentro da amostra",
                    "Separação treino e validação",
                    "Validação cruzada"),
           valor = c(erro_dentro, erro_fora2, erro_vc))


# EXTRA: estudo sobre a ---------------------------------------------------
# decomposicao do erro em vies e variancia --------------------------------
set.seed(1234) # define semente aleatoria

N <- 1000 # numero de repeticoes que vamos fazer

y_pred <- numeric(N)

n_obs <- 30 # numero de observacoes em cada conjunto gerado

for (i in 1:N) {
  anos_estudo <- runif(n = n_obs, min = 8, max = 18) # gera valores para anos de estudo
  salario <- gera_salarios(anos_estudo) # gera valores para salario
  dados <- data.frame(x = anos_estudo, y = salario) # armazena os dados em um dataframe

  fit <- lm(y ~ x, dados)

  y_pred[i] <- predict(fit, newdata = data.frame(x=11)) #avaliar predicao no ponto x=11
}

var(y_pred) # variancia
(mean(y_pred) - f(11))^2 # vies2
var(y_pred) + (mean(y_pred) - f(11))^2 # soma de variancia e vies2
mean((y_pred - f(11))^2) # erro de predicao
