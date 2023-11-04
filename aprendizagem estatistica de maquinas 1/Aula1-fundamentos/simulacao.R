library(tidyverse)


# Introdução --------------------------------------------------------------

# geração de números aleatórios

rnorm(10, mean = 0, sd = 1)

# criar uma sequência 

seq(10, 13, .5)

# criar um data frame

df <- tibble(var1 = c(1, 3, 2), 
             var2 = c(11, 7, 18))

df

df$var1


# Geracao de uma amostra --------------------------------------------------

# numero de observacoes em cada amostra
n_obs <- 30 

# desvio padrao do erro aleatorio
sd <- 4 

# geracao do vetor de valores x
valor_x <- runif(n = n_obs, min = 8, max = 18) 

# geracao do  vetor de valores de y
valor_y <- 45*tanh(valor_x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = sd)

# tabela com valores de x e y
dados <- tibble(x = valor_x,
                y = valor_y)

# grafico de dispersao dos dados
dados %>% 
  ggplot(aes(x, y)) +
  geom_point(col="red")

# grafico  de dispersao dos dados com funcao f
dados %>% 
  ggplot(aes(x, y)) +
  geom_point(col="red") +
  stat_function(fun =  function(x) 45*tanh(x/1.9 - 7) + 57, size = 1.5, col="black")


# valores para os quais faremos as previsões
x_prev <- seq(8, 18, 0.5)


# Modelo usando apenas o valor medio de y ---------------------------------


# banco de dados com os valores e previsões
dados_prev <- tibble(x = x_prev, 
                     y = mean(dados$y))

# gráfico dos valores simulados e valores preditos
ggplot() + 
  geom_point(data = dados, aes(x, y), col="red") + 
  geom_line(data = dados_prev, aes(x, y), color = "blue") + 
  xlim(8, 18) + ylim(0, 130)



# Repetindo 100 vezes a estimacao pelo modelo medio ----------------------

set.seed(312)

modelo_medio <- vector("numeric", 100)


n_obs <- 30

for (i in 1:100) {
  
  dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                  y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))
  
  modelo_medio[i] <- mean(dados$y)
  
}

# grafico com todos os modelos
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = function(x) 45*tanh(x/1.9 - 7) + 57, size = .8) +  
  geom_hline(yintercept = modelo_medio, color = "red", alpha=0.5)+
  geom_point(data = tibble(x = 11, y = 45*tanh(11/1.9 - 7) + 57), 
             aes(x, y), color = "red", size = 5) + 
  xlim(8, 18)


# grafico com a media de todos os modelos
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = function(x) 45*tanh(x/1.9 - 7) + 57, size = .8) +  
  geom_hline(yintercept = mean(modelo_medio), color = "blue", size=2) + 
  geom_point(data = tibble(x = 11, y = 45*tanh(11/1.9 - 7) + 57), 
             aes(x, y), color = "red", size = 5) + 
  xlim(8, 18)


# avaliando para o ponto x=11
mean(modelo_medio) - (45*tanh(11/1.9 - 7) + 57)

var(modelo_medio)

modelo_medio


# mais flexível ---------------------------------------------------
# objetivo: calcular o valor de y para x = 11

set.seed(1234)

modelo_medio <- vector("numeric", 100)

# graus de liberdade
gl <- 4 # experimente trocar este valor

n_obs <- 30


for (i in 1:100) {
  
  dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                  y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))
  
  fit <- smooth.spline(dados$x, dados$y, df = gl)
  
  modelo_medio[i] <- predict(fit, 11)$y
  
}


ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = function(x) 45*tanh(x/1.9 - 7) + 57, size = .8) +  
  geom_point(data = tibble(x = 11, y = mean(modelo_medio)), aes(x, y), color = "blue", size = 5) + 
  geom_point(data = tibble(x = 11, y = 45*tanh(11/1.9 - 7) + 57), 
             aes(x, y), color = "red", size = 5) + 
  xlim(8, 18)


# avaliando para o ponto x=11
mean(modelo_medio) - (45*tanh(11/1.9 - 7) + 57)

var(modelo_medio)

modelo_medio
