# Aprendizagem Estatistica de Maquina II - 2023.1
# Aula 6
# Estimacao de rede neural com uma camada

# Pacote utilizado --------------------------------------------------------
library(tidyverse)

# Geracao de dados --------------------------------------------------------
set.seed(12)

(df <- tibble(area = rgamma(n = 100, scale = 5, shape = 20) / 100,
             preco = 1 + 6 * area + rnorm(n = 100, sd = 1)))


df %>% 
  ggplot(aes(area, preco)) +
  geom_point() +
  theme_classic()


# Estimacao usando lm -----------------------------------------------------
modelo <- lm(preco ~ area, data = df)

coef(modelo)

df %>% 
  ggplot(aes(area, preco)) +
  geom_point() +
  geom_abline(intercept = coef(modelo)[1], 
              slope = coef(modelo)[2],
              linetype = "dashed",
              color = "blue") +
  theme_classic()


# Estimacao usando uma rede neural ----------------------------------------
# definida manualmente

# arquitetura
model <- function(w, b, x) {
  w * x + b
}

# função de perda
loss <- function(y, y_hat) {
  mean((y - y_hat)^2)
}


# Estimacao via descida do gradiente --------------------------------------

y <- df$preco
x <- df$area

## derivadas (não se preocupe com essa parte!)
## dL/dw = dL/dy * dy/dw
## dL/db = dL/dy * dy/db

# derivada da loss com relação à y
dL_dyhat <- function(y_hat) {
  2 * (y - y_hat) * (-1)
}

# derivada de y com relação a w
dyhat_dw <- function(w) {
  x
}

# derivada de y com relação a b
dyhat_db <- function(b) {
  1
}

# inicializando os pesos
set.seed(12)
w <- runif(1)
b <- 0

# hiperparametro: learning rate
lr <- 0.1

for (step in 1:1000) {
  y_hat <- model(w, b, x)
  
  w <- w - lr * mean(dL_dyhat(y_hat) * dyhat_dw(w))
  b <- b - lr * mean(dL_dyhat(y_hat) * dyhat_db(b))
  
  if (step %% 100 == 0) {
    print(loss(y, y_hat))
  }
}

(nn_1 <- c(b, w))
coef(modelo)

df %>% 
  ggplot(aes(area, preco)) +
  geom_point() +
  geom_abline(intercept = coef(modelo)[1], 
              slope = coef(modelo)[2],
              linetype = "solid",
              color = "blue", size = 1.5) +
  geom_abline(intercept = nn_1[1],
             slope = nn_1[2],
             linetype = "dashed",
             color = "green", size = 1.5) +
  theme_classic()


# Estimacao via descida do gradiente estocastica --------------------------

# inicializando os pesos
set.seed(12)
w <- runif(1)
b <- 0

# separa os dados em bloquinhos de 20 (batches)
splits <- split(sample(seq_along(df$preco)), 
                seq_along(df$preco) %% 5)
splits

# numero de iteracoes (epochs)
for (epoch in 1:200) {
  
  for (minibatch in splits) {
    
    y <- df$preco[minibatch]
    x <- df$area[minibatch]
    
    y_hat <- model(w, b, x)
    w <- w - lr * mean(dL_dyhat(y_hat) * dyhat_dw(w))
    b <- b - lr * mean(dL_dyhat(y_hat) * dyhat_db(b))
    
  }
  
  if (epoch %% 10 == 0) print(loss(y, y_hat))
}

(nn_2 <- c(b, w))
coef(modelo)
nn_1

df %>% 
  ggplot(aes(area, preco)) +
  geom_point() +
  geom_abline(intercept = coef(modelo)[1], 
              slope = coef(modelo)[2],
              linetype = "solid",
              color = "blue", size = 1.5) +
  geom_abline(intercept = nn_1[1],
              slope = nn_1[2],
              linetype = "dashed",
              color = "green", size = 1.5) +
  geom_abline(intercept = nn_2[1],
              slope = nn_2[2],
              linetype = "dashed",
              color = "orange", size = 1.5) +
  theme_classic()
