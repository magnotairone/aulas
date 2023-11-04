library(tidyverse)

# spline ------------------------------------------------------------------

# modelo: f(x) + epsilon
# modelo: 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4)
# 
# considere diferentes valores para os graus de liberdade (gl) abaixo

gl <- 3     # 2 a 30

n_obs <- 30

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + 
                  rnorm(n = n_obs, mean = 0, sd = 4))

x_prev <- seq(8, 18, 0.01)

fit <- smooth.spline(dados$x, dados$y, df = gl, all.knots = TRUE)

dados_prev <- tibble(x = x_prev, 
                     y = predict(fit, x_prev)$y)

ggplot() + 
  geom_point(data = dados, aes(x, y)) + 
  geom_line(data = dados_prev, aes(x, y), color = "red", 
            size = 1.2, alpha = .5) + 
  xlim(8, 18) 




# avaliar previsão com base no conjunto de treinamento --------------------

n_obs <- 30

gl <- 2:30

resultados <- tibble(gl = gl, 
                     eqm_tr = NA)

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

for (i in 1:nrow(resultados)) {
  
  fit <- smooth.spline(dados$x, dados$y, 
                       df = resultados$gl[i], 
                       all.knots = TRUE)
  
  resultados$eqm_tr[i] <- mean((dados$y - fit$y)^2)
  
}

resultados

resultados %>% 
  ggplot(aes(gl, eqm_tr)) + 
  geom_hline(yintercept = 0, color = "red", size = 1.2) +   
  geom_point(size = 2) + 
  geom_line(size = 1.2)+
  labs(x = "graus de liberdade", y = "EQM")


ggplot(resultados, mapping = aes(gl, eqm_tr))+
  geom_hline(yintercept = 0, color = "red", size = 1.2) +   
  geom_point(size = 2) + 
  geom_line(size = 1.2)+
  labs(x = "graus de liberdade", y = "EQM")

# Usando x = 11 e x = 15 como conjunto de teste ---------------------------

n_obs <- 30 

gl <- 2:30 # 2 a 30

n_simulacao <- 1:50

dados_prev <- crossing(n_simulacao, gl) %>% 
  mutate(y_11 = NA, 
         y_15 = NA)

set.seed(123)

for (i in 1:nrow(dados_prev)) {
  
  dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                  y = 45*tanh(x/1.9 - 7) + 57 + 
                    rnorm(n = n_obs, mean = 0, sd = 4))
  
  fit <- smooth.spline(dados$x, dados$y, 
                       df = dados_prev$gl[i], 
                       all.knots = TRUE)
  
  dados_prev$y_11[i] <- predict(fit, x = 11)$y
  dados_prev$y_15[i] <- predict(fit, x = 15)$y
  
}


resultados <- dados_prev %>% 
  group_by(gl) %>% 
  summarise(vies2_11 = (mean(y_11) - (45*tanh(11/1.9 - 7) + 57))^2,
            var_11 = var(y_11),
            vies2_15 = (mean(y_15) - (45*tanh(15/1.9 - 7) + 57))^2,
            var_15 = var(y_15)) %>% 
  mutate(eqm_11 = vies2_11 + var_11, 
         eqm_15 = vies2_15 + var_15)


ggplot() +
  geom_line(data = resultados, aes(gl, eqm_11), color = "blue", size = 1.2) + 
  geom_line(data = resultados, aes(gl, eqm_15), color = "red", size = 1.2) + 
  labs(x = "graus de liberdade", y = "eqm") + 
  coord_cartesian(ylim = c(0,150))+
  scale_x_continuous(breaks = seq(2, 30, 2), minor_breaks = seq(1,30,2))

resultados %>% 
  filter(eqm_11 == min(eqm_11))

resultados %>% 
  filter(eqm_15 == min(eqm_15))



# validação cruzada -------------------------------------------------------

n_obs <- 30

set.seed(123)

dados <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

dados %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  theme_bw()

folds <- 5

nrow(dados)

# abordagem 1

lote <- sample(1:folds, size = n_obs, replace = TRUE)

table(lote)

# abordagem 2

lote <- numeric(n_obs)

for (i in 1:(folds - 1)) 
  lote[sample(which(lote == 0), n_obs/folds, replace = FALSE)] <- i

lote[which(lote == 0)] <- folds

table(lote)

# abordagem 3

lote <- sample(rep(1:folds, n_obs/folds, length.out = n_obs))

table(lote)

# -----

eqm_aux <- numeric(folds)

gl <- 2:24 # para validação cruzada

resultados_cv <- tibble(gl = gl, 
                        eqm = NA)

for (j in 1:nrow(resultados_cv)) {
  
  for (i in 1:folds) {
    
    fit <- smooth.spline(dados$x[lote != i], dados$y[lote != i], 
                         df = resultados_cv$gl[j], 
                         all.knots = TRUE)
    
    predito <- predict(fit, dados$x[lote == i])$y
    
    eqm_aux[i] <- mean((dados$y[lote == i] - predito)^2)
    
  }
  
  resultados_cv$eqm[j] <- mean(eqm_aux)
  
}


resultados_cv %>% 
  filter(eqm == min(eqm))

(gl_otimo <- resultados_cv$gl[which.min(resultados_cv$eqm)])

resultados_cv %>% 
  ggplot(aes(gl, eqm)) + 
  geom_line(color = "blue", size=1.5) + 
  geom_point(color = "blue", size=2) + 
  geom_vline(xintercept = gl_otimo, color = "red", linetype="dashed")+
  coord_cartesian(ylim = c(0,200))

fit_spline <- smooth.spline(dados$x, dados$y, df = gl_otimo)


# KNN ---------------------------------------------------------------------
# install.packages("FNN")
library(FNN)

set.seed(123)

n_sample <- 30
n_vizinhos <- 30  # avalie diferentes números de vizinhos mais próximos

dados <- tibble(x = sort(runif(n = n_sample, min = 8, max = 18)),
                y = 45*tanh(x/1.9 - 7) + 57 + 
                  rnorm(n = n_sample, mean = 0, sd = 4))

x_prev <- seq(8, 18, 0.01)

dados_pred <- tibble(x = x_prev, 
                     y = knn.reg(train = dados$x, 
                                 test = matrix(x_prev), 
                                 y = dados$y, 
                                 k = n_vizinhos)$pred)

dados %>% 
  ggplot(aes(x, y)) + 
  geom_point(color = "blue", size = 2, alpha = .3) +
  geom_step(data = dados_pred, aes(x, y), color = "red", size = 1) + 
  labs(x = "Anos de Escolaridade", y = "Renda Anual")

# Validação cruzada 5 lotes -----------------------------------------------

set.seed(123)

n_sample <- 30

dados <- tibble(x = sort(runif(n = n_sample, min = 8, max = 18)),
                y = 45*tanh(x/1.9 - 7) + 57 + 
                  rnorm(n = n_sample, mean = 0, sd = 4))

lote <- numeric(n_sample)


folds <- 5

# ---------------------------------------------------------
# TODO: definir quais observacoes pertencem a cada lote ---
# ---------------------------------------------------------


n_vizinhos <- 1:24 # 24 é o tamanho do conjunto de treinamento em cada iteração

resultados <- tibble(k = n_vizinhos, 
                     eqm = NA)

for (j in 1:nrow(resultados)) {
  
  for (i in 1:folds) {
    
    
  }
  
}

# ------------------------------------
# TODO: determinar qual é o k otimo --
# ------------------------------------

# ------------------------------------
# TODO: rodar novamente o knn com o --
# k otimo ----------------------------
# ------------------------------------
k_otimo <- NA

fit <- knn.reg(train = as.matrix(dados$x), 
               test = as.matrix(dados$x), 
               y = dados$y, 
               k = k_otimo)  
