# Aprendizagem Estatistica de Maquina II 
# Aula 7
# Atividade prática - Redes neurais
# Dados: mnist

library(keras)


# Carregar dados ----------------------------------------------------------
mnist <- dataset_mnist()

str(mnist)

x_train <- mnist$train$x
g_train <- mnist$train$y

x_test <- mnist$test$x
g_test <- mnist$test$y

dim(x_train)
dim(x_test)


x_train <- x_train / 255
x_test <- x_test / 255

# Plotando uma amostra ----------------------------------------------------
par(mfcol = c(6,6))
par(mar = c(0, 0, 2, 0)) 
#'i':  just finds an axis with pretty labels that fits within the original data range.

set.seed(1)
indices <- sample(nrow(x_test), 36)
for (idx in indices) { 
  im <- x_train[idx,,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col = gray((0:255)/255), 
        main = paste(g_train[idx]),
        xaxt='n', yaxt = 'n', xlab = "", ylab = "")
}

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

y_train <- to_categorical(g_train, 10)
y_test <- to_categorical(g_test, 10)

# Tarefa ------------------------------------------------------------------
# implementar uma rede neural para classificar as imagens

