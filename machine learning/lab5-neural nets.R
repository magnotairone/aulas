# Machine Learning - 2022-1 - Laboratório 05
# Redes Neurais


# Carregar biblioteca e dados ---------------------------------------------

library(keras)

mnist <- dataset_mnist()

x_train <- mnist$train$x
y_train <- mnist$train$y

x_test <- mnist$test$x
y_test <- mnist$test$y


# Plotar uma amostra dos dados --------------------------------------------

par(mfcol=c(6,6),
    mar=c(0, 0, 2, 0)) 

set.seed(1)
indices <- sample(nrow(x_test), 36)
for (idx in indices) { 
  im <- x_train[idx,,]
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255), 
        xaxt='n', yaxt='n', main=(y_train[idx]))
}

# Preparando os dados -----------------------------------------------------
# The x data is a 3-d array (images,width,height) of grayscale values . 
# To prepare the data for training we convert the 3-d arrays into matrices by 
# reshaping width and height into a single dimension (28x28 images are flattened into
# length 784 vectors). Then, we convert the grayscale values from integers ranging between 
# 0 to 255 into floating point values ranging between 0 and 1:

# Exemplo reshape ---------------------------------------------------------

# matriz 4x4 (uma imagem)
x <- matrix(1:16, nrow = 4)
x

# rearrange will fill the array row-wise
array_reshape(x, c(1, 16))

# setting the dimensions 'fills' the array col-wise
dim(x) <- c(16, 1)
x
rm(x)
## fim

# Reshape -----------------------------------------------------------------
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# Rescale -----------------------------------------------------------------
x_train <- x_train / 255
x_test <- x_test / 255

# The y data is an integer vector with values ranging from 0 to 9. To prepare this data 
# for training we one-hot encode the vectors into binary class matrices using the Keras 
# to_categorical() function:
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Definindo o modelo ------------------------------------------------------
# use_session_with_seed(4321)

modelo1 <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = 'relu', 
              input_shape = c(784)) %>% 
  # layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  # layer_dropout(rate = 0.3) %>%  
  layer_dense(units = 10, activation = 'softmax')

summary(modelo1)

# Compilando o modelo -----------------------------------------------------
# to configure the learning process
# compile the model with appropriate loss function, optimizer, and metrics:
modelo1 <- compile(modelo1,
                   loss = 'categorical_crossentropy', 
                   optimizer = "adam", #ver comentario abaixo
                   metrics = c("accuracy")) #pode ser customizada


# OPTIMIZER: you can think of a hiker trying to get down a mountain with a blindfold on.
# It’s impossible to know which direction to go in, but there’s one thing she can know: 
# if she’s going down (making progress) or going up (losing progress). Eventually, if 
# she keeps taking steps that lead her downwards, she’ll reach the base.

# ADAM stands for adaptive moment estimation, and is another way of using past gradients
# to calculate current gradients. Adam also utilizes the concept of momentum by adding 
# fractions of previous gradients to the current one.

# Treinando o modelo ------------------------------------------------------

# Trains the model for a fixed number of epoc
history <- fit(modelo1, x_train, y_train, 
               epochs = 30, 
               batch_size = 128, 
               validation_split = 0.2)

# batch_size: defines the number of samples to work through before updating the 
# internal model parameters.
# epoch: defines the number times that the learning algorithm will work through 
# the entire training dataset (One epoch means that each sample in the training 
# dataset has had an opportunity to update the internal model parameters. ).

# There are no magic rules for how to configure these parameters. You must try different
# values and see what works best for your problem.

history

plot(history)

modelo1 %>% 
  evaluate(x_test, y_test)

# Predicao ----------------------------------------------------------------
probs <- modelo1 %>% 
  predict(x_test)

probs[1, ]

class_pred <- modelo1 %>% 
  predict(x_test) %>% 
  k_argmax() %>% 
  as.numeric()

par(mfcol=c(6,6),
    mar=c(0, 0, 2, 0)) 

for (idx in indices) { 
  im <- mnist$test$x[idx,,]
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255), 
        xaxt='n', yaxt='n', 
        main=paste0("Obs: ", mnist$test$y[idx], 
                 ", Pred: ", class_pred[idx]))
}

table(pred=pred1, obs=mnist$test$y)

