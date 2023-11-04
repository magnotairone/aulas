# Aprendizagem Estatistica de Maquina II 
# Aula 7
# Rede neural convolucional para classificacao
# Dados: review de filmes no imdb

# Pacotes utilizados ------------------------------------------------------
library(keras) # rede neural
library(Matrix) # representacao de matriz esparca
library(tidyverse)


# Dados imdb --------------------------------------------------------------

max_features <- 10000 # limite do tamanho do dicionario
imdb <- dataset_imdb(num_words = max_features)

x_train <- imdb$train$x
y_train <- imdb$train$y

x_test <- imdb$test$x
y_test <- imdb$test$y

# primeiras palavras da primeira review
x_train[[1]][1:12]

# Como ver as palavras? ----------------------------------------------

# dicionario de indices e palavras
word_index <- dataset_imdb_word_index()

# funcao que decodifica uma lista de indices em palavras
decode_review <- function(text, word_index) {
  word <- names(word_index)
  idx <- unlist(word_index, use.names = FALSE)
  word <- c("<PAD>", "<START>", "<UNK>", "<UNUSED>", word)
  idx <- c(0:3, idx + 3)
  words <- word[match(text, idx, 2)]
  paste(words, collapse = " ")
}

decode_review(x_train[[1]], word_index)
decode_review(x_train[[1]][1:18], word_index)


# one-hot-encoding --------------------------------------------------------

one_hot <- function(sequences, dimension) {
  seqlen <- sapply(sequences, length)
  n <- length(seqlen)
  rowind <- rep(1:n, seqlen)
  colind <- unlist(sequences)
  sparseMatrix(i = rowind, j = colind,
               dims = c(n, dimension))
}

# representa os dados de treino e teste com one-hot-enconding
x_train_1h <- one_hot(x_train, 10000)
x_test_1h <- one_hot(x_test, 10000)

dim(x_train_1h)

# proporcao de entradas nao-nulas
nnzero(x_train_1h) / (25000 * 10000)

# conjunto de validacao de tamanho 2000
set.seed(3)
id_val <- sample(seq_along(y_train), 2000)


# Ajuste rede neural ------------------------------------------------------


model <- keras_model_sequential() %>%
  layer_dense(units = 16, 
              activation = "relu",
              input_shape = c(10000)) %>%
  layer_dense(units = 16, 
              activation = "relu") %>%
  layer_dense(units = 1, 
              activation = "sigmoid")

model %>% 
  compile(optimizer = "rmsprop",
          loss = "binary_crossentropy", 
          metrics = c("accuracy"))

history <- model %>% 
  fit(x_train_1h[-id_val, ], 
      y_train[-id_val], 
      epochs = 20,
      batch_size = 512, 
      validation_data = list(x_train_1h[id_val, ], 
                             y_train[id_val]))

# predicoes
pred_1 <- model %>% 
  predict(x_test_1h) %>% `>` (0.5) %>% k_cast("int32") %>% 
  k_get_value() 

mean(abs(y_test == as.numeric(pred_1))) # acuracia


# RNN ---------------------------------------------------------------------

# contagem da quantidade de palavras por documento
word_counts <- sapply(x_train, length)

summary(word_counts)

# proporcao de documentos com menos de 500 palavras
sum(word_counts <= 500) / length(word_counts)

# a RNN requer que todos documentos tenham o mesmo tamanho
# vamos considerar 500
max_len <- 500

x_train <- pad_sequences(x_train, maxlen = max_len)
x_test <- pad_sequences(x_test, maxlen = max_len)

dim(x_train)
dim(x_test)

# primeiras 20 palavras do documento 1
x_train[1, 1:20]

# ultimas 20 palavras do documento 1
x_train[1, 481:500]

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, 
                  output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, 
              activation = "sigmoid")

model %>% 
  compile(optimizer = "rmsprop",
          loss = "binary_crossentropy", 
          metrics = c("acc"))

history <- model %>% 
  fit(x_train, y_train, epochs = 10,
      batch_size = 128, 
      validation_data = list(x_test, y_test))

pred_2 <- model %>% 
  predict(x_test_1h) %>% `>` (0.5) %>% k_cast("int32") %>% 
  k_get_value() 

table(pred = pred_2, obs = y_test)

mean(abs(y_test == as.numeric(pred_2)))


