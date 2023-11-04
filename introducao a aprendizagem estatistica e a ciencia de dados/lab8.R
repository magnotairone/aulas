# Redes neurais covolucionais
# Ajuste dos dados CIFAR100

library(keras) # rede neural
library(jpeg) # visualizar imagem
library(Metrics) # calculo de metricas
library(Matrix) # representacao de matriz esparca

# Dados CIFAR100 ----------------------------------------------------------

# carregar os dados
cifar100 <- dataset_cifar100()
names(cifar100)

# dados de treino e teste
x_train <- cifar100$train$x
g_train <- cifar100$train$y

x_test <- cifar100$test$x
g_test <- cifar100$test$y

# dimensao dos dados de treinamento
dim(x_train)
# 5000 imagens de 32 por 32 pixels em tres canais (RGB)

# valores no canal G da primeira imagem
range(x_train[1, , , 2])

# vamos padronizar os valores observados
x_train <- x_train / 255
x_test <- x_test / 255

# one-hot enconding para as categorias da variavel resposta
#(classe de cada imagem)
y_train <- to_categorical(g_train, 100)

dim(y_train) # uma matriz com 50000 linhas e 100 colunas

View(y_train)

# dicionario de labels das imagens
# fonte: https://gist.github.com/adam-dziedzic/4322df7fc26a1e75bee3b355b10e30bc
labels <- c('apple',  # id 0
            'aquarium_fish',
            'baby',
            'bear',
            'beaver',
            'bed',
            'bee',
            'beetle',
            'bicycle',
            'bottle',
            'bowl',
            'boy',
            'bridge',
            'bus',
            'butterfly',
            'camel',
            'can',
            'castle',
            'caterpillar',
            'cattle',
            'chair',
            'chimpanzee',
            'clock',
            'cloud',
            'cockroach',
            'couch',
            'crab',
            'crocodile',
            'cup',
            'dinosaur',
            'dolphin',
            'elephant',
            'flatfish',
            'forest',
            'fox',
            'girl',
            'hamster',
            'house',
            'kangaroo',
            'computer_keyboard',
            'lamp',
            'lawn_mower',
            'leopard',
            'lion',
            'lizard',
            'lobster',
            'man',
            'maple_tree',
            'motorcycle',
            'mountain',
            'mouse',
            'mushroom',
            'oak_tree',
            'orange',
            'orchid',
            'otter',
            'palm_tree',
            'pear',
            'pickup_truck',
            'pine_tree',
            'plain',
            'plate',
            'poppy',
            'porcupine',
            'possum',
            'rabbit',
            'raccoon',
            'ray',
            'road',
            'rocket',
            'rose',
            'sea',
            'seal',
            'shark',
            'shrew',
            'skunk',
            'skyscraper',
            'snail',
            'snake',
            'spider',
            'squirrel',
            'streetcar',
            'sunflower',
            'sweet_pepper',
            'table',
            'tank',
            'telephone',
            'television',
            'tiger',
            'tractor',
            'train',
            'trout',
            'tulip',
            'turtle',
            'wardrobe',
            'whale',
            'willow_tree',
            'wolf',
            'woman',
            'worm')

set.seed(1)
# definicao da grade de imagens no grafico
par(mar = c(0, 0, 0, 0), mfrow = c(5, 5))

# selecao aleatoria de 25 imagens
index <- sample(seq(50000), 25)
for(i in index)
  plot(as.raster(x_train[i,,, ]))

# label numerico
g_train[index]

# label texto
labels[g_train[index]+1]

#3 definicao da estrutura da rede neural
# semelhante a apresentada na figura 10.8 do ISLR
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, # numero de filtros
                kernel_size = c(3, 3), # dimensao do filtro
                padding = "same", # padding
                activation = "relu", # funcao de ativacao
                input_shape = c(32, 32, 3) # camada de entrada
                ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% # camada de pool
  layer_conv_2d(filters = 64,
                kernel_size = c(3, 3),
                padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128,
                kernel_size = c(3, 3),
                padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 256,
                kernel_size = c(3, 3),
                padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>% # camada flatten
  layer_dropout(rate = 0.5) %>% # definicao do dropout
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 100, activation = "softmax")


summary(model)

# definicao do estimador da rede neural
model %>%
  compile(loss = "categorical_crossentropy",
          optimizer = optimizer_rmsprop(),
          metrics = c("accuracy"))

# estimacao
# demora alguns minutos para rodar
history <- model %>%
  fit(x_train, y_train, epochs = 10,
      batch_size = 128, validation_split = 0.2)





# predicao
pred_nn <- model %>%
  predict(x_test) %>%
  k_argmax()

# avaliacao no conjunto de teste
evaluate(model, x_test, to_categorical(g_test, 100))


model %>% get_weights()

# https://machinelearningmastery.com/how-to-visualize-filters-and-feature-maps-in-convolutional-neural-networks/
