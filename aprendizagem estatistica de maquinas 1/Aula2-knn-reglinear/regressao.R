library(tidyverse)

advertising <- read.csv("../../../Dados/Advertising.csv") %>% 
  as_tibble() %>%
  select(-X)

head(advertising)


# Modelo linear considerando apenas TV ------------------------------------

# ajuste do modelo linear
fit1 <- lm(sales ~ TV, data = advertising)

fit1

summary(fit1)


# valor predito utilizando o modelo linear
y_pred <- predict(fit1, advertising)

# valor médio (independente do valor de TV)
y_bar  <- mean(advertising$sales)

RSS <- sum((advertising$sales - y_pred)^2)

TSS <- sum((advertising$sales - y_bar)^2)

1 - RSS/TSS  # R2 calculado

summary(fit1)$r.squared # R2 obtido pelo objeto com modelo linear



# R2 ajustado 

1 - (RSS/(nrow(advertising) - 1 - 1))/(TSS/(nrow(advertising) - 1)) # calculado

summary(fit1)$adj.r.squared # obtido pelo objeto com modelo linear


# -----------------------------------------------------------------------
# Caso tivesse que escolher apenas uma variável (TV, radio, newspaper), -
# qual escolheria para prever a variavel resposta (sales)? --------------
# -----------------------------------------------------------------------