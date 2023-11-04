# instalar pacotes

# install.packages("tidyverse")
# install.packages("ggplot2")

# carregar pacotes

library(tidyverse)
library(ggplot2)

# carregar dados

dados <- read_csv("Advertising.csv")

# visualizar dados em forma tabular

View(dados)

# selecionar colunas

dados <- dados %>% 
  select(-X1)

# gráfico de dispersão

dados %>% 
  ggplot(aes(x=TV, y=sales)) +
  geom_point(col="darkred")+
  labs(y="Sales", x="TV")

dados %>% 
  ggplot(aes(x=radio, y=sales)) +
  geom_point(col="darkred")+
  labs(y="Sales", x="Radio")

dados %>% 
  ggplot(aes(x=newspaper, y=sales)) +
  geom_point(col="darkred")+
  labs(y="Sales", x="Newspaper")


# filtrar vendas maiores que 20

dados %>% 
  filter(sales > 20)


# ordenar vendas de maneira decrescente

dados %>% 
  arrange(desc(sales))

# ajuste de um modelo linear

fit_lm <- lm(sales ~ TV + radio + newspaper, data=dados)

summary(fit_lm)

# predicao
pred <- predict(fit_lm)

# avaliar predicao
pred - dados$sales

sum(pred - dados$sales)

data.frame(real = dados$sales, pred = pred) %>% 
  ggplot(aes(real, pred))+
  geom_point()+
  geom_abline(slope=1, intercept=0, col="red", size=2)+
  labs(x = "Observado", y="Predito")

sum(abs(pred - dados$sales))

sum((pred - dados$sales)^2)
