# Machine Learning - 2022-1 - Laboratório 02 
# Modelos de regressão: 
# Regressão Linear e árvore de decisão


# Carregar dados ----------------------------------------------------------
# install.packages("AmesHousing")
library(AmesHousing)

# descrição dos dados: 
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data

ames <- make_ames()
?make_ames


# Análise descritiva ------------------------------------------------------

dim(ames)

View(ames)

str(ames)

colSums(is.na(ames))

# options(scipen=999)
hist(ames$Sale_Price,
     main = "Sale Price",
     xlab = "Value (USD)",
     ylab = "Frequency")

library(ggplot2)
ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(alpha = 0.5) +
  labs(title = "Sale Price",
       x = "Value (USD)",
       y = "Frequency") +
  theme_bw()

# para essa análise, vamos considerar apenas algumas variáveis

# SalePrice - the property's sale price in dollars.
# LotArea: Lot size in square feet
# GrLivArea: Above grade (ground) living area square feet
# YearBuilt: Original construction date
# CentralAir: Central air conditioning
# LotShape: General shape of property
# OverallCond: Overall condition rating


df <- ames[, c("Sale_Price", "Lot_Area", "Gr_Liv_Area", 
                  "Year_Built", "Central_Air", "Lot_Shape",
                  "Overall_Cond")]

# Lot_Area
limite_lot_area <- 80000

plot(x = df$Lot_Area, 
     y = df$Sale_Price,
     xlab = "Lot Area", ylab = "Sale Price",
     xlim = c(0, limite_lot_area))

ggplot(df, aes(x = Lot_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.1) +
  xlim(0, limite_lot_area) +
  labs(x = "Lot Area", y = "Sale Price") +
  theme_bw()

sum(df$Lot_Area > limite_lot_area)

# Gr_Liv_Area
limite_Gr_Liv_Area <- 4000

plot(x = df$Gr_Liv_Area, 
     y = df$Sale_Price,
     xlab = "Above grade (ground) living area ", ylab = "Sale Price",
     xlim = c(0, limite_Gr_Liv_Area))

ggplot(df, aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.1) +
  xlim(0, limite_Gr_Liv_Area) +
  labs(x = "Above grade (ground) living area ", y = "Sale Price")

# Year_Built
plot(x = df$Year_Built, 
     y = df$Sale_Price,
     xlab = "Year built", ylab = "Sale Price")

ggplot(df, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.1) +
  labs(x = "Year built", y = "Sale Price")

# Central_Air 
boxplot(Sale_Price ~ Central_Air, data = df,
        xlab = "Central Air", ylab = "Sale Price")

ggplot(df, aes(x = Central_Air, y = Sale_Price)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Central Air", y = "Sale Price") +
  theme_bw()

# LotShape
boxplot(Sale_Price ~ Lot_Shape, data = df, horizontal = TRUE,
        las = 2,
        ylab = "Lot Shape", xlab = "Sale Price")

ggplot(df, aes(x = Lot_Shape, y = Sale_Price)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Lot Shape", y = "Sale Price") +
  coord_flip()


# OverallCond
boxplot(Sale_Price ~ Overall_Cond, data = df, horizontal = TRUE,
        las = 2,
        ylab = "Overall Condition", xlab = "Sale Price")

ggplot(df, aes(x = Overall_Cond, y = Sale_Price)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Overall Condition", y = "Sale Price") +
  coord_flip() +
  theme_bw()


# Separação dos dados em treino e teste -----------------------------------
set.seed(1)

idx <- sample(1:nrow(df), round(0.7 * nrow(df)))

training <- df[idx,]
test <- df[-idx,]

nrow(training)
nrow(test)



# Regressão linear --------------------------------------------------------

linear <- lm(Sale_Price ~ ., data = training)

summary(linear)

y_hat_linear <- predict(linear, newdata = test)

(RMSE_linear <- sqrt(mean((y_hat_linear - test$Sale_Price)^2)))


# Árvore de regressão -----------------------------------------------------

library(tree)

tree <- tree(Sale_Price ~ ., data = training)

summary(tree)

plot(tree, type = "uniform") 
text(tree, cex = 0.75)

y_hat_tree <- predict(tree, newdata = test)

(RMSE_tree <- sqrt(mean((y_hat_tree - test$Sale_Price)^2)))


# Árvore de regressão com rpart -------------------------------------------

library(rpart)
library(rpart.plot)


rpart_tree <- rpart(Sale_Price ~ ., data = df,
                    control = rpart.control(minsplit = 3))

rpart.plot(rpart_tree)

?rpart.plot

rpart.plot(rpart_tree, cex=0.55)

y_hat_rpart <- predict(rpart_tree, test)

# Tarefa:
# ajustar os mesmos modelos acima considerando mais variáveis