library(tidymodels)
library(skimr)
library(modeldata)
library(glmnet)

data(mlc_churn)

# Customer churn data
# Description
# A data set from the MLC++ machine learning software for modeling customer churn. There 
# are 19 predictors, mostly numeric: state (categorical), account_length area_code 
# international_plan (yes/no), voice_mail_plan (yes/no), number_vmail_messages 
# total_day_minutes total_day_calls total_day_charge total_eve_minutes total_eve_calls 
# total_eve_charge total_night_minutes total_night_calls total_night_charge 
# total_intl_minutes total_intl_calls total_intl_charge, and number_customer_service_calls.
# 
# Details
# The outcome is contained in a column called churn (also yes/no). A note in one of the 
# source files states that the data are "artificial based on claims similar to real world".


?mlc_churn 

mlc_churn %>% 
  group_by(churn) %>% 
  skim()

mlc_churn$state <- NULL

mlc_churn <- mlc_churn %>% 
  mutate(churn = factor(churn, levels = c("no", "yes")))

corte <- 0.50

# comparação dos modelos --------------------------------------------------

resultados <- tibble(modelo = c("logistica", "ridge", "lasso", "elastic-net"), 
                     acuracia = NA, 
                     auc = NA)

# treinamento x teste -----------------------------------------------------

set.seed(313)

idx <- sample(nrow(mlc_churn), size = .90*nrow(mlc_churn), replace = FALSE)


# logística ---------------------------------------------------------------

fit <- glm(churn ~ .,mlc_churn[idx,], family = "binomial")

summary(fit)

prob_logistica <- predict(fit, mlc_churn[-idx,], type = "response")

resultados$acuracia[resultados$modelo == "logistica"] <- mean(mlc_churn$churn[-idx] == ifelse(prob_logistica >= corte, "yes", "no"))

library(pROC)

resultados$auc[resultados$modelo == "logistica"] <- roc(mlc_churn$churn[-idx], prob_logistica)$auc





# prepara para glmnet -----------------------------------------------------

X_tr <- model.matrix(churn ~ ., mlc_churn)[idx, -1]
y_tr <- mlc_churn$churn[idx]

X_test <- model.matrix(churn ~ ., mlc_churn)[-idx,-1]
y_test <- mlc_churn$churn[-idx]


# ridge -------------------------------------------------------------------

ridge <- glmnet(X_tr, y_tr, alpha = 0, family = "binomial")

plot_glmnet(ridge)

# ridge$beta

cv_ridge <- cv.glmnet(X_tr, y_tr, alpha = 0, family = "binomial")

plot(cv_ridge)

lambda_ridge <- cv_ridge$lambda.1se

predict(ridge, newx = X_test, type = "response", s = lambda_ridge)

predict(ridge, newx = X_test, type = "class", s = lambda_ridge)

prob_ridge <- as.numeric(predict(ridge, newx = X_test, type = "response", s = lambda_ridge))

resultados$acuracia[resultados$modelo == "ridge"] <- mean(y_test == ifelse(prob_ridge >= corte, "yes", "no"))

resultados$auc[resultados$modelo == "ridge"] <- roc(mlc_churn$churn[-idx], prob_ridge)$auc


# lasso -------------------------------------------------------------------

lasso <- glmnet(X_tr, y_tr, alpha = 1, family = "binomial", nlambda = 1000)

plot_glmnet(lasso)

lasso$beta

cv_lasso <- cv.glmnet(X_tr, y_tr, alpha = 1, family = "binomial", lambda = lasso$lambda)

plot(cv_lasso)

lambda_lasso <- cv_lasso$lambda.1se

prob_lasso <- as.numeric(predict(lasso, newx = X_test, type = "response", s = lambda_lasso))

resultados$acuracia[resultados$modelo == "lasso"] <- mean(y_test == ifelse(prob_lasso > corte, "yes", "no"))

resultados$auc[resultados$modelo == "lasso"] <-roc(mlc_churn$churn[-idx], prob_lasso)$auc





# elastic-net -------------------------------------------------------------

elastic <- glmnet(X_tr, y_tr, alpha = 0.50, family = "binomial", nlambda = 1000)

plot_glmnet(elastic)

elastic$beta

cv_elastic <- cv.glmnet(X_tr, y_tr, alpha = 0.50, family = "binomial", lambda = lasso$elastic)

plot(cv_elastic)

lambda_elastic <- cv_elastic$lambda.1se

prob_elastic <- as.numeric(predict(elastic, newx = X_test, type = "response", s = lambda_elastic))

resultados$acuracia[resultados$modelo == "elastic-net"] <- mean(y_test == ifelse(prob_elastic > corte, "yes", "no"))

resultados$auc[resultados$modelo == "elastic-net"] <-roc(mlc_churn$churn[-idx], prob_elastic)$auc

resultados

