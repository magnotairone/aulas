library(MASS)
library(ISLR)
library(tidyverse)
library(vip)
library(patchwork)

data(Credit)


# Analise exploratoria ----------------------------------------------------

Credit %>% 
  ggplot(aes(x=Income, y=Balance, color=Married))+
  geom_point()

Credit %>% 
  group_by(Student) %>% 
  summarise(mean_balance = mean(Balance))
  

# completar

# modelo nulo -------------------------------------------------------------

fit <- lm(Balance ~ 1, data = Credit[,-1])

mean(Credit$Balance)

# forward -----------------------------------------------------------------

stepAIC(fit, direction = "forward", 
        scope = list(lower = ~ 1, #o minimo de variaveis para ter (no caso, o modelo nulo. poderia ser o Income por exemplo)
                     upper = ~ Income + Limit + Rating + Cards + Age + #o maior modelo a ser considerado
                       Education + Gender + Student + Married + 
                       Ethnicity))


# backward ----------------------------------------------------------------

fit <- lm(Balance ~ ., data = Credit[,-1])

stepAIC(fit, direction = "backward")

# both --------------------------------------------------------------------

fit <- lm(Balance ~ ., data = Credit[,-1])

stepAIC(fit, direction = "both")

# variable importance -----------------------------------------------------

fit1 <- lm(Balance ~ ., data = Credit[, -1])

summary(fit1)

fit2 <- lm(Balance ~ Rating + Cards + Age + 
             Student, data = Credit[, -1])

g1 <- vip(fit1, mapping = aes(fill = Sign)) + labs(title = "Full")

g2 <- vip(fit2, mapping = aes(fill = Sign)) + labs(title = "Both")

g1 + g2 + plot_layout(guides = "collect")
