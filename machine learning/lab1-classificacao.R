# Machine Learning - 2022-1 - Laboratório 01 
# Modelos de classificação: 
# Regressão Logística e árvores de decisão

# Carregando e organizando os dados ---------------------------------------
df <- read.csv("titanic.csv", stringsAsFactors = F)

dim(df)

View(df)

str(df)

summary(df)


df <- df[,c("Survived", "Pclass", "Sex", "Age", 
            "SibSp", "Parch", "Fare", "Embarked")]

summary(df)

df <- df[!is.na(df$Age) & df$Embarked!="",]

dim(df)

df$Survived <- factor(df$Survived)
df$Pclass <- factor(df$Pclass)
df$Sex <- factor(df$Sex)
df$Embarked <- factor(df$Embarked)


# Visualização dos dados --------------------------------------------------
plot(df$Age, df$Fare, 
     col = ifelse(df$Survived == "1", "darkblue", "darkred"), 
     pch = 19, cex = .6,
     xlab = "Idade", ylab = "Tarifa",
     main = "Sobreviveu ao naufrágio?")
legend("topleft", legend = c("Sim","Não"), 
       col = c("darkblue", "darkred"), pch = 19)


barplot(table(df$Survived, df$Pclass), 
        main = "Sobrevivencia entre as classes",
        xlab = "Classe", 
        ylab = "Número de sobreviventes",
        col = c("darkblue","darkred"),
        legend = c("Não","Sim"), 
        beside = TRUE, args.legend = list(x = "topleft"))


barplot(table(df$Survived, df$Embarked),
        main = "Sobreviveu ao naufrágio?",
        xlab = "Local de embarque", 
        ylab = "Número de sobreviventes",
        col = c("darkblue","darkred"),
        legend = c("Não","Sim"), 
        beside = TRUE, args.legend = list(x = "topleft"))

barplot(table(df$Survived, df$Sex),
        main = "Sobrevivencia entre os generos",
        xlab = "Genero", ylab="Número de sobreviventes",
        col = c("darkblue","darkred"),
        legend = c("Não","Sim"), 
        beside=TRUE, args.legend = list(x = "topleft"))

# Separação treino teste --------------------------------------------------
set.seed(1234)

idx <- sample(1:nrow(df), size = round(0.7 * nrow(df)))

training <- df[idx, ]
test <- df[-idx, ]


# Regressao Logistica -----------------------------------------------------
model <- glm(Survived ~ .,
             data = training,
             family = binomial)

summary(model)

prob <- predict(model, newdata = test, type = "response")

threshold <- 0.5

y_hat <- factor(ifelse(prob >= threshold, "1", "0"))

table(Predicted = y_hat, Observed = test$Survived)

library(pROC)

par(mfrow = c(1, 1))

plot.roc(test$Survived, prob, col = "blue", grid = TRUE,
         xlab = "FPR (1 -  Specificity)",
         ylab = "TPR (Sensitivity)",
         main = "ROC", legacy.axes = TRUE, asp = FALSE, las = 1,
         print.thres.pattern = " %.2f",
         print.thres = threshold)
         # print.thres = "best")

auc(test$Survived, prob)

resultados <- data.frame(modelo = character(), 
                         auc = numeric())

resultados <- rbind(resultados, 
                    data.frame(modelo = "Logistica", 
                               auc = auc(test$Survived, prob)))

# Arvore de classificacao -------------------------------------------------

library(tree)
library(pROC)

ctree <- tree(Survived ~ . , data = training)

summary(ctree)

plot(ctree, type = "uniform")
text(ctree, cex = 0.8)

cv <- cv.tree(ctree, FUN = prune.misclass)

plot(cv$size, cv$dev, type = "b", lwd = 2, col = "dark green",
     xlab = "Número de folhas",
     ylab = "Número de erros de classificação",
     main = "Validação cruzada em 10 lotes")

pruned <- prune.misclass(ctree, best = 6)

plot(pruned, type = "uniform")
text(pruned, cex = 0.8)

y_hat_pruned <- predict(pruned, test, type = "class")
mean(y_hat_pruned != test$Survived)

table(Predicted = y_hat_pruned, Observed = test$Survived)

prob_pruned <- predict(pruned, test, type = "vector")[, 2]

plot.roc(test$Survived, prob_pruned, col = "blue", grid = TRUE,
         xlab = "FPR (1 -  Specificity)",
         ylab = "TPR (Sensitivity)",
         main = "ROC", legacy.axes = TRUE, asp = FALSE,
         las = 1)

auc(test$Survived, prob_pruned)

resultados <- rbind(resultados,
                    data.frame(modelo="Arvore de class", auc=auc(test$Survived, prob_pruned)))


# Árvore de classificação com rpart ---------------------------------------
library(rpart)
library(rpart.plot)


rpart_tree <- rpart(Survived ~ . , data = df,
                    control = rpart.control(minsplit = 3))

?rpart.control

rpart.plot(rpart_tree)

?rpart.plot

rpart.plot(rpart_tree, extra=104, cex=0.5)

predict(rpart_tree, test)


# Resultados --------------------------------------------------------------
resultados$auc <- round(resultados$auc,3)

resultados[order(resultados$auc),]
