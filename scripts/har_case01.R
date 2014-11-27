
# aquisicao dos dados
dataset <- read.csv("data/dataset-har-PUC-Rio-ugulino.csv", sep=";")
dim(dataset)
sapply(dataset, class)
table(dataset$user)
table(dataset$class)
prop.table(table(dataset$class))
sum(is.na(dataset))

# divisao entre conjunto de treinamento e testes

library(caret)
set.seed(1234)
trainIndex <- createDataPartition(dataset$class, p = .6,
                                  list = FALSE,
                                  times = 1)
treinamento <- dataset[trainIndex,]
teste <- dataset[-trainIndex,]

# analise descritiva

prop.table(table(treinamento$class))
sapply(treinamento,summary)

par(mfrow=c(3,2))
with(treinamento[treinamento$class == 'sitting',], 
     hist(x1, xlim=c(-350,200), main='sitting', col='cyan'))
with(treinamento[treinamento$class == 'sittingdown',], 
     hist(x1, xlim=c(-350,200), main='sitting down', col='cyan'))
with(treinamento[treinamento$class == 'standing',], 
     hist(x1, xlim=c(-350,200), main='standing', col='cyan'))
with(treinamento[treinamento$class == 'standingup',], 
     hist(x1, xlim=c(-350,200), main='standing up', col='cyan'))
with(treinamento[treinamento$class == 'walking',], 
     hist(x1, xlim=c(-350,200), main='walking', col='cyan'))

par(mfrow=c(1,1))
plot(treinamento[, 7:9], pch = 21, 
     bg = c("red", "green","black","yellow","cyan")[treinamento$class],
     main="Distribuição das atividades levando-se em consideração dados do sensor 1")

# criando o modelo

library(randomForest)
formula <- class ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4
model <- randomForest(formula, data=treinamento, do.trace=100, importance=TRUE)
model

plot(model, lty = c(1, 1, 1, 1, 1, 1), 
     main = "Erro estimado baseado na quantidade de árvores utilizadas")
legend("top", c("OOB", "sitting", "sitting down", "standing", 
                "standing up", "walking"), lty = c(1, 1, 1, 1, 1, 1), 
       lwd = c(2.5,2.5, 2.5,2.5, 2.5, 2.5), 
       col = c("black", "red", "green", "yellow", "cyan", "purple"))

varImpPlot(model, 
           main = "Importância dos atributos ao classificar as observações")


# outro modelo utilizando apenas dados do acelerometro 1

formula1 <- class ~ x1 + y1 + z1
model1 <- randomForest(formula1, data=treinamento, do.trace=100, importance=TRUE)
model1

plot(model1, lty = c(1, 1, 1, 1, 1, 1), 
     main = "Erro estimado baseado na quantidade de árvores utilizadas")
legend("top", c("OOB", "sitting", "sitting down", "standing", 
                "standing up", "walking"), lty = c(1, 1, 1, 1, 1, 1), 
       lwd = c(2.5,2.5, 2.5,2.5, 2.5, 2.5), 
       col = c("black", "red", "green", "yellow", "cyan", "purple"))

# validando o modelo completo no conjunto de teste

testPred <- predict(model, newdata = teste)
t <- table(testPred, teste$class)
confusionMatrix(t)
