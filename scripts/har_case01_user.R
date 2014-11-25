# aquisicao dos dados
dataset <- read.csv("data/dataset-har-PUC-Rio-ugulino.csv", sep=";")
dim(dataset)
table(dataset$user)
prop.table(table(dataset$user))


library(caret)
set.seed(1234)
trainIndex <- createDataPartition(dataset$user, p = .6,
                                  list = FALSE,
                                  times = 1)
treinamento <- dataset[trainIndex,]
teste <- dataset[-trainIndex,]

library(randomForest)
formula <- user ~ x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4
model <- randomForest(formula, data=treinamento, do.trace=100, importance=TRUE)
model

plot(model, lty = c(1, 1, 1, 1, 1), 
     main = "Erro estimado baseado na quantidade de árvores utilizadas")
legend("top", c("OOB", "debora", "jose carlos", "katia", 
                "wallace"), lty = c(1, 1, 1, 1, 1), 
       lwd = c(2.5,2.5, 2.5,2.5, 2.5), 
       col = c("black", "red", "green", "yellow", "cyan"))

varImpPlot(model, 
           main = "Importância dos atributos ao classificar as observações")

# validando o modelo completo no conjunto de teste

testPred <- predict(model, newdata = teste)
t <- table(testPred, teste$user)
confusionMatrix(t)
