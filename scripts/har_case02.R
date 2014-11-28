
# aquisicao dos dados

load("data/samsungData.rda")
names(samsungData) <- gsub("\\)","_",
                        gsub("\\(","_",
                          gsub(",","_",
                            gsub("-","_",names(samsungData)))))
samsungData$activity <- as.factor(samsungData$activity)

# separacao dos dados em treinamento e teste

train <- subset(samsungData, samsungData$subject < 20)
test <- subset(samsungData, samsungData$subject > 20)
train$subject <- NULL
test$subject <- NULL

# criando o modelo com os atributos selecionados

library(randomForest)
model <- randomForest(activity ~ ., data=train, importance=TRUE, do.trace=100)
model

# validando o modelo completo no conjunto de teste

library(caret)
testPred <- predict(model, newdata = test)
t <- table(testPred, test$activity)
confusionMatrix(t)

