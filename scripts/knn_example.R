dataset <- read.csv2("data/dataset-har-PUC-Rio-ugulino.csv")
dataset <- dataset[,7:19]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

dataset$x1 <- normalize(dataset$x1)
dataset$y1 <- normalize(dataset$y1)
dataset$z1 <- normalize(dataset$z1)
dataset$x2 <- normalize(dataset$x2)
dataset$y2 <- normalize(dataset$y2)
dataset$z2 <- normalize(dataset$z2)
dataset$x3 <- normalize(dataset$x3)
dataset$y3 <- normalize(dataset$y3)
dataset$z3 <- normalize(dataset$z3)
dataset$x4 <- normalize(dataset$x4)
dataset$y4 <- normalize(dataset$y4)
dataset$z4 <- normalize(dataset$z4)

set.seed(1234)
ind <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
train <- dataset[ind==1,]
test <- dataset[ind==2,]

library(class)

k_estimado = round(nrow(train)^(1/2))

predicted <- knn(train = train, test = test, cl = class, k=k_estimado)


