Reconhecimento de atividades humanas: cenário 02
========================================================

O objetivo deste trabalho é construir um modelo capaz de identificar que atividade uma pessoa está realizando a partir de informações de acelerômetro e giroscópio presentes em um Samsung Galaxy II.

Aquisição dos dados
-------------------


```r
load("../data/samsungData.rda")
names(samsungData) <- gsub("\\)", "_", gsub("\\(", "_", gsub(",", "_", gsub("-", 
    "_", names(samsungData)))))
samsungData$activity <- as.factor(samsungData$activity)
```


Separação dos dados em treinamento e teste
------------------------------------------


```r
train <- subset(samsungData, samsungData$subject < 20)
test <- subset(samsungData, samsungData$subject > 20)
train$subject <- NULL
test$subject <- NULL
```


Criando o modelo
----------------


```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
model <- randomForest(activity ~ ., data = train, importance = TRUE, do.trace = 100)
```

```
## ntree      OOB      1      2      3      4      5      6
##   100:   2.17%  0.00%  3.31%  4.84%  1.41%  2.39%  1.01%
##   200:   1.79%  0.00%  3.31%  4.15%  1.41%  1.10%  0.50%
##   300:   1.72%  0.00%  3.01%  4.01%  1.13%  1.10%  0.84%
##   400:   1.59%  0.00%  2.26%  3.87%  1.41%  1.10%  0.67%
##   500:   1.51%  0.00%  2.26%  3.73%  1.41%  0.92%  0.50%
```

```r
model
```

```
## 
## Call:
##  randomForest(formula = activity ~ ., data = train, importance = TRUE,      do.trace = 100) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 23
## 
##         OOB estimate of  error rate: 1.51%
## Confusion matrix:
##          laying sitting standing walk walkdown walkup class.error
## laying      731       0        0    0        0      0    0.000000
## sitting       0     649       15    0        0      0    0.022590
## standing      0      27      696    0        0      0    0.037344
## walk          0       0        0  697        7      3    0.014144
## walkdown      0       0        0    5      538      0    0.009208
## walkup        0       0        0    1        2    594    0.005025
```


Validando o modelo no conjunto de teste
---------------------------------------


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
testPred <- predict(model, newdata = test)
t <- table(testPred, test$activity)
confusionMatrix(t)
```

```
## Confusion Matrix and Statistics
## 
##           
## testPred   laying sitting standing walk walkdown walkup
##   laying      676       0        0    0        0      0
##   sitting       0     578       70    0        0      0
##   standing      0      44      581    0        0     23
##   walk          0       0        0  471       14      9
##   walkdown      0       0        0    4      335      6
##   walkup        0       0        0   44       94    438
## 
## Overall Statistics
##                                         
##                Accuracy : 0.909         
##                  95% CI : (0.899, 0.919)
##     No Information Rate : 0.2           
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.89          
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: laying Class: sitting Class: standing
## Sensitivity                    1.0          0.929           0.892
## Specificity                    1.0          0.975           0.976
## Pos Pred Value                 1.0          0.892           0.897
## Neg Pred Value                 1.0          0.984           0.974
## Prevalence                     0.2          0.184           0.192
## Detection Rate                 0.2          0.171           0.172
## Detection Prevalence           0.2          0.191           0.191
## Balanced Accuracy              1.0          0.952           0.934
##                      Class: walk Class: walkdown Class: walkup
## Sensitivity                0.908          0.7562         0.920
## Specificity                0.992          0.9966         0.953
## Pos Pred Value             0.953          0.9710         0.760
## Neg Pred Value             0.983          0.9645         0.986
## Prevalence                 0.153          0.1308         0.141
## Detection Rate             0.139          0.0989         0.129
## Detection Prevalence       0.146          0.1019         0.170
## Balanced Accuracy          0.950          0.8764         0.936
```


