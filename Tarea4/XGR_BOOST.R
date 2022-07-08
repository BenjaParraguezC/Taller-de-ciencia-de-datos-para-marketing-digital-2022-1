library("caret")
library(ggplot2)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caretEnsemble)
library(doParallel)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(rio)

data_list <- read_xls("~/Taller de Ciencia de Datos para MKT Digital/Tarea 4/Train_Taller4.xls", col_names =TRUE)

data_list2 <- read_excel("~/Taller de Ciencia de Datos para MKT Digital/Tarea 4/Test_Taller4.xlsx")

train <- data.frame(data_list)

train <- na.aggregate(train)

train <- dplyr::select(train,STRTYP,PHARM5,CAR_PERF,TYP7,REGIOTYP,TYP9,TYP3,PHARM3,PHARM4,TYP6,TYP1,TYP5,BUYPOWER,
                       ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,TARGET)

str(train)


#train[sapply(train, is.numeric)] <- lapply(train[sapply(train, is.numeric)], as.factor)

test <- data.frame(data_list2)

test <- na.aggregate(test)


test <- dplyr::select(test,STRTYP,PHARM5,CAR_PERF,TYP7,REGIOTYP,TYP9,TYP3,PHARM3,PHARM4,TYP6,TYP1,TYP5,BUYPOWER,
                      ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,ID)




library(caTools)
set.seed(1)
split = sample.split(train$TARGET, SplitRatio = 0.90)
training_set = subset(train, split == TRUE) #separamos el train 
test_set = subset(train, split == FALSE)

library(xgboost)
classifier <-  xgboost(data = as.matrix(training_set[-21]), 
                     label = training_set$TARGET,
                     nrounds = 40,
                     max_depth = 1,
                     sub_sample = 1,
                     colsample_bytree = 1,
                     maximize=TRUE,
                     gamma = 0,
                     min_child_weight = 1,
                     booster = 'gbtree',
                     verbose = 1)

xgboost <- predict(classifier, newdata = as.matrix(test_set[-21]))

xgboost <- as.numeric(xgboost >0.5)

confusionMatrix(factor(xgboost), 
                factor(test_set$TARGET))
#-------------------------------------------------------------

Tabla <- data.frame(ID=test$ID)
Tabla$Canceller <- NA 


test <- select(test,-ID)

predictionFinal <-  predict(classifier,  newdata = as.matrix(test))  #realizamos la prediccion con la base test.



Tabla$Canceller <- ifelse (predictionFinal > 0.5,1,0)


Tabla <- as.data.frame(Tabla)

library(readr)

write_csv(Tabla, "Xgboost_Resultados2.csv")
