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
                       ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,TARGET,ID)


#train[sapply(train, is.numeric)] <- lapply(train[sapply(train, is.numeric)], as.factor)

test <- data.frame(data_list2)

test <- na.aggregate(test)


test <- dplyr::select(test,STRTYP,PHARM5,CAR_PERF,TYP7,REGIOTYP,TYP9,TYP3,PHARM3,PHARM4,TYP6,TYP1,TYP5,BUYPOWER,
                      ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,ID)



#full <- bind_rows(train,test)

#full <- na.aggregate(full)

#full[sapply(full, is.numeric)] <- lapply(full[sapply(full, is.numeric)], as.factor)

#full <- dplyr::select(full,STRTYP,PHARM5,CAR_PERF,TYP7,REGIOTYP,TYP9,TYP3,PHARM3,PHARM4,TYP6,TYP1,TYP5,BUYPOWER,
                      #ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,TARGET,ID)


#Separar el train en dos modelos distintos, train_model ; train_modeltest
library(caTools)
set.seed(0)
split = sample.split(train$TARGET, SplitRatio = 0.85)
training_set = subset(train, split == TRUE) #separamos el train 
test_set = subset(train, split == FALSE)

library(e1071)
regressor_svr = svm(formula = training_set$TARGET ~ .,
                    data = training_set,
                    type = 'C-classification',
                    kernel = 'linear')

y_pred_svr = predict(regressor_svr,  newdata = test_set) #

confusionMatrix(factor(y_pred_svr), 
                factor(test_set$TARGET))

y_pred_svr <- ifelse (y_pred_svr > 0.5,1,0)

library(forecast)

Accuracy_log <- accuracy(test_set$TARGET,y_pred_svr)

Accuracy_log

F1_Score(y_pred_svr, test_set$TARGET)



###############################

prediction = predict(regressor_svr,  newdata = test)  #realizamos la prediccion con la base test.

Tabla <- data.frame(ID=test$ID)
Tabla$Canceller <- NA 

Tabla$Canceller <- ifelse (prediction > 0.5,1,0)

library(readr)

write_csv(Tabla, "svm_test1.csv")


csvnuevo <- read.csv("/Taller de Ciencia de Datos para MKT Digital/Tarea 4/svm_test1.csv")


####################

solution1 <- data.frame(PassengerID = test$ID, Cancellers = y_pred_svr_pred)



