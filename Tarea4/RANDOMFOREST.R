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

#base de datos train

data_list <- read_xls("~/Taller de Ciencia de Datos para MKT Digital/Tarea 4/Train_Taller4.xls", col_names =TRUE)

train <- data.frame(data_list)

train <- na.aggregate(train)

train <- dplyr::select(train,STRTYP,PHARM5,CAR_PERF,TYP7,REGIOTYP,TYP9,TYP3,PHARM3,PHARM4,TYP6,TYP1,TYP5,BUYPOWER,
                       ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,TARGET,ID)



#train[sapply(train, is.numeric)] <- lapply(train[sapply(train, is.numeric)], as.factor)

#Base de datos test

data_list2 <- read_excel("~/Taller de Ciencia de Datos para MKT Digital/Tarea 4/Test_Taller4.xlsx")

test <- data.frame(data_list2)

test <- na.aggregate(test)


test <- dplyr::select(test,STRTYP,PHARM5,CAR_PERF,TYP7,REGIOTYP,TYP9,TYP3,PHARM3,PHARM4,TYP6,TYP1,TYP5,BUYPOWER,
                      ANZGEW,ANZHH,PROB_AUT,PHARM2,CASATYP,VAL_TIER,YEAR_STA,ID)

#test[sapply(test, is.numeric)] <- lapply(test[sapply(train, is.numeric)], as.factor)

library(caTools)
set.seed(1)
split = sample.split(train$TARGET, SplitRatio = 0.85)
training_set = subset(train, split == TRUE) #separamos el train 
test_set = subset(train, split == FALSE)

library(randomForest)

rf_model <- randomForest(factor(TARGET) ~STRTYP+PHARM5+CAR_PERF+TYP7+REGIOTYP+TYP9+TYP3+PHARM3+PHARM4+TYP6+TYP1+TYP5+BUYPOWER+
                         ANZGEW+ANZHH+PROB_AUT+PHARM2+CASATYP+VAL_TIER+YEAR_STA+ID,
                         data = training_set,
                         ntree = 2000,
                         nodesize = 4)

y_predrfmodel = predict(rf_model, test_set)


confusionMatrix(factor(y_predrfmodel), 
                factor(test_set$TARGET),
                mode = "everything")

#prueba1 <- as.data.frame(y_predrfmodel)

y_predrfmodel <- ifelse (y_predrfmodel > 0.5,1,0)

library(forecast)

prediction = predict(y_predrfmodel,  newdata = test)


?xgboost
cat("making predictions in batches due to 8GB memory limitation\n")
submission <- data.frame(ID=test$ID)
submission$TARGET <- NA 

submission <- as.data.frame(submission)

write_csv(submission, "xgboost_submission10.csv")


csvnuevo <- read.csv("/Taller de Ciencia de Datos para MKT Digital/Tarea 4/xgboost_submission10.csv")
