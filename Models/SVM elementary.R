
install.packages("mlr")
install.packages("e1071")
install.packages("caret")
install.packages('caTools') 
install.packages('MLmetrics')

library(caret)
library(e1071)
library(mlr)
library(caTools) 
library(MLmetrics)
library(pROC)


data = mimic_icu_data

str(data)



data1 <- data[c('age', 
                'gender', 
                'heartrate_mean',
                'sysbp_mean',
                'resprate_mean',
                'tempc_mean',
                'wbc_mean',
                'platelet_min',
                'creatinine_max',
                'lactate_mean',
                'hospital_expire_flag')]

head(data1)


# Splitting the dataset into the Training set and Test set 

set.seed(123) 

split = sample.split(data1$hospital_expire_flag, SplitRatio = 0.80) 

training_set = subset(data1, split == TRUE) 
test_set = subset(data1, split == FALSE) 

str(training_set)
str(test_set)


# Fitting SVM to the Training set 

classifier = svm(as.factor(hospital_expire_flag) ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 

print(classifier)

# Predicting the Test set results 
y_pred = predict(classifier, type = 'r', newdata = test_set) 

print(y_pred)


# F1 Score 

F1_Score(y_pred,test_set$hospital_expire_flag)

#Confusion Matrix

test_set1 <- test_set$hospital_expire_flag

cm= confusionMatrix(factor(test_set1)['actual labels'],y_pred['predicted labels'])

print(cm)

print(y_pred)

