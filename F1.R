
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

data1 <- subset( data, select = -c(3,4,5,6,8,10,11,14,16 ))




# Splitting the dataset into the Training set and Test set 

set.seed(123) 

split = sample.split(data1$hospital_expire_flag, SplitRatio = 0.80) 

training_set = subset(data1, split == TRUE) 
test_set = subset(data1, split == FALSE) 

str(training_set)
str(test_set)


# Fitting SVM to the Training set 

classifier = svm(as.factor(hospital_expire_flag) ~ ., 
                 data = training_set[,3:15], 
                 type = 'C-classification', 
                 kernel = 'linear') 

print(classifier)

# Predicting the Test set results 
y_pred = predict(classifier, type = 'r', newdata = test_set[,3:15]) 

print(y_pred)


# F1 Score 

F1_Score(y_pred,test_set$hospital_expire_flag)

#Confusion Matrix

test_set1 <- test_set$hospital_expire_flag

cm= confusionMatrix(factor(test_set1)['actual labels'],y_pred['predicted labels'])

print(cm)

print(y_pred)

