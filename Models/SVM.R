
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

mimic_icu_data$gender <- ifelse(mimic_icu_data$gender == "M", 1, 0)

mimic_data <- na.omit(mimic_icu_data)

train1 <- mimic_data[c('age', 
                  'gender', 
                  'hospital_expire_flag', 
                  'heartrate_mean',
                  'sysbp_mean',
                  'resprate_mean',
                  'tempc_mean',
                  'wbc_mean',
                  'platelet_min',
                  'creatinine_max',
                  'lactate_mean')]

Test <- mimic_data[c( 'age', 
              'gender', 
              'heartrate_mean',
              'sysbp_mean',
              'resprate_mean',
              'tempc_mean',
              'wbc_mean',
              'platelet_min',
              'creatinine_max',
              'lactate_mean')]

X <- train1
y <- train1$hospital_expire_flag
X <- X[, !(names(X) %in% c("hospital_expire_flag"))]
Xtest <- Test

set.seed(1)

trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

##svm <- svm(X_train, y_train, kernel = "linear", cost = 1, gamma = "auto", random.seed = 1)

svm <- svm(hospital_expire_flag ~ ., 
        data = train1 ,
        type = 'C-classification', 
        kernel = 'linear')

y_pred <- predict(svm, X_test)
preds <- predict(svm, X)
targs <- y
accuracy <- sum(preds == targs) / length(targs)
precision <- sum(preds == 1 & targs == 1) / sum(preds == 1)
recall <- sum(preds == 1 & targs == 1) / sum(targs == 1)
f1 <- 2 * precision * recall / (precision + recall)
auc <- roc(preds, targs)
train_preds <- preds

print(paste("accuracy: ", accuracy))
print(paste("precision: ", precision))
print(paste("recall: ", recall))
print(paste("f1: ", f1))
print(paste("area under curve (auc): ", auc))

cf_matrix <- table(y_test, y_pred)
print(cf_matrix)

y_train_pred <- predict(svm, X_train, decision.values = TRUE)
y_test_pred <- predict(svm, X_test, decision.values = TRUE)

##train_roc <- roc(y_train, y_train_pred)
##test_roc <- roc(y_test, y_test_pred)




plot(y_train, col = "blue", main = "AUC(ROC curve)")
lines(y_test, col = "red")
abline(0, 1, lty = 2, col = "green")
legend("bottomright", legend = c("AUC TRAIN =", auc(y_train_pred), "AUC TEST =", auc(y_test_pred)), col = c("blue", "red"), lty = 1)
xlabel("False Positive Rate")
ylabel("True Positive Rate")
title("AUC(ROC curve)")
grid(color = "black", linestyle = "-", linewidth = 0.5)





---------------
  
  
X <- train1
y <- train1$hospital_expire_flag
X <- X[, !(names(X) %in% c("hospital_expire_flag"))]
Xtest <- Test
set.seed(1)
train_test_split <- sample(1:nrow(X), size = floor(0.8*nrow(X)), replace = FALSE)
X_train <- X[train_test_split, ]
X_test <- X[-train_test_split, ]
y_train <- y[train_test_split]
y_test <- y[-train_test_split]

svm <- svm(X_train, y_train, kernel = "linear", cost = 1, gamma = "auto", random.seed = 1)
y_pred <- predict(svm, X_test)
preds <- predict(svm, X)
targs <- y
print("accuracy: ", accuracy(targs, preds))
print("precision: ", precision(targs, preds))
print("recall: ", recall(targs, preds))
print("f1: ", F1_Score(targs, preds))
print("area under curve (auc): ", auc(targs, preds))

train_preds <- preds
y_train_pred <- decision.values(svm, X_train)
y_test_pred <- decision.values(svm, X_test)
train_roc <- roc(y_train, y_train_pred)
test_roc <- roc(y_test, y_test_pred)

plot(y_train, col = "blue", main = "AUC(ROC curve)")
lines(y_test, col = "red")
abline(0, 1, lty = 2, col = "green")
legend("bottomright", legend = c("AUC TRAIN =", auc(y_train_pred), "AUC TEST =", auc(y_test_pred)), col = c("blue", "red"), lty = 1)
xlabel("False Positive Rate")
ylabel("True Positive Rate")
title("AUC(ROC curve)")
grid(color = "black", linestyle = "-", linewidth = 0.5)

