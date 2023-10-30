# a random number based on keyboard bashing the number row to ensure that our random data set is repeatable 
# while we are reporting on it
set.seed(01923904)

# Create a subset of the entire dataset to just include the rows we have determined are of interest
# convert the gender field to an `is_male` flag
model_data <- mimic_data %>%
  select(
    age,
    gender,
    hospital_expire_flag,
    heartrate_mean,
    sysbp_mean,
    resprate_mean,
    tempc_mean,
    wbc_mean,
    platelet_min,
    creatinine_max,
    lactate_mean
  ) %>%
  mutate(is_male = case_when(gender == "M" ~ 1, TRUE ~ 0))

# drop all observations with an NA for any value
model_data <- na.omit(model_data)

# ensure that hospital expire flag is a factor
model_data$hospital_expire_flag <- as.factor(model_data$hospital_expire_flag)

# assign an id for each element in the row
model_data$id <- 1:nrow(model_data)

# Divide our dataset into a training and test set with 80% of data going to training and 20% to test
train <- model_data %>% dplyr::sample_frac(0.8)
test  <- dplyr::anti_join(model_data, train, by = 'id')

#Tune the model based on a list of costs ranging from 0.001 -> 100 by 10x steps 
modelTuning.out <- tune(svm, 
                        hospital_expire_flag ~ ., data = train, 
                        kernel = "linear", 
                        ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)), 
                        scale = TRUE)

# take the best model from the tuning and use it
model <- modelTuning.out$best.model
summary(model)

# run predictions based on our model against the test set
predictions <- predict(model, test)

# Construct a confusion matrix and print it out
confusionMatrix(table(actual=test$hospital_expire_flag, prediction=predictions), positive="1")