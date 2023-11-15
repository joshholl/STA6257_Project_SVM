#| label: package-install
#| echo: false
library(easypackages)
dep_packages = c("tidyverse",
                 "ggthemes",
                 "ggrepel",
                 "dslabs", 
                 "ggplot2",
                 "dplyr",
                 "data.table",
                 "extrafont",
                 "dataMaid",
                 "gt",
                 "gtsummary",
                 "gtExtras",
                 "mlr",
                 "e1071",
                 "caret",
                 "caTools",
                 "MLmetrics",
                 "pROC"
                 
)
easypackages::libraries(dep_packages)

mimic_data <- fread("./MIMIC_ICU_Data/mimic_icu_data.csv")

#| label: main-model

# a random number based on keyboard bashing the number row to ensure that our random data set is repeatable
# while we are reporting on it
set.seed(01923904)

# Create a subset of the entire dataset to just include the rows we have determined are of interest
# convert the gender field to an `is_male` flag. Also discretize the heart rate and age
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
  mutate(
    is_male = factor(case_when(gender == "M" ~ 1, TRUE ~ 0)),
    age_range = factor(case_when(
      age <= 18 ~ "<=18",
      age > 18 & age <= 40 ~ "19 to 40",
      age > 40 & age <= 60 ~ "41 to 60",
      age > 60 ~ ">=61"
    )),
    heart_rate = factor(case_when(
      heartrate_mean < 60 ~ "<60",
      heartrate_mean >= 61 & heartrate_mean <= 80 ~ "60 to 80",
      heartrate_mean >= 81 & heartrate_mean <= 100 ~ "81 to 100",
      heartrate_mean > 100 ~ "above 100"
    )),
  ) %>%
  select(-gender, -heartrate_mean, -age)

model_data$id <- 1:nrow(model_data)
model_data$hospital_expire_flag <- as.factor(model_data$hospital_expire_flag)
model_data <- na.omit(model_data)


# use z score standardization on our features to see what we get
scaled_model_data <- model_data %>%  
  select(-id) %>%
  mutate_if(is.numeric, scale) %>%
  cbind(id = model_data$id)


# our data are skewed such that the patients tend to survive (hospital expire flag = 0), to resolve this issue we will downsample the surviving pateints and construct a training set based on an 80% selection of all of the hospital_expire_flag = 1, then randomly select and equal number of observations where hosptial expre flag=1

amount_to_sample = floor(sum(scaled_model_data$hospital_expire_flag == 1) * 0.95)


train <- scaled_model_data %>% group_by(hospital_expire_flag) %>% sample_n(size = amount_to_sample) %>% ungroup()
test <- scaled_model_data %>% anti_join(train, by="id") %>% select(-id)

train <- train %>% select(-id)

## Things to do
## possibliy do some weights
## Down or 
## bootstrapping with multiple data sets


# build the model
# nu and gamas will need to be set for tuning, they have a single value for now
# so we have something to show
kernels <- c("linear", "radial")
costs <- c(250,500,750,1000,1250)
gammas <- c(0.66)
nus <- c(0.25)
ranges <- list(cost=costs, gamma = gammas, kernel = kernels, nu=nus)


modelTuning <- tune(svm,
                    hospital_expire_flag ~ ., 
                    type="nu-classification",
                    data= train, 
                    probability=TRUE,
                    ranges = ranges,
                    scale = TRUE)

model <- modelTuning$best.model

print('---------- Primary Model --------------')
model_predict <- predict(model, test, probability = TRUE)
confusionMatrix(model_predict, test$hospital_expire_flag)

model_probabilities <- attr(model_predict, "probabilities")[, 1]

model_predictions <- ROCR::prediction(model_probabilities, test$hospital_expire_flag, label.ordering = c(0,1))
model_perf <- ROCR::performance(model_predictions, "tpr", "fpr")
plot(model_perf, colorize = TRUE)
abline(a=0, b=1)

summary(model)


print('---------- CHALLENGER --------------')
challengerTuning <- tune(svm,
                         hospital_expire_flag ~ age_range + heart_rate + sysbp_mean + resprate_mean + tempc_mean + platelet_min + is_male, 
                         data= train, 
                         type = "nu-classification",
                         probability=TRUE,
                         ranges = ranges,
                         scale = TRUE)

challenger <- challengerTuning$best.model
summary(challenger)


challenger_predict <- predict(challenger, test, probability = TRUE)
confusionMatrix(challenger_predict, test$hospital_expire_flag)
challenger_probabilities <- attr(challenger_predict, "probabilities")[, 1]
challenger_predictions <- ROCR::prediction(challenger_probabilities, test$hospital_expire_flag, label.ordering = c(0,1))
challenger_perf <- ROCR::performance(challenger_predictions, "tpr", "fpr")
plot(challenger_perf, colorize = TRUE)
abline(a=0,b=1)