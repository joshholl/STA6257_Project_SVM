#
# The purpose of this file is to do some 'grid search' model tuning.
# we know that we have a binary classification so our best types of SVM will be
# a c-classification or a nu-classification. What we want to find is the model
# that produces the best AUC when the cost, nu, gamma, and kernel are changed.
# In this case we don't care about the model produced just its AUC performance.
# the values found will be used in the primary model for the report.
#
# Results:
# gamma = 0.17
# nu    = 0.87
# auc   = 0.7155


library(easypackages)
dep_packages = c(
  "tidyverse",
  "dplyr",
  "data.table",
  "e1071",
  "caret",
  "pROC",
  "doSNOW",
  "doParallel",
  "foreach",
  "rlecuyer"
)
easypackages::libraries(dep_packages)

# load all the data from the main file
mimic_data <- fread("./MIMIC_ICU_Data/mimic_icu_data.csv")

# Create a subset of the entire data set to just include the rows we have determined are of interest
# convert the gender field to an `is_male` flag. Also discretize the heart rate and age
model_data <- mimic_data %>%
  select(
    icustay_id,
    age,
    gender,
    hospital_expire_flag,
    heartrate_mean,
    sysbp_mean,
    resprate_mean,
    tempc_mean,
    platelet_min
  ) %>%
  mutate(
    is_male = factor(case_when(gender == "M" ~ 1, TRUE ~ 0)),
    age_range = factor(
      case_when(
        age <= 18 ~ "<=18",
        age > 18 & age <= 40 ~ "19 to 40",
        age > 40 & age <= 60 ~ "41 to 60",
        age > 60 ~ ">=61"
      )
    ),
    heart_rate = factor(
      case_when(
        heartrate_mean < 60 ~ "<60",
        heartrate_mean >= 61 & heartrate_mean <= 80 ~ "60 to 80",
        heartrate_mean >= 81 & heartrate_mean <= 100 ~ "81 to 100",
        heartrate_mean > 100 ~ "above 100"
      )
    ),
    survival = factor(case_when(
      hospital_expire_flag == 1 ~ "DIED", TRUE ~ "SURVIVED"
    ))
  ) %>%
  select(-gender,-heartrate_mean,-age,-hospital_expire_flag) %>%
  drop_na()


# use z score standardization on our features to see what we get
scaled_model_data <- model_data %>%
  select(-icustay_id) %>%
  mutate_if(is.numeric, scale) %>%
  cbind(icustay_id = model_data$icustay_id)


# set a random seed so that this is repeatable.
randomSeed <- 01923904
set.seed(randomSeed)

# our data are imbalanced such that the patients tend to survive (hospital expire flag = 0), to resolve this issue we will downsample 
# the surviving patients and construct a training set based on an 80% selection of all of the survival == "DIED"
# then randomly select and equal number of observations where survival == "DIED"
amount_to_sample = floor(sum(scaled_model_data$survival == "DIED") * 0.95)
train <-
  scaled_model_data %>% group_by(survival) %>% sample_n(size = amount_to_sample) %>% ungroup()
test <- scaled_model_data %>% anti_join(train, by = "icustay_id")


# setup the search grid with a collection of potential values for gamma and nu
# each will increment by a step of 0.01. gammas are between 0.01 and 1 inclusive and
# nu values are between 0.01 and 0.99 inclusive
potentialGammas <- seq(from = 0.01, to = 1, by = 0.01)
potentialNus <- seq(from = 0.01, to = 0.99, by = 0.01)
searchGrid <- expand.grid(gamma = potentialGammas, nu = potentialNus)



# This search operation is very very slow if done sequentially or with the e1071 package's tune function
# for this to work better, we want to run the commands in parallel. since each iteration occurs in a new 
# rsession, use clusterSetupRNG to ensure that our seed is used for every run
nuParallelCluster <- makeCluster(detectCores() - 1)
clusterSetupRNG(nuParallelCluster, seed = randomSeed)
registerDoSNOW(nuParallelCluster)

#since this is meant to be ran standalone, go a head an setup a progress bar
prog_bar <- txtProgressBar(max=nrow(searchGrid), style=3)
progress <- function(n) setTxtProgressBar(prog_bar, n)
snow_opts <- list(progress = progress)


#execute the tuning search
tuneResults <- foreach(i = 1:nrow(searchGrid), .combine = rbind, .options.snow = snow_opts) %dopar% {
  tuneModel <- e1071::svm(survival ~ . - icustay_id, 
    type = "nu-classification", kernel = "radial",
    data = train,
    probability = TRUE,
    gamma = searchGrid[i,]$gamma,
    nu = searchGrid[i,]$nu,
    scale = TRUE)

  tunePredictions <- predict(tuneModel, test, probability = TRUE)
  tuneProbabilities <- attr(tunePredictions, "probabilities")[,1]
  tuneROC <- pROC::roc(as.factor(test$survival), tuneProbabilities)

  return(list(gamma = searchGrid[i, ]$gamma, 
    nu = searchGrid[i,]$nu, 
    auc = as.numeric(tuneROC$auc)))
}


#clean up the cluster
stopCluster(nuParallelCluster)

#run the SVM again to ensure that the random context was appropriately set for each run the parallel processing
bestTuneResult <- tuneResults[which.max(tuneResults[,"auc"]),]

#reset the random seed just in case
set.seed(randomSeed)

#validate the model
validationModel <- e1071::svm(survival ~ . - icustay_id,
                      type="nu-classification",
                      kernel="radial",
                      data=train,
                      probability=TRUE,
                      gamma=bestTuneResult$gamma,
                      nu=bestTuneResult$nu,
                      scale=TRUE)

validationPredictions <- predict(validationModel, test, probability=TRUE)
validationProbabilities <- attr(validationPredictions, "probabilities")[,1]
validationROC <- pROC::roc(as.factor(test$survival), validationProbabilities)

cat(
  paste('-----------------------------'),
  paste("gamma =", bestTuneResult$gamma),
  paste("nu    =", bestTuneResult$nu),
  paste('-----------------------------'),
  paste("g-search =", bestTuneResult$auc),
  paste("validate =", validationROC$auc),
  sep = '\n'
)


