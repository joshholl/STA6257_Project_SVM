# depending on where this is being ran, the default repo may or may not be set.
# since everything is from CRAN, this sets the repo to cran if one is not already configured

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})

# This is the list of packages our project uses.
project_dependencies <- c(
  "tidyverse",
  "ggthemes",
  "ggrepel",
  "dslabs",
  "ggplot2",
  "dplyr",
  "data.table",
  "extrafont",
  "dataMaid",
  "DataExplorer",
  "SmartEDA",
  "pander",
  "gt",
  "gtsummary",
  "mlr",
  "e1071",
  "caret",
  "caTools",
  "MLmetrics",
  "gtExtras",
  "pROC",
  "doSNOW",
  "doParallel",
  "foreach",
  "graphics"
)

# install and run easypackages to setup our dependencies
install.packages("easypackages")
library(easypackages)

# install all dependencies
easypackages::packages(project_dependencies, prompt = FALSE)
