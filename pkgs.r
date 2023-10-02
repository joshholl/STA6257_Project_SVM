

packages <- c("tidyverse", "ggthemes","ggrepel","dslabs")
install.packages(setdiff(packages, rownames(installed.packages())),repos = "http://cran.us.r-project.org" )