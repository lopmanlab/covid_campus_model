package_list <- c(
  "EpiModel",
  "tidyverse",
  "viridis",
  "deSolve",
  "reshape2",
  "ggplot2",
  "ggpubr",
  "dplyr",
  "gridExtra",
  "tidyverse",
  "lhs",
  "sensitivity",
  "boot",
  "prevalence",
  "kableExtra"
)



#install dependencies
if(F){
  install.packages(package_list)
}

lapply(package_list, function(x) library(x, character.only = T))

rm(package_list)