package_list <- c(
  "tidyverse",
  "viridis",
  "deSolve",
  "reshape2",
  "ggplot2",
  "ggpubr",
  "dplyr",
  "gridExtra",
  "gt",
  "tidyverse",
  "lhs",
  "sensitivity",
  "boot",
  "prevalence",
  "kableExtra",
  "patchwork",
  "here"
)



#install dependencies
if(F){
  install.packages(package_list)
}

lapply(package_list, function(x) library(x, character.only = T))

rm(package_list)