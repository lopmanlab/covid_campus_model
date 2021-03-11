## FIgure 3. Combines PSA with scenario analysis to explore outcomes over a sweep of screening intervals. No testing interventions. NPI effectiveness set at 35%

knitr::opts_chunk$set(echo = TRUE)
# Load dependencies, functions and parameters
source("99_dependencies.R")
source("99_model_func.R")
source("99_parm_init_control.R")
source("99_psa_optimizedistr.R")
source("99_psa_parm.R")   #Note this overwrites initial parameters from parm_init_control
source("99_psa_plot.R")
options(scipen=999)
options(digits=4)

## Scenario with seven day screening, two day testing, contact tracing as variable parameter


## Loop through screening scenarios
screen.int <- 1/seq(7,120,7)
screen.int.days <- seq(7,120,7)

screen_list<-list()                   #Initialize list to collect results from each screening interval
# Below loop runs model for each screening scenario
for (i in 1:length(screen.int)) {
  screen_list[[i]]<-model_scenarios(screening=screen.int[i],screening_on=screen.int[i],eff_npi.int=eff_npi.int)
  
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
screen_list_cases <- list()
for (i in 1:length(screen_list)){
  screen_list_cases[[i]] <- getcases(screen_list[[i]]) %>%
    mutate(scenario = rep(screen.int.days[i]))
}



screen_df <- bind_rows(screen_list_cases, .id = "column_label")

trans_df <- readRDS("tables/res_fig2_trans_df.RDS")
screen_df<- rbind(screen_df %>% filter(scenario %in% c(7,28, 119)),trans_df%>% filter(scenario == 0))

saveRDS(screen_df,"tables/res_fig2.RDS")
saveRDS(screen_list,"tables/res_fig2_rawmodeloutputs.RDS")


