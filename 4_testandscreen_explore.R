## FIgure 5 rerun
## Combine PSA with scenario analysis to explore the effects of screening and testing across different proportions of contacts traced

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

# Testing only scenarios (default contact tracing), need to also specify both testing and sensitivty along with it
test.int <- 1/c(2,4,7)
contacts.reached <- seq(0, 1, 0.5)
screen.int <- 1/seq(7, 120, 7)
p <- expand.grid(test.int = test.int, contacts.reached = contacts.reached,
                 screen.int = screen.int) %>%
    mutate(test = rep(c(2,4,7),times=51),
           screen = rep(seq(7, 120, 7),each=9))


#sensitivity_scen <-list(sensitivity_2.int,sensitivity.int,sensitivity_7.int)

pal <- brewer_ramp(length(test.int), "Spectral")


test_list<-list()
# Below loop runs model and then extracts median of active and cumulative student cases and active and cumulative staff cases

for (i in 1:nrow(p)) {
  test_list[[i]]<-model_scenarios(testing=p$test.int[i], screening = p$screen.int[i],screening_on = p$screen.int[i],p_contacts_reached = p$contacts.reached[i],eff_npi.int = eff_npi.int)
  print(paste("finished scenario",i, "out of 153"))
}

test_list_cases <- list()

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
for (i in 1:length(test_list)){
  test_list_cases[[i]] <- getcases(test_list[[i]]) %>%
    mutate(contacts = rep(p$contacts.reached[i]),
           screen = rep(p$screen[i]),
           test = rep(p$test[i])) 
}


##Need to filter on last time
test_trace_df <- data.frame(matrix(0, ncol = 8, nrow = nrow(p)))
colnames(test_trace_df) <- colnames(test_list_cases[[1]])

for (i in 1:length(test_list_cases)){
  test_trace_df[i,] <- test_list_cases[[i]] %>% filter(time == 116)
}


saveRDS(test_trace_df, "tables/res_fig5_test_trace_df.RDS")
saveRDS(test_list,"tables/res_fig5_rawmodeloutputs.RDS")

