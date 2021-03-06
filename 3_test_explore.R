## FIgure 4: Combines PSA and scenario analysis over a sweep of testing interval and contact tracing scenarios. No screening interventions and NPI effectiveness using 35%
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
test_scen_in <- 1/c(2,4,7)
test_scen <- c(2, 4, 7)
sensitivity_scen <-list(sensitivity_2.int,sensitivity.int,sensitivity_7.int)

pal <- brewer_ramp(length(test_scen), "Spectral")


test_list<-list()                   #Initialize list to collect results from each screening interval
# Below loop runs model

for (i in 1:length(test_scen_in)) {
  test_list[[i]]<-model_scenarios(testing=test_scen_in[i], sensitivity_input = sensitivity_scen[[i]],eff_npi.int=eff_npi.int) 
  
}
#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
test_list_cases <- list()
for (i in 1:length(test_list)){
  test_list_cases[[i]] <- getcases(test_list[[i]]) %>%
    mutate(scenario = rep(test_scen[i])) 
}


test_df <- bind_rows(test_list_cases, .id = "column_label")
# plot
theme <- theme_classic()+ theme(legend.position = "none",
                                plot.title = element_text(size=11, face="bold"),
                                axis.text = element_text(size=9),
                                axis.title = element_text(size=9))


# Testing over proportion contacts reached (default contact tracing), need to also specify both testing and sensitivty along with it

p_contacts_reached <- seq(0, 1, 0.1)
test2_list<-list() 
test4_list <- list()
test7_list<-list()
test2_list_cases <-list()
test4_list_cases <- list()
test7_list_cases <- list()
#Initialize list to collect results from each screening interval
# Below loop runs model 
for (i in 1:length(p_contacts_reached)){
    test2_list[[i]]<-model_scenarios(testing=test_scen_in[1], sensitivity_input = sensitivity_scen[[1]], p_contacts_reached = p_contacts_reached[i],eff_npi.int=eff_npi.int)
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
for (i in 1:length(test2_list)){
  test2_list_cases[[i]] <- getcases(test2_list[[i]])  %>%
    mutate(scenario = rep(p_contacts_reached[i]))
}
## 4day test delay

for (i in 1:length(p_contacts_reached)){
  test4_list[[i]]<-model_scenarios(testing=test_scen_in[2], sensitivity_input = sensitivity_scen[[2]], p_contacts_reached = p_contacts_reached[i],eff_npi.int=eff_npi.int)
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
for (i in 1:length(test4_list)){
  test4_list_cases[[i]] <- getcases(test4_list[[i]])  %>%
    mutate(scenario = rep(p_contacts_reached[i]))
}


# 7 day test delay
for (i in 1:length(p_contacts_reached)){
  test7_list[[i]]<-model_scenarios(testing=test_scen_in[3], sensitivity_input = sensitivity_scen[[3]], p_contacts_reached = p_contacts_reached[i],eff_npi.int=eff_npi.int)
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
for (i in 1:length(test7_list)){
  test7_list_cases[[i]] <- getcases(test7_list[[i]])  %>%
    mutate(scenario = rep(p_contacts_reached[i]))
}


test_all <- list(test2_list_cases,test4_list_cases,test7_list_cases)


for (i in 1:length(test_all)){
test_all[[i]] <- bind_rows(test_all[[i]], .id = "column_label") %>% filter(time==116)%>%
                  select(med_stud_cum,med_saf_cum,scenario)%>%
                  melt(id.vars="scenario") %>%
                  mutate(test = test_scen[i])
                      }

test_trace_df <- do.call(rbind,test_all)




saveRDS(test_df,"tables/res_fig3_testdf.RDS")
saveRDS(test_trace_df,"tables/res_fig3_testtracedf.RDS")


saveRDS(test_list,"tables/res_fig2_rawmodeloutputs_test.RDS")
saveRDS(test2_list,"tables/res_fig2_rawmodeloutputs_test2.RDS")
saveRDS(test4_list,"tables/res_fig2_rawmodeloutputs_test4.RDS")
saveRDS(test7_list,"tables/res_fig2_rawmodeloutputs_test7.RDS")


