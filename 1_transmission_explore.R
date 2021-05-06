# Figure 2: PSA + scenario analysis over a range of transmission reduction scenarios due to implentation of NPIs.

memory.limit(size=500000000)
# Load dependencies, functions and parameters
source("99_dependencies.R")
source("99_model_func.R")
source("99_parm_init_control.R")
source("99_psa_optimizedistr.R")
source("99_psa_parm.R")   #Note this overwrites initial parameters from parm_init_control





eff_npi <- c(seq(0, 0.3, 0.1),0.35, seq(0.4,1.0,0.1))

pal <- brewer_ramp(7, "Spectral")


trans_list<-list()                   #Initialize list to collect results from each screening interval
# Below loop runs model

for (i in 1:length(eff_npi)) {
  trans_list[[i]]<-model_scenarios(eff_npi.int=eff_npi[[i]],p_contacts_reached = 0) 
  
}

#Below loop takes each scenario and computes median active and cumulative cases for students and staff on each day
trans_list_cases <- list()
for (i in 1:length(trans_list)){
  trans_list_cases[[i]] <- getcases(trans_list[[i]]) %>%
    mutate(scenario = rep(eff_npi[i])) 
}


trans_df <- bind_rows(trans_list_cases, .id = "column_label")


trans_list_peaks <- data.frame()

for(i in 1:length(trans_list)) {
  trans_list_peaks[i,1] <-max(trans_list[[i]]$I_stu)
  trans_list_peaks[i,2] <- max(trans_list[[i]]$I_saf)
}

peakcases <- function(x){
  x%>%
    summarize(max_stu = max(I_stu),
              max_saf = max(I_saf))
}


saveRDS(trans_df,"tables/res_fig2_trans_df.RDS")
saveRDS(trans_list, "tables/res_fi2_rawmodeloutputs_trans.RDS")

