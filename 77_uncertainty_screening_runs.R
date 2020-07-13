#Do uncertainty analysis by running over parameter samples
#baseline without screening or testing
# Load packages, data files and functions 
source("77_dependencies_new.R") #loads needed packages
source("77_model_func_new.R") #loads a function called 'model' which contains the main model. Used below in dcm routine
source("77_parm_init_new.R") #loads function that pulls values from spreadsheet to set parameter values and initial conditions
source("77_parm_dist_new.R") #loads function that creates a large matrix of distributions for each parameter

##########################################
#Do run for Emory for different NPI effectiveness
##########################################

#variable names for quantities we want to track/save for later use as a table
varnames = c( 'Peak_Stu_Inf', 'Cum_Stu_Inf', 'Cum_Stu_Hosp', 'Cum_Stu_Death', 'Cum_Stu_Iso', 'Cum_Stu_Q', 
              'Peak_Saf_Inf', 'Cum_Saf_Inf', 'Cum_Saf_Hosp', 'Cum_Saf_Death', 'Cum_Saf_Iso', 'Cum_Saf_Q', 'N_Test' )


nsamples = 10 #LHS samples

#load parameters and initial conditions for Emory
#returns a list with ini_cond and parvals 

pars_ini <- setpars_ini(school = "Emory")

#creates the specified number of samples (in rows) for all parameters (in columns)
#for convenience, all parameters are sampled, even the fixed ones. Those just have lower and upper bounds the same, thus their value doesn't change 
pardist = set_pardist(samples = nsamples, school = "Emory") 

pardist$screening = 30


#testing and screening are given/sampled as days. model needs rates, so convert here
pardist <- pardist %>% mutate(testing = 1/testing) %>% mutate(screening = 1/screening)

#if we want no screening and testing
pardist$testing = 0

all_res = NULL

#contains some quantities for table generation
res_df_emo = data.frame(matrix(0,ncol=length(varnames),nrow=nrow(pardist) ) ) 
colnames(res_df_emo) = varnames


#do loop over all parameter samples, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:nrow(pardist))
{
  parvals = pardist[i,] 
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, pars_ini$parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(run = i)
  
  res_df_emo[i,"Peak_Stu_Inf"]=max(df$Iasym_on+df$Isym_on+df$Iasym_off+df$Isym_off)
  res_df_emo[i,"Cum_Stu_Inf"]=dplyr::last(df$Iasymcum_on+df$Isymcum_on+df$Iasymcum_off+df$Isymcum_off)
  res_df_emo[i,"Cum_Stu_Hosp"]=dplyr::last(df$Hcum_on+df$Hcum_off)
  res_df_emo[i,"Cum_Stu_Death"]=dplyr::last(df$Dcum_on+df$Dcum_off)
  res_df_emo[i,"Cum_Stu_Iso"]=dplyr::last(df$Pcum_on+df$Pcum_off)
  res_df_emo[i,"Cum_Stu_Q"]=dplyr::last(df$Qcum_on+df$Qcum_off)
  
  res_df_emo[i,"Peak_Saf_Inf"]=max(df$Iasym_saf+df$Isym_saf)
  res_df_emo[i,"Cum_Saf_Inf"]=dplyr::last(df$Iasymcum_saf+df$Isymcum_saf)
  res_df_emo[i,"Cum_Saf_Hosp"]=dplyr::last(df$Hcum_saf)
  res_df_emo[i,"Cum_Saf_Death"]=dplyr::last(df$Dcum_saf)
  res_df_emo[i,"Cum_Saf_Iso"]=dplyr::last(df$Pcum_saf)
  res_df_emo[i,"Cum_Saf_Q"]=dplyr::last(df$Qcum_saf)
  
  
  if (i == 1) {all_res = df} #combine results from all runs into a long data frame
  #if (i > 1) {  all_res = rbind(all_res,df)} #could save all trajectories, but that could get massive, so disable in general
}

#add column with school label
df_emo = all_res %>% mutate(school = "Emory")


##########################################
#Do run for UGA 
##########################################


#load parameters and initial conditions for Emory
#returns a list with ini_cond and parvals 

pars_ini <- setpars_ini(school = "UGA")

#creates the specified number of samples (in rows) for all parameters (in columns)
#for convenience, all parameters are sampled, even the fixed ones. Those just have lower and upper bounds the same, thus their value doesn't change 
pardist = set_pardist(samples = nsamples, school = "UGA") 


pardist$screening = 30

#testing and screening are given/sampled as days. model needs rates, so convert here
pardist <- pardist %>% mutate(testing = 1/testing) %>% mutate(screening = 1/screening)

#if we want no screening and testing
pardist$testing = 0

all_res = NULL

#contains some quantities for table generation
res_df_uga = data.frame(matrix(0,ncol=length(varnames),nrow=nrow(pardist) ) ) 
colnames(res_df_uga) = varnames


#do loop over all parameter samples, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:nrow(pardist))
{
  parvals = pardist[i,] 
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, pars_ini$parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(run = i)
  
  res_df_uga[i,"Peak_Stu_Inf"]=max(df$Iasym_on+df$Isym_on+df$Iasym_off+df$Isym_off)
  res_df_uga[i,"Cum_Stu_Inf"]=dplyr::last(df$Iasymcum_on+df$Isymcum_on+df$Iasymcum_off+df$Isymcum_off)
  res_df_uga[i,"Cum_Stu_Hosp"]=dplyr::last(df$Hcum_on+df$Hcum_off)
  res_df_uga[i,"Cum_Stu_Death"]=dplyr::last(df$Dcum_on+df$Dcum_off)
  res_df_uga[i,"Cum_Stu_Iso"]=dplyr::last(df$Pcum_on+df$Pcum_off)
  res_df_uga[i,"Cum_Stu_Q"]=dplyr::last(df$Qcum_on+df$Qcum_off)
  
  res_df_uga[i,"Peak_Saf_Inf"]=max(df$Iasym_saf+df$Isym_saf)
  res_df_uga[i,"Cum_Saf_Inf"]=dplyr::last(df$Iasymcum_saf+df$Isymcum_saf)
  res_df_uga[i,"Cum_Saf_Hosp"]=dplyr::last(df$Hcum_saf)
  res_df_uga[i,"Cum_Saf_Death"]=dplyr::last(df$Dcum_saf)
  res_df_uga[i,"Cum_Saf_Iso"]=dplyr::last(df$Pcum_saf)
  res_df_uga[i,"Cum_Saf_Q"]=dplyr::last(df$Qcum_saf)
  
  
  if (i == 1) {all_res = df} #combine results from all runs into a long data frame
  #if (i > 1) {  all_res = rbind(all_res,df)}
}

#add column with school label
df_uga = all_res %>% mutate(school = "Emory")



##########################################
#Save results for baseline uncertainty run for loading and displaying inside Rmd file
##########################################

res_df_emo = cbind(res_df_emo, School = "Emory")
res_df_uga = cbind(res_df_uga, School = "UGA")
res_df = rbind(res_df_emo,res_df_uga)

filename = here('tables/','uncertain_screening_table.Rds')

saveRDS(res_df,filename)




