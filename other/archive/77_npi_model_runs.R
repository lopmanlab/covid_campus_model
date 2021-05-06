# Load packages, data files and functions 
source("77_dependencies_new.R") #loads needed packages
source("77_model_func_new.R") #loads a function called 'model' which contains the main model. Used below in dcm routine
source("77_ggplot_themes.R") #loads themes for ggplot styling
source("77_parm_init_new.R") #loads function that pulls values from spreadsheet to set parameter values and initial conditions

#variable names for quantities we want to track/save for later use as a table
varnames = c( 'eff_npi',
              'Peak_Stu_Inf', 'Cum_Stu_Inf', 'Cum_Stu_Hosp', 'Cum_Stu_Death', 'Cum_Stu_Iso', 'Cum_Stu_Q', 
              'Peak_Saf_Inf', 'Cum_Saf_Inf', 'Cum_Saf_Hosp', 'Cum_Saf_Death', 'Cum_Saf_Iso', 'Cum_Saf_Q', 'N_Test' )


##########################################
#Do run for Emory for different NPI effectiveness
##########################################

#load parameters and initial conditions for Emory
#returns a list with ini_cond and parvals 

pars_ini <- setpars_ini(school = "Emory")
parvals <- pars_ini$parvals

parvals["testing"] = 0
parvals["screening"] = 0


eff_npi <- seq(0, 1, 0.2) #changing amount that NPI are expected to reduce transmission
#eff_npi <- 0 #changing amount that NPI are expected to reduce transmission
all_res = NULL #contains time-series for all variables for all runs

#contains some quantities for table generation
res_df_emo = data.frame(matrix(0,ncol=length(varnames),nrow=length(eff_npi) ) ) 
colnames(res_df_emo) = varnames
res_df_emo$eff_npi = eff_npi  

#do loop over npi, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:length(eff_npi))
{
  parvals["eff_npi"] = eff_npi[i]
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(eff_npi = eff_npi[i])
  
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
  if (i > 1) {  all_res = rbind(all_res,df)}
}

#add column with school label
df_emo = all_res %>% mutate(school = "Emory")


##########################################
#Do run for UGA for different NPI effectiveness
##########################################

pars_ini <- setpars_ini(school = "UGA")
parvals <- pars_ini$parvals

parvals["testing"] = 0
parvals["screening"] = 0


eff_npi <- seq(0, 1, 0.2) #changing amount that NPI are expected to reduce transmission
all_res = NULL

#contains some quantities for table generation
res_df_uga = data.frame(matrix(0,ncol=length(varnames),nrow=length(eff_npi) ) ) 
colnames(res_df_uga) = varnames
res_df_uga$eff_npi = eff_npi  


for (i in 1:length(eff_npi))
{
  parvals["eff_npi"] = eff_npi[i]
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(eff_npi = eff_npi[i])
  
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
  
  
  
  if (i == 1) {all_res = df}
  if (i > 1) {  all_res = rbind(all_res,df)}
}

df_uga = all_res %>% mutate(school = "UGA")


df <- bind_rows(df_emo,df_uga) %>% 
        mutate(NPI_Impact = as.factor(eff_npi)) %>%
        mutate(All_students = Isym_on + Isym_off + Iasym_on + Iasym_off) %>%
        mutate(Cum_students = Iasymcum_on + Isymcum_on + Iasymcum_off + Isymcum_off) %>%
        mutate(All_saf = Isym_saf + Iasym_saf) %>%
        mutate(Cum_saf = Isymcum_saf + Iasymcum_saf) 
  


##########################################
#Make plot for NPI impact
##########################################


p1 <- df %>% filter(school == "Emory") %>%
             ggplot(aes(x=time,y=All_students, color=NPI_Impact)) +  
             geom_line() + 
             geom_line(aes(y=Cum_students)) + manuscript_theme

  
p2 <- df %>% filter(school == "Emory") %>%
             ggplot(aes(x=time,y=All_saf, color=NPI_Impact)) +  
             geom_line() + 
             geom_line(aes(y=Cum_saf)) + manuscript_theme

p3 <- df %>%  filter(school == "UGA") %>%
              ggplot(aes(x=time,y=All_students, color=NPI_Impact)) +  
              geom_line() + 
              geom_line(aes(y=Cum_students)) + manuscript_theme

p4 <- df %>%  filter(school == "UGA") %>%
              ggplot(aes(x=time,y=All_saf, color=NPI_Impact)) +  
              geom_line() + 
              geom_line(aes(y=Cum_saf)) + manuscript_theme

pl <- p1 + p2 + p3 + p4 

plot(pl)

filename = here('figures/','npi_fig.png')
ggsave(filename, plot = pl, width = 10, height = 9)


##########################################
#Save results for NPI impact for loading and displaying inside Rmd file
##########################################

res_df_emo = cbind(res_df_emo, School = "Emory")
res_df_uga = cbind(res_df_uga, School = "UGA")
res_df = rbind(res_df_emo,res_df_uga)

filename = here('tables/','npi_table.Rds')

saveRDS(res_df,filename)

