#Run testing/tracing scenarios for different levels of contact tracing
# Load packages, data files and functions 
source("77_dependencies_new.R") #loads needed packages
source("77_model_func_new.R") #loads a function called 'model' which contains the main model. Used below in dcm routine
source("77_parm_init_new.R") #loads function that pulls values from spreadsheet to set parameter values and initial conditions

##########################################
#Do run for Emory for different NPI effectiveness
##########################################

#load parameters and initial conditions for Emory
#returns a list with ini_cond and parvals 

pars_ini <- setpars_ini(school = "Emory")
parvals <- pars_ini$parvals

parvals["eff_npi"] = 0.4
parvals["screening"] = 0 #no screening
parvals["testing"] = 1/4 #4 day testing delay, need to convert to rate

#different proportion of contacts being traced
prop_contacts = seq(0,1,by=0.1)

#saves time-series
all_res = NULL

#saves values at end
varnames = c('p_contacts_reached','All_students','All_saf')
res_df = data.frame(matrix(0,ncol=length(varnames),nrow=length(prop_contacts) ) ) 
colnames(res_df) = varnames

#do loop over npi, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:length(prop_contacts))
{
  parvals["p_contacts_reached"] = prop_contacts[i] 
  
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) 
  #don't want to save time-series
  #if (i == 1) {all_res = df} #combine results from all runs into a long data frame
  #if (i > 1) {  all_res = rbind(all_res,df)}
  
  res_df[i,"p_contacts_reached"]=parvals["p_contacts_reached"]
  res_df[i,"All_students"]=dplyr::last(df$Iasymcum_on+df$Isymcum_on+df$Iasymcum_off+df$Isymcum_off)
  res_df[i,"All_saf"]=dplyr::last(df$Iasymcum_saf+df$Isymcum_saf)
}

#add column with school label
df_emo = res_df %>% mutate(school = "Emory")


##########################################
#Do run for UGA for different NPI effectiveness
##########################################

pars_ini <- setpars_ini(school = "UGA")
parvals <- pars_ini$parvals

parvals["eff_npi"] = 0.4
parvals["screening"] = 0 #no screening
parvals["testing"] = 1/4 #4 day testing delay, need to convert to rate

#different proportion of contacts being traced
prop_contacts = seq(0,1,by=0.1)

#saves time-series
all_res = NULL

#saves values at end
varnames = c('p_contacts_reached','All_students','All_saf')
res_df = data.frame(matrix(0,ncol=length(varnames),nrow=length(prop_contacts) ) ) 
colnames(res_df) = varnames

#do loop over npi, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:length(prop_contacts))
{
  parvals["p_contacts_reached"] = prop_contacts[i] 
  
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) 
  #don't want to save time-series
  #if (i == 1) {all_res = df} #combine results from all runs into a long data frame
  #if (i > 1) {  all_res = rbind(all_res,df)}
  
  res_df[i,"p_contacts_reached"]=parvals["p_contacts_reached"]
  res_df[i,"All_students"]=dplyr::last(df$Iasymcum_on+df$Isymcum_on+df$Iasymcum_off+df$Isymcum_off)
  res_df[i,"All_saf"]=dplyr::last(df$Iasymcum_saf+df$Isymcum_saf)
  
}


#add column with school label
df_uga = res_df %>% mutate(school = "UGA")

df <- rbind(df_emo,df_uga)


##########################################
#Make plot for NPI impact
##########################################


p1 <- df %>% filter(school == "Emory") %>%
             ggplot(aes(x=p_contacts_reached,y=All_students)) +  
             geom_line() 

p2 <- df %>% filter(school == "Emory") %>%
  ggplot(aes(x=p_contacts_reached,y=All_saf)) +  
  geom_line() 

p3 <- df %>% filter(school == "UGA") %>%
  ggplot(aes(x=p_contacts_reached,y=All_students)) +  
  geom_line() 

p4 <- df %>% filter(school == "UGA") %>%
  ggplot(aes(x=p_contacts_reached,y=All_saf)) +  
  geom_line() 


pl <- p1 + p2 + p3 + p4 

plot(pl)

filename = here('figures/','tracing_fig.png')
ggsave(filename, plot = pl, width = 10, height = 9)


