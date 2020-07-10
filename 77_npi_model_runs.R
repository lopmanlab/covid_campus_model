# Load packages, data files and functions 
source("99_dependencies_new.R") #loads needed packages
source("99_model_func_new.R") #loads a function called 'model' which contains the main model. Used below in dcm routine
source("99_parm_init_new.R") #loads function that pulls values from spreadsheet to set parameter values and initial conditions

##########################################
#Do run for Emory for different NPI effectiveness
##########################################

#load parameters and initial conditions for Emory
#returns a list with ini_cond and parvals 

pars_ini <- setpars_ini(school = "Emory")
parvals <- pars_ini$parvals

#eff_npi <- seq(0, 1, 0.2) #changing amount that NPI are expected to reduce transmission
eff_npi <- 0 #changing amount that NPI are expected to reduce transmission
all_res = NULL
for (i in 1:length(eff_npi))
{
  parvals["eff_npi"] = eff_npi[i]
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 0.1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(eff_npi = eff_npi[i])
  if (i == 1) {all_res = df}
  if (i > 1) {  all_res = rbind(all_res,df)}
}

df_emo = all_res


##########################################
#Do run for UGA for different NPI effectiveness
##########################################

pars_ini <- setpars_ini(school = "UGA")
parvals <- pars_ini$parvals

eff_npi <- seq(0, 1, 0.2) #changing amount that NPI are expected to reduce transmission
all_res = NULL
for (i in 1:length(eff_npi))
{
  parvals["eff_npi"] = eff_npi[i]
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(eff_npi = eff_npi[i])
  if (i == 1) {all_res = df}
  if (i > 1) {  all_res = rbind(all_res,df)}
}

df_uga = all_res

#convert to data frame so we can use ggplot
df_uga = as.data.frame(npi_UGA)


##########################################
#Make plot for NPI impact
##########################################


p1 <- df_emo %>% ggplot(aes(x=time,y=Isym_on + Isym_off, color=as.factor(eff_npi))) +  
             geom_line() 

p2 <- df_emo %>% ggplot(aes(x=time,y=I_saf, color=as.factor(run))) +  
  geom_line() 

p3 <- df_uga %>% ggplot(aes(x=time,y=I_stu, color=as.factor(run))) +  
  geom_line() 

p4 <- df_uga %>% ggplot(aes(x=time,y=I_saf, color=as.factor(run))) +  
  geom_line() 

pl <- p1 + p2 + p3 + p4

##########################################
#Save results for NPI impact for loading and displaying inside Rmd file
##########################################

#npi = list()
#npi$R0_on = R0_student_to_student + R0_on_to_on
#npi$R0_off = R0_student_to_student
#npi$R0_saf =  R0_saf


