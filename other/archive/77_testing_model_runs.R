#Run testing scenarios
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
parvals["screening"] = 0

#2, 4 and 7 days
testing_delays = c(2,4,7)

all_res = NULL

#do loop over npi, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:length(testing_delays))
{
  parvals["testing"] = 1/testing_delays[i] #take inverse since it's used as rate in the model
  
  #depending on testing delay, test sensitivity changes
  if (testing_delays[i] == 2) { parvals["sensitivity"] = pars_ini$parvals["sensitivity_2"] }
  if (testing_delays[i] == 4) { parvals["sensitivity"] = pars_ini$parvals["sensitivity"] }
  if (testing_delays[i] == 7) { parvals["sensitivity"] = pars_ini$parvals["sensitivity_7"] }
  
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(Testing_delay = as.factor(testing_delays[i]))
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

parvals["eff_npi"] = 0.4
parvals["screening"] = 0

#2, 4 and 7 days
testing_delays = c(2,4,7)

all_res = NULL

#do loop over npi, run model for each
#not using EpiModel, just basic ode solver
for (i in 1:length(testing_delays))
{
  parvals["testing"] = 1/testing_delays[i] #take inverse since it's used as rate in the model
  res <- deSolve::ode(y = pars_ini$ini_cond, times = seq(0, parvals["tmax"], by = 1), func = covid_model, parms = parvals)
  df <- data.frame(res) %>% mutate(Testing_delay = as.factor(testing_delays[i]))
  if (i == 1) {all_res = df} #combine results from all runs into a long data frame
  if (i > 1) {  all_res = rbind(all_res,df)}
}

#add column with school label
df_uga = all_res %>% mutate(school = "UGA")


df <- bind_rows(df_emo,df_uga) %>% 
        mutate(All_students = Isym_on + Isym_off + Iasym_on + Iasym_off) %>%
        mutate(Cum_students = Iasymcum_on + Isymcum_on + Iasymcum_off + Isymcum_off) %>%
        mutate(All_saf = Isym_saf + Iasym_saf) %>%
        mutate(Cum_saf = Isymcum_saf + Iasymcum_saf) 
  


##########################################
#Make plot for NPI impact
##########################################


p1 <- df %>% filter(school == "Emory") %>%
             ggplot(aes(x=time,y=All_students, color=Testing_delay)) +  
             geom_line() + 
             geom_line(aes(y=Cum_students)) 
  
p2 <- df %>% filter(school == "Emory") %>%
             ggplot(aes(x=time,y=All_saf, color=Testing_delay)) +  
             geom_line() + 
             geom_line(aes(y=Cum_saf)) 

p3 <- df %>%  filter(school == "UGA") %>%
              ggplot(aes(x=time,y=All_students, color=Testing_delay)) +  
              geom_line() + 
              geom_line(aes(y=Cum_students)) 

p4 <- df %>%  filter(school == "UGA") %>%
              ggplot(aes(x=time,y=All_saf, color=Testing_delay)) +  
              geom_line() + 
              geom_line(aes(y=Cum_saf)) 

pl <- p1 + p2 + p3 + p4 

plot(pl)

filename = here('figures/','testing_fig.png')
ggsave(filename, plot = pl, width = 10, height = 9)


##########################################
#Save results for NPI impact for loading and displaying inside Rmd file
##########################################

#npi = list()
#npi$R0_on = R0_student_to_student + R0_on_to_on
#npi$R0_off = R0_student_to_student
#npi$R0_saf =  R0_saf


