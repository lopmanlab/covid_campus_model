#Load the CSV file containing parameter values and assign them to variables below

setpars_ini <- function(school)
{

  #load parameter table
  parameter_table <- read.csv("77_ParameterTable_new.csv")
  p_tab <- parameter_table %>% dplyr::filter(School == school | School == "All") #get values for the right school, remove the others
  
  #some cleanup from CSV file
  p_tab$Value <-gsub(",","",p_tab$Value)
  p_tab$Value <- as.numeric(as.character(p_tab$Value))
  p_tab$Lower <- as.numeric(as.character(p_tab$Lower))
  p_tab$Upper <- as.numeric(as.character(p_tab$Upper))
  
  #parameter values and their names in model from CSV file
  parvals = p_tab$Value
  names(parvals) = p_tab$Var
  
  #some rows in the table are not parameters and contain NA, kick them out
  parvals = parvals[!is.na(parvals)]
  
  #compute some additional quantities
  #as numeric strips names away
  beta_student_to_student <- as.numeric(parvals["R0_student_to_student"]/parvals["infectious"])                       
  beta_on_to_on <- as.numeric(parvals["Rp_on_to_on"]/parvals["infectious"])                                           
  beta_saf <- as.numeric(parvals["R0_saf"]/parvals["infectious"])                                                     
  
  p_asym_stu <- as.numeric(1-parvals["p_sympt_stu"])                  
  p_asym_saf <- as.numeric(1-parvals["p_sympt_saf"]) 
  
  N_off = as.numeric(parvals["N"]-parvals["N_on"])
  
  #start and end date of term/simulation
  #not used right now, run each model/school for 120 days
  #start_date <- as.Date("2020-08-26")
  #end_date <- as.Date("2020-12-19")
  #tmax = as.vector(difftime(end_date,start_date,units = "days"))
  tmax = 120
  
  #place all parameters in a vector
  parvals = c(parvals,p_asym_stu = p_asym_stu,
                      p_asym_saf = p_asym_saf,
                  beta_student_to_student = beta_student_to_student,
              beta_on_to_on = beta_on_to_on,
              beta_saf = beta_saf,
              tmax = tmax,
              N_off=N_off)
  
  #assume one infectious in each category at start
  Iasym_on = 1; Iasym_off = 1; Iasym_saf = 1;
  

  ## Initial conditions to model
  ini_cond <- c(S_on=as.numeric(parvals["N_on"]-Iasym_on),        # number initially susceptible
                E_on = 0,                      #students living on campus
                Iasym_on = Iasym_on,
                Isym_on = 0,
                P_on = 0,
                Q_on = 0,
                R_on = 0,
                Iasymcum_on = 0,                       # cumulative cases -- for counting incidence
                Isymcum_on = 0,                       # cumulative cases -- for counting incidence
                Pcum_on = 0,
                Qcum_on = 0,
                Hcum_on =0,
                Dcum_on=0,
                
                S_off=as.numeric(parvals["N_off"]-Iasym_off),
                E_off = 0,                     #students living off campus
                Iasym_off = Iasym_off,
                Isym_off = 0,
                P_off = 0,
                Q_off = 0,
                R_off = 0,
                Iasymcum_off = 0,                       # cumulative cases -- for counting incidence
                Isymcum_off = 0,                       # cumulative cases -- for counting incidence
                Pcum_off = 0,
                Qcum_off = 0,
                Hcum_off =0,
                Dcum_off=0,
                
                S_saf=as.numeric(parvals["N_saf"]-Iasym_saf),
                E_saf = 0,
                Iasym_saf = Iasym_saf,
                Isym_saf = 0,
                P_saf = 0,
                Q_saf = 0,
                R_saf = 0,
                Iasymcum_saf = 0,                       # cumulative cases -- for counting incidence
                Isymcum_saf = 0,                       # cumulative cases -- for counting incidence
                Pcum_saf = 0,
                Qcum_saf = 0,
                Hcum_saf =0,
                Dcum_saf=0,
                
                Test = 0
  )
  
  
  return(list(parvals = parvals, ini_cond = ini_cond))
}

