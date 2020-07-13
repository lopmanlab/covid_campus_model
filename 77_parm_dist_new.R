#Load the CSV file containing parameter values and assign them to variables below

set_pardist <- function(samples = 10, school)
{

  #load parameter table
  parameter_table <- read.csv("77_ParameterTable_new.csv")
  p_tab <- parameter_table %>% dplyr::filter(School == school | School == "All") #get values for the right school, remove the others
  
  #some cleanup from CSV file
  p_tab$Value <-gsub(",","",p_tab$Value)
  p_tab$Value <- as.numeric(as.character(p_tab$Value))
  p_tab$Lower <- as.numeric(as.character(p_tab$Lower))
  p_tab$Upper <- as.numeric(as.character(p_tab$Upper))
  
  #some rows in the table are not parameters and contain NA, kick them out
  p_tab = p_tab[!is.na(p_tab$Value),]
  
  #parameter values and their names in model from CSV file
  parvals = p_tab$Value
  parmin = p_tab$Lower
  parmax = p_tab$Upper
  
  #set seed for reproducibility, then sample
  set.seed(123)
  #random sample between 0 and 1 number of rows is number of replicates, each column is a different parameter
  parvals=as.data.frame(lhs::randomLHS(samples,nrow(p_tab)))
  colnames(parvals) = p_tab$Var #name the samples
  #convert uniform 0-1 values for each parameter to uniform lower-upper values
  for (i in 1:ncol(parvals))
  {
    parvals[,i] = qunif(parvals[,i],min = parmin[i], max = parmax[i])
  }
  
  #LHS is done on reproductive number, now need to compute betas see the parm_init_new file for details on these equations
  beta_saf <- (parvals["R0_saf"]/parvals["infectious"]/(parvals["N_on"]+parvals["N_off"]+parvals["N_saf"]))                                                     
  beta_student_to_student <- ( (parvals["R0_student_to_student"]-beta_saf*parvals["infectious"]*parvals["N_saf"]) /parvals["infectious"]/(parvals["N_on"]+parvals["N_off"]))
  beta_on_to_on <- ((parvals["R0_student_to_student"] + parvals["Rp_on_to_on"]-beta_saf*parvals["infectious"]*parvals["N_saf"]-beta_student_to_student*parvals["infectious"]*parvals["N_off"] ) / parvals["infectious"] / parvals["N_on"]   )                    
  
  names(beta_student_to_student) = "beta_student_to_student"
  names(beta_on_to_on) = "beta_on_to_on"
  names(beta_saf) = "beta_saf"
  
  #add some more quantities/model parameters  
  pardist <- parvals %>% 
             mutate(beta_student_to_student = beta_student_to_student ) %>%
              mutate(beta_on_to_on = beta_on_to_on ) %>%
              mutate(beta_saf = beta_saf ) %>%
              mutate(p_asym_stu = 1- p_sympt_stu ) %>%
              mutate(p_asym_saf = 1- p_sympt_saf ) 
  
  return(pardist)
}

