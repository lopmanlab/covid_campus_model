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
  lhssample=as.data.frame(lhs::randomLHS(samples,nrow(p_tab)))
  colnames(lhssample) = p_tab$Var #name the samples
  #convert uniform 0-1 values for each parameter to uniform lower-upper values
  for (i in 1:ncol(lhssample))
  {
    lhssample[,i] = qunif(lhssample[,i],min = parmin[i], max = parmax[i])
  }


  #add some more quantities/model parameters  
  pardist <- lhssample %>% 
             mutate(beta_student_to_student = R0_student_to_student/infectious ) %>%
              mutate(beta_on_to_on = Rp_on_to_on/infectious ) %>%
              mutate(beta_saf = R0_saf/infectious ) %>%
              mutate(p_asym_stu = 1- p_sympt_stu ) %>%
              mutate(p_asym_saf = 1- p_sympt_saf ) %>%
            mutate(N_off = N - N_on )   
  
  return(pardist)
}

