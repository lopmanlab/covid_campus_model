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
  pdist = p_tab$Distribution
  
  #set seed for reproducibility, then sample
  set.seed(123)
  #random sample between 0 and 1 number of rows is number of replicates, each column is a different parameter
  lhssample=as.data.frame(lhs::randomLHS(samples,nrow(p_tab)))
  parsample = lhssample
  colnames(parsample) = p_tab$Var #name the samples
  #convert uniform 0-1 values for each parameter to uniform lower-upper values
  for (i in 1:ncol(parsample))
  {
    
    if (pdist[i]=="Beta")
    {
      #compute shape parameters based on mean and variance
      mu = parvals[i]; var = parvals[i]/10;
      shape1 = mu*(mu*(1-mu)/var - 1)
      shape2 = (1-mu)*(mu*(1-mu)/var - 1)
      parsample[,i] = qbeta(lhssample[,i], shape1 = shape1, shape2 = shape2)
    }
    if (pdist[i]=="Gamma")
    {
      #compute shape parameters based on mean and variance
      mu = parvals[i]; var = parvals[i]/10;
      shape = mu^2/var
      scale = var/mu
      parsample[,i] = qgamma(lhssample[,i], shape = shape, scale = scale)
    }
    if (pdist[i]=="Uniform")
    {
      parsample[,i] = qunif(lhssample[,i],min = parmin[i], max = parmax[i])
    }
    if (pdist[i]=="") #no distribution
    {
      parsample[,i] = parvals[i]
    }

    #turn this on to ignore everything above and do uniform sampling for all (so beta/gamma sampling)
    if (pdist[i]!="") 
    {
      #parsample[,i] = qunif(lhssample[,i],min = parmin[i], max = parmax[i])
    }
        
  }
  
  #browser()
  
  #LHS is done on reproductive number, now need to compute betas see the parm_init_new file for details on these equations
  beta_saf <- (parsample["R0_saf"]/parsample["infectious"]/(parsample["N_on"]+parsample["N_off"]+parsample["N_saf"]))                                                     
  beta_student_to_student <- ( (parsample["R0_student_to_student"]-beta_saf*parsample["infectious"]*parsample["N_saf"]) /parsample["infectious"]/(parsample["N_on"]+parsample["N_off"]))
  beta_on_to_on <- ((parsample["R0_student_to_student"] + parsample["Rp_on_to_on"]-beta_saf*parsample["infectious"]*parsample["N_saf"]-beta_student_to_student*parsample["infectious"]*parsample["N_off"] ) / parsample["infectious"] / parsample["N_on"]   )                    
  
  names(beta_student_to_student) = "beta_student_to_student"
  names(beta_on_to_on) = "beta_on_to_on"
  names(beta_saf) = "beta_saf"
  
  #add some more quantities/model parameters  
  parsample <- parsample %>% 
             mutate(beta_student_to_student = beta_student_to_student ) %>%
              mutate(beta_on_to_on = beta_on_to_on ) %>%
              mutate(beta_saf = beta_saf ) %>%
              mutate(p_asym_stu = 1- p_sympt_stu ) %>%
              mutate(p_asym_saf = 1- p_sympt_saf ) 
  
  return(parsample)
}

