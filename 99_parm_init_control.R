parameter_table <- read.csv("99_ParameterTable_s.csv")
p_tab <- parameter_table
p_tab$Value <-gsub(",","",p_tab$Value)
p_tab$Value <- as.numeric(as.character(p_tab$Value))
p_tab$Lower <- as.numeric(as.character(p_tab$Lower))
p_tab$Upper <- as.numeric(as.character(p_tab$Upper))

#Epi parameters
R0_student_to_student <- p_tab$Value[which(p_tab$Var=="R0_student_to_student")]   # number of other students that a student infects, on average
R0_on_to_on <- p_tab$Value[which(p_tab$Var == "R0_on_to_on")]                     # number of additional students that a student living off campus infects, on average
R0_saf <- p_tab$Value[which(p_tab$Var == "R0_saf")]                               # number of staff and faculty that an average student infects

beta_student_to_student <- as.numeric(R0_student_to_student/infectious/(N_on+N_off)) # daily effective contact rates
beta_on_to_on <- as.numeric((R0_student_to_student + R0_on_to_on) / infectious / N_on)                                           
beta_saf <- as.numeric(R0_saf/infectious/(N_on+N_off+N_saf))  

latent <- p_tab$Value[which(p_tab$Var == "latent")]                               # latent period duration in days.  This is shorter than incubation period, which is more like 5 days
infectious <- p_tab$Value[which(p_tab$Var == "infectious")]                       # infectious period in days.This is longer than symptomatic period, which is more like 6 days.
                                                                                  # Effectively this assumes that infectiousness starts 1 day before symptoms

#beta_student_to_student <- R0_student_to_student/infectious                       # daily effective contact rate -- symptomatic period
#beta_on_to_on <- R0_on_to_on/infectious                                           # daily effective contact rate -- symptomatic period
#beta_saf <- R0_saf/infectious  
# daily effective contact rate -- symptomatic period
eff_npi <- p_tab$Value[which(p_tab$Var == "eff_npi")]                             # efficacy of masking and other NPIs

community <- p_tab$Value[which(p_tab$Var == "community")]                         # daily probability of community infection - not acquired on campus

p_asympt_stu <- 1-p_tab$Value[which(p_tab$Var == "p_sympt_stu")]                  # proportion asymptomatic -- students
p_hosp_stu <- p_tab$Value[which(p_tab$Var == "p_hosp_stu")]                       # probability of hospitalization -- students
p_death_stu <- p_tab$Value[which(p_tab$Var == "p_death_stu")]                     # probability of death -- students

p_asympt_saf <- 1-p_tab$Value[which(p_tab$Var == "p_sympt_saf")]                  # proportion asymptomatic -- staff and faculty
p_hosp_saf <- p_tab$Value[which(p_tab$Var == "p_hosp_saf")]                       # probability of hospitalization -- staff and faculty
p_death_saf <- p_tab$Value[which(p_tab$Var == "p_death_saf")]                     # probability of death -- staff and faculty

contacts <- p_tab$Value[which(p_tab$Var == "contacts")]                           # contacts per case
p_contacts_reached <- p_tab$Value[which(p_tab$Var == "p_contacts_reached")]       # proportion of contacts reached
ili <- p_tab$Value[which(p_tab$Var == "ili")]                                     # daily ili
sensitivity  <- p_tab$Value[which(p_tab$Var == "sensitivity")]                    # PCR sensitivity on day 4
sensitivity_2  <- p_tab$Value[which(p_tab$Var == "sensitivity_2")]                # PCR sensitivity on day 2
sensitivity_7  <- p_tab$Value[which(p_tab$Var == "sensitivity_7")]                # PCR sensitivity on day 7

isolation <- p_tab$Value[which(p_tab$Var == "isolation")]                         # isolation or quarantine period in days



# Emory population
E_on=0                      #students living on campus
I_on=0
P_on = 0
R_on = 0
Q_on = 0

E_off=0                     #students living off campus
I_off=0
P_off = 0
R_off = 0
Q_off = 0

N_saf = 15266               #staff and faculty
E_saf=0
I_saf=0
P_saf = 0
R_saf = 0
Q_saf = 0


N_on <- p_tab$Value[which(p_tab$Var == "N_on")]
testing=0
screening=0

N_off<-p_tab$Value[which(p_tab$Var == "N")]-N_on           #Based on number on campus

N = N_on + N_off + N_saf


## Initial conditions to model
init <- init.dcm(S_on=N_on-(E_on+I_on+R_on),        # number initially susceptible
                 E_on=E_on,                         # number initially incubating
                 I_on=I_on,                         # number initially infectious
                 Isym_on = 0,
                 P_on=P_on,                         # number initially isolated
                 R_on=R_on,                         # initially immune
                 Icum_on = 0,                       # cumulative cases -- for counting incidence
                 Pcum_on = 0,
                 Q_on = Q_on,
                 Qcum_on = 0,
                 Hcum_on =0,
                 Dcum_on=0,

                 S_off=N_off-(E_off+I_off+R_off),
                 E_off=E_off,
                 I_off=I_off,
                 Isym_off = 0,
                 P_off=P_off,
                 R_off=R_off,
                 Icum_off = 0,
                 Pcum_off = 0,
                 Q_off = Q_off,
                 Qcum_off = 0,
                 Hcum_off = 0,
                 Dcum_off =0,

                 S_saf=N_saf-(E_saf+I_saf+R_saf),
                 E_saf=E_saf,
                 I_saf=I_saf,
                 Isym_saf = 0,
                 P_saf=P_saf,
                 R_saf=R_saf,
                 Icum_saf = 0,
                 Pcum_saf = 0,
                 Q_saf = 0,
                 Hcum_saf =0,
                 Dcum_saf =0,

                 Test = 0,
                 )

# Control features
control <- control.dcm(nsteps = 116, new.mod = model)
