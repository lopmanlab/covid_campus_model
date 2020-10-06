set.seed(234567)
total.set.size <- 100
l <- randomLHS(total.set.size, 17)

# Uniform distributions
R0_s_s <- c(p_tab$Lower[which(p_tab$Var=="R0_student_to_student")],p_tab$Upper[which(p_tab$Var=="R0_student_to_student")]) # number of other students that a student infects, on average
R0_o_o <- c(p_tab$Lower[which(p_tab$Var=="R0_on_to_on")],p_tab$Upper[which(p_tab$Var=="R0_on_to_on")])           # number of additional students that a student living off campus infects, on average
R0_sa <- c(p_tab$Lower[which(p_tab$Var=="R0_saf")],p_tab$Upper[which(p_tab$Var=="R0_saf")])                # number of staff and faculty that an average student infects

contacts <- c(p_tab$Lower[which(p_tab$Var=="contacts")],p_tab$Upper[which(p_tab$Var=="contacts")])
p_contacts_reached <- c(p_tab$Lower[which(p_tab$Var=="p_contacts_reached")],p_tab$Upper[which(p_tab$Var=="p_contacts_reached")])
ili <-c(p_tab$Lower[which(p_tab$Var=="ili")],p_tab$Upper[which(p_tab$Var=="ili")])

#beta distributions
under_report <- p_tab$Value[which(p_tab$Var == "under_report")]                   # under-report factor
community <- betaExpert(best = p_tab$Value[which(p_tab$Var=="daily_new_case")] *under_report, lower=p_tab$Lower[which(p_tab$Var=="daily_new_case")]*under_report, 
                        upper= p_tab$Upper[which(p_tab$Var=="daily_new_case")]*under_report, p=0.95, method = "mean")

p_asympt_stu<- betaExpert(best = 1-p_tab$Value[which(p_tab$Var=="p_sympt_stu")], lower=1-p_tab$Upper[which(p_tab$Var=="p_sympt_stu")], upper= 1-p_tab$Lower[which(p_tab$Var=="p_sympt_stu")], p=0.95, method = "mean")
p_asympt_saf<- betaExpert(best = 1-p_tab$Value[which(p_tab$Var=="p_sympt_saf")], lower=1-p_tab$Upper[which(p_tab$Var=="p_sympt_saf")], upper= 1-p_tab$Lower[which(p_tab$Var=="p_sympt_saf")], p=0.95, method = "mean")

p_hosp_stu <- betaExpert(best = p_tab$Value[which(p_tab$Var=="p_hosp_stu")], lower=p_tab$Lower[which(p_tab$Var=="p_hosp_stu")], upper= p_tab$Upper[which(p_tab$Var=="p_hosp_stu")], p=0.95, method = "mean")
p_hosp_saf <- betaExpert(best = p_tab$Value[which(p_tab$Var=="p_hosp_saf")], lower=p_tab$Lower[which(p_tab$Var=="p_hosp_saf")], upper= p_tab$Upper[which(p_tab$Var=="p_hosp_saf")], p=0.95, method = "mean")

p_death_stu <- betaExpert(best = p_tab$Value[which(p_tab$Var=="p_death_stu")], lower=p_tab$Lower[which(p_tab$Var=="p_death_stu")], upper= p_tab$Upper[which(p_tab$Var=="p_death_stu")], p=0.95, method = "mean")
p_death_saf <- betaExpert(best = p_tab$Value[which(p_tab$Var=="p_death_saf")], lower=p_tab$Lower[which(p_tab$Var=="p_death_saf")], upper= p_tab$Upper[which(p_tab$Var=="p_death_saf")], p=0.95, method = "mean")

sensitivity <- betaExpert(best = p_tab$Value[which(p_tab$Var=="sensitivity")], lower=p_tab$Lower[which(p_tab$Var=="sensitivity")], upper= p_tab$Upper[which(p_tab$Var=="sensitivity")], p=0.95, method = "mean")

eff_npi <- betaExpert(best = p_tab$Value[which(p_tab$Var=="eff_npi")],lower=p_tab$Lower[which(p_tab$Var=="eff_npi")],upper=p_tab$Upper[which(p_tab$Var=="eff_npi")],p=0.95, method="mean")

# gamma distributions
latent <-getdistr_parms(int.quantiles = c(2,4), int.mean = 3, starting.params = c(32, 11), distrib = "gamma")
infectious_beta <-getdistr_parms(int.quantiles = c(6,8), int.mean = 7, starting.params = c(32, 11), distrib = "gamma")


R0_student_to_student.int <-round((l[,1]*(R0_s_s[2]-R0_s_s[1]))+R0_s_s[1],3)
R0_on_to_on.int <- round((l[,2]*(R0_o_o[2]-R0_o_o[1]))+R0_o_o[1],3)
R0_saf.int <-round((l[,3]*(R0_sa[2]-R0_sa[1]))+R0_sa[1],3)
community.int <- qbeta(l[,4],community$alpha,community$beta)


p_asympt_stu.int <- qbeta(l[,5],p_asympt_stu$alpha,p_asympt_stu$beta)
p_asympt_saf.int <- qbeta(l[,6],p_asympt_saf$alpha,p_asympt_saf$beta)

contacts.int <- round((l[,7]*(contacts[2]-contacts[1]))+contacts[1],3)
ili.int <- round((l[,8]*(ili[2]-ili[1]))+ili[1],5)
sensitivity.int <-qbeta(l[,9],sensitivity$alpha,sensitivity$beta)

p_contacts_reached.int <- round((l[,10]*(p_contacts_reached[2]-p_contacts_reached[1]))+p_contacts_reached[1],5)   # We only include this in PSA of scenarios with screening and testing. 

latent.int <-qgamma(l[,11],latent[1],latent[2])
infectious.int <-qgamma(l[,12],infectious_beta[1],infectious_beta[2])

p_hosp_stu.int <- qbeta(l[,13],p_hosp_stu$alpha,p_hosp_stu$beta)
p_hosp_saf.int <- qbeta(l[,14],p_hosp_saf$alpha,p_hosp_saf$beta)

p_death_stu.int <-qbeta(l[,15],p_death_stu$alpha,p_death_stu$beta)
p_death_saf.int <- qbeta(l[,16],p_death_saf$alpha,p_death_saf$beta)

eff_npi.int <- qbeta(l[,17],eff_npi$alpha, eff_npi$beta)

beta_student_to_student.int <- as.numeric(R0_student_to_student.int/infectious/(N_on+N_off))
beta_on_to_on.int = as.numeric((R0_student_to_student.int + R0_on_to_on.int) / infectious / N_on)  
beta_saf.int = as.numeric(R0_saf.int/infectious/(N_on+N_off+N_saf)) 

