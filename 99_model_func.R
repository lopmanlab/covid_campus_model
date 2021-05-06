model <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    # ODEs
    # On campus students
    lam_on = (1-eff_npi)*(beta_student_to_student*(I_off+I_on)+ beta_on_to_on*I_on + beta_saf*I_saf)
    #dS_on <- -lam_on*S_on - community*S_on - testing*(1-p_asympt_stu)*I_on*sensitivity*(contacts-R0_on_to_on-R0_student_to_student)*p_contacts_reached + 1/isolation*Q_on*((contacts-R0_on_to_on-R0_student_to_student)/contacts)
    dS_on <- -lam_on*S_on - community*S_on - (testing*(1-p_asympt_stu)*I_on+screening_on*(E_on+I_on))*sensitivity*(contacts-R0_on_to_on-R0_student_to_student)*p_contacts_reached + 1/isolation*Q_on*((contacts-R0_on_to_on-R0_student_to_student)/contacts)
    #dE_on <-  lam_on*S_on + community*S_on - 1/latent*E_on  - screening_on*E_on*sensitivity - testing*(1-p_asympt_stu)*I_on*sensitivity*(R0_on_to_on+R0_student_to_student)*p_contacts_reached #last term represents contract tracing
    dE_on <-  lam_on*S_on + community*S_on - 1/latent*E_on  - screening_on*E_on*sensitivity - (testing*(1-p_asympt_stu)*I_on+screening_on*(E_on+I_on))*sensitivity*(R0_on_to_on+R0_student_to_student)*p_contacts_reached #last term represents contract tracing
    dI_on <- 1/latent*E_on - testing*(1-p_asympt_stu)*sensitivity*I_on - 1/infectious*I_on - screening_on*I_on*sensitivity
    #dIsym_on <- (1-p_asympt_stu)*(1/latent*E_on - testing*(1-p_asympt_stu)*sensitivity*I_on - 1/infectious*I_on - screening_on*I_on*sensitivity)
    dIsym_on <- (1-p_asympt_stu)*(1/latent*E_on - testing*(1-p_asympt_stu)*sensitivity*I_on - 1/infectious*I_on - screening_on*I_on*sensitivity)
    dR_on <- 1/infectious*I_on + 1/isolation*P_on + 1/isolation*Q_on*((R0_on_to_on+R0_student_to_student)/contacts)
    dP_on <- testing*(1-p_asympt_stu)*sensitivity*I_on + screening_on*(E_on+I_on)*sensitivity - 1/isolation*P_on
    #dQ_on <- testing*(1-p_asympt_stu)*I_on*sensitivity*contacts*p_contacts_reached  - 1/isolation*Q_on
    dQ_on <- testing*(1-p_asympt_stu)*I_on*sensitivity*contacts*p_contacts_reached  + screening_on*(E_on+I_on)*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_on
    dIcum_on = (1-p_asympt_stu)*(lam_on*S_on + community*S_on)
    dIcum_on_camp <- (1-p_asympt_stu) * lam_on*S_on
    #dIcum_on_com <- (1-p_asympt_stu) *community * S_on
    dPcum_on <- testing*(1-p_asympt_stu)*I_on*sensitivity + screening_on*(E_on+I_on)*sensitivity
    dQcum_on <- testing*(1-p_asympt_stu)*I_on*sensitivity*contacts*p_contacts_reached + screening_on*(E_on+I_on)*sensitivity*contacts*p_contacts_reached
    dHcum_on <- (p_hosp_stu)*(lam_on*S_on + community*S_on)
    dDcum_on <- p_death_stu*(lam_on*S_on + community*S_on)
    # Off campus students
    lam_off = (1-eff_npi)*(beta_student_to_student*(I_off+I_on) + beta_saf*I_saf)
    #dS_off <- -lam_off*S_off - community*S_off - testing*(1-p_asympt_stu)*I_off*sensitivity*(contacts - R0_student_to_student)*p_contacts_reached + 1/isolation*Q_off*((contacts-R0_student_to_student)/contacts)
    dS_off <- -lam_off*S_off - community*S_off - (testing*(1-p_asympt_stu)*I_off+screening*(E_off+I_off))*sensitivity*(contacts - R0_student_to_student)*p_contacts_reached + 1/isolation*Q_off*((contacts-R0_student_to_student)/contacts)
    #dE_off <-  lam_off*S_off + community*S_off - (1/latent)*E_off - screening*E_off*sensitivity - testing*(1-p_asympt_stu)*I_off*sensitivity*(R0_student_to_student)*p_contacts_reached
    dE_off <-  lam_off*S_off + community*S_off - (1/latent)*E_off - screening*E_off*sensitivity - (testing*(1-p_asympt_stu)*I_off+screening*(E_off+I_off))*sensitivity*(R0_student_to_student)*p_contacts_reached
    dI_off <- 1/latent*E_off - testing*(1-p_asympt_stu)*I_off*sensitivity - 1/infectious*I_off - screening*I_off*sensitivity
    dIsym_off <- (1-p_asympt_stu)*(1/latent*E_off - testing*(1-p_asympt_stu)*I_off*sensitivity - 1/infectious*I_off - screening*I_off*sensitivity)
    dR_off <- 1/infectious*I_off + 1/isolation*P_off + 1/isolation*Q_off*(R0_student_to_student/contacts)
    dP_off <- testing*(1-p_asympt_stu)*I_off*sensitivity - 1/isolation*P_off + screening*(E_off+I_off)*sensitivity
    #dQ_off <- testing*(1-p_asympt_stu)*I_off*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_off
    dQ_off <- testing*(1-p_asympt_stu)*I_off*sensitivity*contacts*p_contacts_reached + screening*(E_off+I_off)*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_off
    dIcum_off <- (1-p_asympt_stu)*(lam_off*S_off + community*S_off)
    dIcum_off_camp <- (1-p_asympt_stu) * lam_off*S_off
    #dIcum_off_com <- (1-p_asympt_stu) *community * S_off
    dPcum_off <- testing*(1-p_asympt_stu)*I_off*sensitivity + screening*(E_off+I_off)*sensitivity
    dQcum_off <-testing*(1-p_asympt_stu)*I_off*sensitivity*contacts*p_contacts_reached + screening*(E_off+I_off)*sensitivity*contacts*p_contacts_reached
    dHcum_off <- (p_hosp_stu)*(lam_off*S_off + community*S_off)
    dDcum_off <- p_death_stu * (lam_off*S_off + community*S_off)
    # Staff and faculty
    lam_saf = (1-eff_npi)*(beta_saf*(I_on+I_off+I_saf))
    #dS_saf <- -lam_saf*S_saf - community*S_saf - testing*(1-p_asympt_saf)*I_saf*sensitivity*(contacts-R0_saf)*p_contacts_reached + 1/isolation*Q_saf*((contacts-R0_saf)/contacts)
    dS_saf <- -lam_saf*S_saf - community*S_saf - (testing*(1-p_asympt_saf)*I_saf+screening*(E_saf+I_saf))*sensitivity*(contacts-R0_saf)*p_contacts_reached + 1/isolation*Q_saf*((contacts-R0_saf)/contacts)
    #dE_saf <-  lam_saf*S_saf + community*S_saf - 1/latent*E_saf - screening*E_saf*sensitivity - testing*(1-p_asympt_saf)*I_saf*sensitivity*(R0_saf)*p_contacts_reached
    dE_saf <-  lam_saf*S_saf + community*S_saf - 1/latent*E_saf - screening*E_saf*sensitivity -(testing*(1-p_asympt_saf)*I_saf+screening*(E_saf+I_saf))*sensitivity*(R0_saf)*p_contacts_reached
    dI_saf <- 1/latent*E_saf - testing*(1-p_asympt_saf)*I_saf*sensitivity- 1/infectious*I_saf - screening*I_saf*sensitivity
    dIsym_saf <- (1-p_asympt_saf)*(1/latent*E_saf - testing*(1-p_asympt_saf)*I_saf*sensitivity- 1/infectious*I_saf - screening*I_saf*sensitivity)
    dR_saf <- 1/infectious*I_saf + 1/isolation*P_saf + 1/isolation*Q_saf*(R0_saf/contacts)
    dP_saf <- testing*(1-p_asympt_saf)*I_saf*sensitivity - 1/isolation*P_saf + screening*(E_saf+I_saf)*sensitivity
    #dQ_saf <- testing*(1-p_asympt_saf)*I_saf*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_saf
    dQ_saf <- testing*(1-p_asympt_saf)*I_saf*sensitivity*contacts*p_contacts_reached - 1/isolation*Q_saf + screening*(E_saf+I_saf)*sensitivity*contacts*p_contacts_reached
    dIcum_saf = (1-p_asympt_saf)*(lam_saf*S_saf + community*S_saf)
    dIcum_saf_camp <- (1-p_asympt_saf) * lam_saf*S_saf
    #dIcum_saf_com <- (1-p_asympt_saf) *community * S_saf
    dPcum_saf <- testing*(1-p_asympt_saf)*lam_saf*S_saf*sensitivity
    dHcum_saf <- p_hosp_saf*(lam_saf*S_saf + community*S_saf)
    dDcum_saf <- p_death_saf*(lam_saf*S_saf + community*S_saf)
    #Calculated outputs
    dTest <- (N_off+N_saf)*screening + N_on*screening_on+((I_on+I_off+I_saf)*testing) + N*ili*ifelse(testing > 0, 1, 0) #diagnostics performed
    dCase_stu =   (1-p_asympt_saf)*(1/latent*E_on - testing*(1-p_asympt_stu)*sensitivity*I_on - 1/infectious*I_on - screening*I_on*sensitivity +
                                      1/latent*E_off - testing*(1-p_asympt_stu)*I_off*sensitivity - 1/infectious*I_off - screening*I_off*sensitivity)
    state_list <- c(dS_on,dE_on,dI_on,dIsym_on, dP_on,dR_on,dIcum_on, dPcum_on, dQ_on, dQcum_on, dHcum_on, dDcum_on,  dIcum_on_camp,
                    dS_off,dE_off,dI_off,dIsym_off,dP_off,dR_off,dIcum_off, dPcum_off, dQ_off, dQcum_off, dHcum_off, dDcum_off,dIcum_off_camp,
                    dS_saf,dE_saf,dI_saf,dIsym_saf, dP_saf,dR_saf,dIcum_saf,dPcum_saf, dQ_saf, dHcum_saf, dDcum_saf, dIcum_saf_camp,
                    dTest,lam_on,lam_off,lam_saf)
    out <- list(state_list)
    return(out)
  })
}

model_scenarios<- function(testing =0, screening=0, screening_on=0,p_contacts_reached = p_contacts_reached.int,
                           sensitivity_input=sensitivity.int, r.int =1,eff_npi.int=eff_npi.int) {
  param <- param.dcm(latent = latent.int,
                     infectious = infectious.int,
                     isolation = isolation,
                     beta_student_to_student = r.int*beta_student_to_student.int,
                     beta_on_to_on = r.int*beta_on_to_on.int,
                     beta_saf = r.int*beta_saf.int,
                     community = community.int,
                     p_asympt_stu = p_asympt_stu.int,
                     p_asympt_saf = p_asympt_saf.int,
                     p_hosp_stu = p_hosp_stu.int,
                     p_hosp_saf = p_hosp_saf.int,
                     p_death_stu = p_death_stu.int,
                     p_death_saf = p_death_saf.int,
                     contacts = contacts.int,
                     ili = ili.int,
                     sensitivity = sensitivity_input,
                     eff_npi = eff_npi.int,
                     testing = testing,
                     screening = screening,
                     screening_on=screening_on,
                     p_contacts_reached = p_contacts_reached)
  
  mod <- dcm(param, init, control)
  mod <- mutate_epi(mod, I_stu = Isym_on + Isym_off,
                    Icum_stu = Icum_on + Icum_off,
                    P_stu = P_on + P_off,
                    Pcum_stu = Pcum_on + Pcum_off,
                    Qcum_stu = Qcum_on + Qcum_off,
                    Hcum_stu = Hcum_on + Hcum_off,
                    Dcum_stu = Dcum_on + Dcum_off)
  
  return(as.data.frame(mod))
  
}

getcases <- function(x){
    x%>%
    group_by(time) %>%
    summarize(med_stud_active = quantile(I_stu, 0.5, na.rm = TRUE),
              med_stud_cum = quantile(Icum_stu, 0.5, na.rm = TRUE),
              med_saf_active = quantile(I_saf, 0.5, na.rm =TRUE),
              med_saf_cum = quantile(Icum_saf,0.5,na.rm=TRUE))
}