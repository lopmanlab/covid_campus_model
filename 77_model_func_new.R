covid_model <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {

    # ODEs
    #compartments: S - susceptible, E - exposed/latent, I - infectious, Isym - symptomatic, R - recovered, P - positive tested (and quarantined), Q - quarantine
    #################################
    # On campus students
    #################################
    
    #force of infection, reduced by effectiveness of NPIs
    lam_on = (1-eff_npi)*(beta_student_to_student*(Iasym_off+Isym_off)/N_off + (beta_student_to_student+beta_on_to_on)*(Iasym_on+Isym_on)/N_on + beta_saf*(Iasym_saf+Isym_saf)/N_saf)

    #susceptible are screened but it has no effect, they are not isolated
    #if a case tests positive, contacts are being isolated. Assumption is that the infectious person already infected R0 others, so those are removed from the E compartment, the rest from here
    dS_on <- -lam_on*S_on - community*S_on - testing*Isym_on*sensitivity*(contacts-Rp_on_to_on-R0_student_to_student)*p_contacts_reached + 1/isolation*Q_on*((contacts-Rp_on_to_on-R0_student_to_student)/contacts)
    
    #exposed, infected but pre-symptomatic and not yet infectious
    #last term represents contract tracing, assuming an infectious person already did all their infecting, those their full R0 of contacts is infected and in this category
    dE_on <-  lam_on*S_on + community*S_on - 1/latent*E_on  - screening*sensitivity*E_on - testing*Isym_on*sensitivity*(Rp_on_to_on+R0_student_to_student)*p_contacts_reached 
    
    #infectious asymomatic and symptomatic. only symptomatic are tested.
    #note that screening and testing are defined as frequency (number of days) in parameter table, in this functions those are the inverse, i.e. the rates of screening and testing
    dIasym_on <- p_asym_stu*1/latent*E_on - 1/infectious*Iasym_on - screening*Iasym_on*sensitivity
    dIsym_on <- p_sympt_stu*1/latent*E_on - 1/infectious*Isym_on - screening*Isym_on*sensitivity - testing*sensitivity*Isym_on  
    
    #positive case is assumed to be isolated and won't further spread
    #one can end up in P by either be symptomatic infected and tested, or in any infection state and screened
    #after isolation, individuals move to R
    dP_on <- testing*sensitivity*Isym_on + screening*sensitivity*(E_on+Isym_on+Iasym_on) - 1/isolation*P_on
    #contact traced and isolated individuals. First term are S individuals, 2nd term are E individuals
    dQ_on <- testing*Isym_on*sensitivity*(contacts-Rp_on_to_on-R0_student_to_student)*p_contacts_reached + testing*Isym_on*sensitivity*(Rp_on_to_on+R0_student_to_student)*p_contacts_reached - 1/isolation*Q_on
    #those that are contact traced and were infected (the E category) move into R, the rest moves back to S
    dR_on <- 1/infectious*Isym_on + 1/infectious*Iasym_on + 1/isolation*P_on + 1/isolation*Q_on*((Rp_on_to_on+R0_student_to_student)/contacts)
    
    #cumulative numbers for different compartments, just for book-keeping/evaluating/plotting
    dIasymcum_on <- p_asym_stu*1/latent*E_on
    dIsymcum_on <-  p_sympt_stu*1/latent*E_on
    dPcum_on <- testing*sensitivity*Isym_on + screening*sensitivity*(E_on+Isym_on+Iasym_on)
    dQcum_on <- testing*Isym_on*sensitivity*(contacts-Rp_on_to_on-R0_student_to_student)*p_contacts_reached + testing*Isym_on*sensitivity*(Rp_on_to_on+R0_student_to_student)*p_contacts_reached 
    #fraction of all those getting infected (flowing into the E compartment) are assumed to be at risk of hospitalization and death
    #H and D compartments are not explicitly tracked in model
    dHcum_on <- (lam_on*S_on + community*S_on)*p_hosp_stu
    dDcum_on <- (lam_on*S_on + community*S_on)*p_death_stu

    #################################
    # Off campus students
    #################################
    #same equations as above for off-campus students. Only difference is reduced transmission
    
    #force of infection, reduced by effectiveness of NPIs
    lam_off = (1-eff_npi)*(beta_student_to_student*(Iasym_off+Isym_off)/N_off + (beta_student_to_student)*(Iasym_on+Isym_on)/N_on + beta_saf*(Iasym_saf+Isym_saf)/N_saf)
    dS_off <- -lam_off*S_off - community*S_off - testing*Isym_off*sensitivity*(contacts-R0_student_to_student)*p_contacts_reached + 1/isolation*Q_on*((contacts-R0_student_to_student)/contacts)
    dE_off <-  lam_off*S_off + community*S_off - 1/latent*E_off  - screening*sensitivity*E_off - testing*Isym_off*sensitivity*(R0_student_to_student)*p_contacts_reached 
    dIasym_off <- p_asym_stu*1/latent*E_off - 1/infectious*Iasym_off - screening*Iasym_off*sensitivity
    dIsym_off <- p_sympt_stu*1/latent*E_off - 1/infectious*Isym_off - screening*Isym_off*sensitivity - testing*sensitivity*Isym_off  
    dP_off <- testing*sensitivity*Isym_off + screening*sensitivity*(E_off+Isym_off+Iasym_off) - 1/isolation*P_off
    dQ_off <- testing*Isym_off*sensitivity*(contacts-R0_student_to_student)*p_contacts_reached + testing*Isym_off*sensitivity*(R0_student_to_student)*p_contacts_reached - 1/isolation*Q_off
    dR_off <- 1/infectious*Isym_off + 1/infectious*Iasym_off + 1/isolation*P_off + 1/isolation*Q_off*((R0_student_to_student)/contacts)
    
    #cumulative numbers for different compartments, just for book-keeping/evaluating/plotting
    dIasymcum_off <- p_asym_stu*1/latent*E_off
    dIsymcum_off <-  p_sympt_stu*1/latent*E_off
    dPcum_off <- testing*sensitivity*Isym_off + screening*sensitivity*(E_off+Isym_off+Iasym_off)
    dQcum_off <- testing*Isym_off*sensitivity*(contacts-R0_student_to_student)*p_contacts_reached + testing*Isym_off*sensitivity*(R0_student_to_student)*p_contacts_reached 
    dHcum_off <- (lam_off*S_off + community*S_off)*p_hosp_stu
    dDcum_off <- (lam_off*S_off + community*S_off)*p_death_stu
    
    #################################
    # staff and faculty  
    #################################
    #same equations as above for off-campus students. Only difference is reduced transmission
    
    #force of infection, reduced by effectiveness of NPIs
    #transmission among staff and from students to staff is assumed to all be at rate beta_saf
    lam_saf = (1-eff_npi)*(beta_saf*(Iasym_on+Isym_on)/N_on + (beta_saf)*(Iasym_off+Isym_off)/N_off + beta_saf*(Iasym_saf+Isym_saf)/N_saf)
    dS_saf <- -lam_saf*S_saf - community*S_saf - testing*Isym_saf*sensitivity*(contacts-R0_saf)*p_contacts_reached + 1/isolation*Q_on*((contacts-R0_saf)/contacts)
    dE_saf <-  lam_saf*S_saf + community*S_saf - 1/latent*E_saf  - screening*sensitivity*E_saf - testing*Isym_saf*sensitivity*(R0_saf)*p_contacts_reached 
    dIasym_saf <- p_asym_stu*1/latent*E_saf - 1/infectious*Iasym_saf - screening*Iasym_saf*sensitivity
    dIsym_saf <- p_sympt_stu*1/latent*E_saf - 1/infectious*Isym_saf - screening*Isym_saf*sensitivity - testing*sensitivity*Isym_saf  
    dQ_saf <- testing*Isym_saf*sensitivity*(contacts-R0_saf)*p_contacts_reached + testing*Isym_saf*sensitivity*(R0_saf)*p_contacts_reached - 1/isolation*Q_saf
    dR_saf <- 1/infectious*Isym_saf + 1/infectious*Iasym_saf + 1/isolation*P_saf + 1/isolation*Q_saf*((R0_saf)/contacts)
    dP_saf <- testing*sensitivity*Isym_saf + screening*sensitivity*(E_saf+Isym_saf+Iasym_saf) - 1/isolation*P_saf
    
    #cumulative numbers for different compartments, just for book-keeping/evaluating/plotting
    dIasymcum_saf <- p_asym_stu*1/latent*E_saf
    dIsymcum_saf <-  p_sympt_stu*1/latent*E_saf
    dPcum_saf <- testing*sensitivity*Isym_saf + screening*sensitivity*(E_saf+Isym_saf+Iasym_saf)
    dQcum_saf <- testing*Isym_saf*sensitivity*(contacts-R0_saf)*p_contacts_reached + testing*Isym_saf*sensitivity*(R0_saf)*p_contacts_reached 
    dHcum_saf <- (lam_saf*S_saf + community*S_saf)*p_hosp_stu
    dDcum_saf <- (lam_saf*S_saf + community*S_saf)*p_death_stu
    
    #Calculated outputs
    #number of tests done - not fully inderstanding this, so setting to 0 for now
    dTest <- 0
    #dTest <- N*screening + ((Isym_on+Isym_off+Isym_saf)*testing) + N*ili*ifelse(testing > 0, 1, 0) #diagnostics performed
                  

    state_list <- c(dS_on,dE_on,dIasym_on,dIsym_on,dP_on,dQ_on,dR_on,dIasymcum_on, dIsymcum_on, dPcum_on, dQcum_on, dHcum_on, dDcum_on,
                    dS_off,dE_off,dIasym_off,dIsym_off,dP_off,dQ_off,dR_off,dIasymcum_off, dIsymcum_off, dPcum_off, dQcum_off, dHcum_off, dDcum_off,
                    dS_saf,dE_saf,dIasym_saf,dIsym_saf,dP_saf,dQ_saf,dR_saf,dIasymcum_saf, dIsymcum_saf, dPcum_saf, dQcum_saf, dHcum_saf, dDcum_saf,
                    dTest)
    out <- list(state_list)
    return(out)
  })
}
