# Tools and prerequisite for the app

# Libraries --------------------------------------------------------------------
library(shiny)
library(shinydashboard)

suppressPackageStartupMessages(library(EpiModel))
library(lhs)

library(dplyr)
library(tidyr)
library(purrr)
library(glue)

library(kableExtra)

library(plotly)
library(ggplot2)

# Utility functions ------------------------------------------------------------
interval2rate <- function(interval) {
  if (interval == 0) 0 else 1 / interval
}

expand_runif <- function(range, rn_vec) {
  diff(range) * rn_vec + range[1]
}

update_init_vals_pattern <- function(pattern, input, session) {
    ids <- keep(names(input), ~ grepl(pattern, .x))
    names <- map_chr(ids, ~ strsplit(.x, pattern)[[1]][2])
    walk2(ids, names, ~ updateNumericInput(session, .x, value = input[[.y]]))
}

kable_sum <- function(df) {
  nms <- names(df)[-1]

  df %>%
    kable("html", col.names = c("", nms)) %>%
    kable_styling(bootstrap_options = c("striped", "condensed")) %>%
    pack_rows("Students", 1, 11) %>%
    pack_rows("Staff and Faculty", 12, 16) %>%
    pack_rows("Tests Performed", 17, 18)
}

name2lab <- function(x, labs) {
  if (!x %in% names(labs)) x else labs[x]
}

# Variables and options --------------------------------------------------------
base_scenario_name <- "Base Model: No Intervention"

theme_set(theme_minimal())

format_nb <- scales::label_number(accuracy = 1, big.mark = ",")
options(
  max.print = 1000,
  digits = 0,
  scipen = 1000
)

# UI strings -------------------------------------------------------------------
compartment_labs <- c(
  "N"    = "Number",
  "S"    = "Susceptible",
  "E"    = "Latent",
  "I"    = "Infectious",
  "P"    = "Isolated",
  "R"    = "Recovered",
  "Icum" = "Infectious (Cumulative)",
  "Pcum" = "Isolated (Cumulative)",
  "Q"    = "Quarantined",
  "Qcum" = "Quarantined (Cumulative)",
  "Hcum" = "Hospitalized (Cumulative)",
  "Dcum" = "Deaths (Cumulative)",
  "Test" = "Tests Performed (Cumulative)"
)

pop_labs <- c(
  "on"  = "On Campus Students",
  "off" = "Off Campus Students",
  "stu" = "All Students",
  "saf" = "Staff and Faculty Members",
  "all" = "Everyone"
)

cp_pop_labs <- map_chr(
  cross2(compartment_labs, pop_labs),
  ~ paste0(.x[[1]], ": ", .x[[2]])
)
names(cp_pop_labs) <- map_chr(
  cross2(names(compartment_labs), names(pop_labs)),
  ~ paste0(.x[[1]], "_", .x[[2]])
)

summ_labs <- c(
  # Summary table
  "student_n"          = "Total at Last Time Step",
  "student_cases"      = "Cummulative Cases",
  "student_cases_peak" = "Peak Incidence",
  "student_hosps"      = "Cummulative Hospitalizations",
  "student_isos"       = "Cummulative Isolations",
  "student_isos_peak"  = "Peak Isolations",
  "student_isos_days"  = "Cummulative Days in Isolation",
  "student_quas"       = "Cummulative Quarantines",
  "student_quas_peak"  = "Peak Quarantines",
  "student_quas_days"  = "Cummulative Days Quarantined",
  "student_deaths"     = "Cummulative Deaths"
)

summ_labs <- c(summ_labs,c(
  "saf_n"          = summ_labs[["student_n"]],
  "saf_cases"      = summ_labs[["student_cases"]],
  "saf_cases_peak" = summ_labs[["student_cases_peak"]],
  "saf_hosps"      = summ_labs[["student_hosps"]],
  "saf_deaths"     = summ_labs[["student_deaths"]],
  "tests"          = "Total",
  "tests_pc"       = "Per Capita"
))

all_labs <- c(
  # Parameters
  "latent"                = "Duration of Latent Period (days)",
  "infectious"            = "Duration of Infectious Period (days)",
  "isolation"             = "Duration of Isolation Period (days)",
  "R0_student_to_student" = "R0 #1: Number of Students Infected by Each Student",
  "R0_on_to_on"           = "R0 #2: Number of Additional Students Infected by Students Living on Campus",
  "R0_saf"                = "R0 #3: Number of Students Infected by Staff/Faculty (and Staff/Faculty Infected by Students)",
  "community"             = "Daily Probability of Community Infection (Not Acquired on Campus)",
  "p_asympt_saf"          = paste0("Proportion of Asymptomatic - ", pop_labs["saf"]),
  "p_asympt_stu"          = paste0("Proportion of Asymptomatic - ", pop_labs["stu"]),
  "p_hosp_stu"            = paste0("Probability of Hospitalization - ", pop_labs["stu"]),
  "p_hosp_saf"            = paste0("Probability of Hospitalization - ", pop_labs["saf"]),
  "p_death_stu"           = paste0("Probability of Death - ", pop_labs["stu"]),
  "p_death_saf"           = paste0("Probability of Death - ", pop_labs["saf"]),
  "p_contacts_reached"    = "Proportion of Contacts reached",
  "contacts"              = "Contacts per Case",
  "ili"                   = "Daily Influenza-Like Illnesses (ILI)",
  "sensitivity"           = "PCR Sensitivity",
  "testing"               = "Daily Testing Rate",
  "screening"             = "Daily Screening Rate",

  # Misc
  "test_int"              = "Test Delay in Days (0 = no Testing)",
  "screen_int"            = "Screening Interval in Days (0 = no Screening)",
  "nsteps"                = "Number of Days to Simulate",
  "Plot_pop"              = "Populations",
  "Plot_measures"         = "Compartments",
  "model_plots"           = "Model Plots",
  "model_summary"         = "Model Summary",
  "model_scenario"        = "Model Options - Intervention Parameters",
  "model_opts"            = "Model Options - Initial Campus Conditions",
  "model_opts_trans"            = "Model Options - Transmission Parameters",
  "dl_btn"                = "Download Raw Data as TSV",
  "model_range"           = "Parameter Ranges",
  "base_raw"              = "Base Model Parameters",
  "base_param"            = "Control - Parameters",
  "base_init"             = "Initial Values",
  "reset_button"         = "Reset Inputs"
)

all_labs <- c(all_labs, cp_pop_labs, pop_labs, compartment_labs, summ_labs)

cp_labs <- compartment_labs[-1]

