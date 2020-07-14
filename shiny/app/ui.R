source("R/tools.R")

ui <- dashboardPage(
  ## Header content
  dashboardHeader(title = "COVID University Model"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "text", icon = icon("info")),
      menuItem("Main Scenarios", tabName = "main", icon = icon("dashboard")),
      menuItem("Sensitivity Analyses",
               tabName = "sens_ana", icon = icon("bar-chart")),
      menuItem("Raw Model Parameters", tabName = "raw_params", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "text",
        uiOutput("introductionText")
      ),
      tabItem(
        tabName = "main",
        uiOutput("ui_main")
      ),
      tabItem(
        tabName = "sens_ana",
        uiOutput("ui_sens")
      ),
      tabItem(
        tabName = "raw_params",
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL, title = name2lab("base_raw", all_labs),
              status = "primary", solidHeader = TRUE,

              uiOutput("rawParamText"),
              actionButton("btn_reload", "Reload the App and Reset All Values"),
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL, title = name2lab("base_init", all_labs),
              status = "primary", solidHeader = TRUE,

              column(
                width = 4,

                numericInput("N_on", name2lab("N_on", all_labs), 4500),
                numericInput("E_on", name2lab("E_on", all_labs), 0),
                numericInput("I_on", name2lab("I_on", all_labs), 0),
                numericInput("Isym_on", name2lab("Isym_on", all_labs), 0),
                numericInput("P_on", name2lab("P_on", all_labs), 0),
                numericInput("R_on", name2lab("R_on", all_labs), 0),
                numericInput("Q_on", name2lab("Q_on", all_labs), 0)
              ),
              column(
                width = 4,

                numericInput("N_off", name2lab("N_off", all_labs), 10500),
                numericInput("E_off", name2lab("E_off", all_labs), 0),
                numericInput("I_off", name2lab("I_off", all_labs), 0),
                numericInput("Isym_off", name2lab("Isym_off", all_labs), 0),
                numericInput("P_off", name2lab("P_off", all_labs), 0),
                numericInput("R_off", name2lab("R_off", all_labs), 0),
                numericInput("Q_off", name2lab("Q_off", all_labs), 0),
                numericInput("Test", name2lab("Test", all_labs), 0)
              ),
              column(
                width = 4,

                numericInput("N_saf", name2lab("N_saf", all_labs), 20000),
                numericInput("E_saf", name2lab("E_saf", all_labs), 0),
                numericInput("I_saf", name2lab("I_saf", all_labs), 0),
                numericInput("Isym_saf", name2lab("Isym_saf", all_labs), 0),
                numericInput("P_saf", name2lab("P_saf", all_labs), 0),
                numericInput("R_saf", name2lab("R_saf", all_labs), 0),
                numericInput("Q_saf", name2lab("Q_saf", all_labs), 0)

              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12, title = name2lab("base_param", all_labs),
                status = "primary", solidHeader = TRUE,

                column(
                  width = 4,

                  numericInput("R0_student_to_student",
                               name2lab("R0_student_to_student", all_labs), 2.0),
                  numericInput("R0_on_to_on",
                               name2lab("R0_on_to_on", all_labs), 1.0),
                  numericInput("R0_saf",
                               name2lab("R0_saf", all_labs), 0.5),
                  numericInput("latent", name2lab("latent", all_labs), 3),
                  numericInput("infectious", name2lab("infectious", all_labs), 7),
                  numericInput("isolation", name2lab("isolation", all_labs), 14),
                  numericInput("community",
                               name2lab("community", all_labs), 33)
                ),
                column(
                  width = 4,

                  numericInput("p_asympt_stu",
                               name2lab("p_asympt_stu", all_labs), 0.65),
                  numericInput("p_hosp_stu",
                               name2lab("p_hosp_stu", all_labs), 0.0224),
                  numericInput("p_death_stu",
                               name2lab("p_death_stu", all_labs), 0.0006),
                  numericInput("p_asympt_saf",
                               name2lab("p_asympt_saf", all_labs), 0.49),
                  numericInput("p_hosp_saf",
                               name2lab("p_hosp_saf", all_labs), 0.055),
                  numericInput("p_death_saf",
                               name2lab("p_death_saf", all_labs), 0.0052),
                  numericInput("ili", name2lab("ili", all_labs), 0.00333)
                ),
                column(
                  width = 4,

                  numericInput("contacts", name2lab("contacts", all_labs), 14),
                  numericInput("p_contacts_reached",
                               name2lab("p_contacts_reached", all_labs), 0.75),
                  ## numericInput("testing", name2lab("testing", all_labs), 0),
                  ## numericInput("screening", name2lab("screening", all_labs), 0),
                  numericInput("sensitivity",
                               name2lab("sensitivity", all_labs), 0.8),
                  numericInput("eff_npi",
                               name2lab("eff_npi", all_labs), 0.3),
                  sliderInput("nsteps", name2lab("nsteps", all_labs), 0, 365, 180)
                )
              )
            )
          )
        )
      )
    )
  )
)

ui
