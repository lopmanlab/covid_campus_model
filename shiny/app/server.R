source("R/model_func.R")

server <- function(input, output, session) {

# defaults -------------------------------------------------------------------
  param <- reactive({
    beta_student_to_student <- input$R0_student_to_student / input$infectious
    beta_on_to_on <- input$R0_on_to_on / input$infectious
    beta_saf <- input$R0_saf / input$infectious
    N <- input$N_on + input$N_off + input$N_saf

    param.dcm(
      latent                  = input$latent,
      infectious              = input$infectious,
      isolation               = input$isolation,

      R0_on_to_on             = input$R0_on_to_on,
      R0_student_to_student   = input$R0_student_to_student,
      R0_saf                  = input$R0_saf,
      beta_student_to_student = beta_student_to_student,
      beta_on_to_on           = beta_on_to_on,
      beta_saf                = beta_saf,

      community               = input$community,
      p_asympt_stu            = input$p_asympt_stu,
      p_asympt_saf            = input$p_asympt_saf,

      p_hosp_stu              = input$p_hosp_stu,
      p_hosp_saf              = input$p_hosp_saf,

      p_death_stu             = input$p_death_stu,
      p_death_saf             = input$p_death_saf,

      contacts                = input$contacts,
      p_contacts_reached      = input$p_contacts_reached,
      sensitivity             = input$sensitivity,
      testing                 = 0,#input$testing,
      screening               = 0,#input$screening,
      p_asympt_stu            = input$p_asympt_stu,
      p_asympt_saf            = input$p_asympt_saf,
      ili                     = input$ili,
      N                       = N
    )
  })

  init <- reactive({
    N_off <- input$N_stu - input$N_on # Based on number on campus

    init.dcm(
      # S_on must be input that updates with E I R N
      S_on = input$N_on - (input$E_on + input$I_on + input$R_on), # number initially susceptible
      E_on = input$E_on, # number initially incubating
      I_on = input$I_on, # number initially infectious
      P_on = input$P_on, # number initially isolated
      R_on = input$R_on, # initially immune
      Icum_on = 0, # cumulative cases -- for counting incidence
      Pcum_on = 0,
      Q_on = input$Q_on,
      Qcum_on = 0,
      Hcum_on = 0,
      Dcum_on = 0,

      S_off = input$N_off - (input$E_off + input$I_off + input$R_off),
      E_off = input$E_off,
      I_off = input$I_off,
      P_off = input$P_off,
      R_off = input$R_off,
      Icum_off = 0,
      Pcum_off = 0,
      Q_off = input$Q_off,
      Qcum_off = 0,
      Hcum_off = 0,
      Dcum_off = 0,

      S_saf = input$N_saf - (input$E_saf + input$I_saf + input$R_saf),
      E_saf = input$E_saf,
      I_saf = input$I_saf,
      P_saf = input$P_saf,
      R_saf = input$R_saf,
      Icum_saf = 0,
      Pcum_saf = 0,
      Q_saf = input$Q_saf,
      ## Qcum_saf = input$Qcum_saf,
      Hcum_saf = 0,
      Dcum_saf = 0,

      Test = input$Test
    )
  })

  control <- reactive(control.dcm(nsteps = input$nsteps, new.mod = model))

  observeEvent(input$btn_reload, {
    session$reload()
  })

# base -----------------------------------------------------------------------

  res_main <- reactiveVal()
  output$ui_main <- renderUI({
    reset <- input$reset_main
    res_main(runif(1))
    list(
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, title = name2lab("model_plots", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 10,

              plotlyOutput("mainPlot", height = 500)
            ),
            column(
              width = 2,
              checkboxGroupInput(
                "mainPlot_pop", name2lab("Plot_pop", all_labs),
                choiceValues = names(pop_labs),
                choiceNames = unname(pop_labs),
                selected = c("stu", "saf")
              ),
              checkboxGroupInput(
                "mainPlot_measures",
                name2lab("Plot_measures", all_labs),
                choiceValues = names(cp_labs),
                choiceNames = unname(cp_labs),
                selected = c("I", "Icum")
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL, title = name2lab("model_summary", all_labs),
            status = "primary", solidHeader = TRUE,

            tableOutput("mainTable")
          ),
        ),
        column(
          width = 6,
          box(
            width = NULL, title = name2lab("model_scenario", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 6,
              sliderInput(
                "mainPlot_test_int",
                name2lab("test_int", all_labs),
                0, 28, 4
              )
            ),
            column(
              width = 6,
              sliderInput(
                "mainPlot_screen_int",
                name2lab("screen_int", all_labs),
                0, 180, 30
              )
            )
          ),
          box(
            width = NULL, title = name2lab("model_opts", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 6,
              numericInput("baseIni_N_off", name2lab("N_off", all_labs), 0),
              numericInput("baseIni_N_saf", name2lab("N_saf", all_labs), 0)
            ),
            column(
              width = 6,
              numericInput("baseIni_N_on", name2lab("N_on", all_labs), 0),
              sliderInput(
                "baseCon_nsteps",
                name2lab("nsteps", all_labs),
                0, 365, 180
              )
            ),
            # action buttons
            fluidRow(
              column(
                width = 6,
                downloadButton("mainDL", name2lab("dl_btn", all_labs))
              ),
              column(
                width = 6,
                actionButton("reset_main", name2lab("reset_button", all_labs))
              )
            )
          ),
          box(
            width = NULL, title = name2lab("model_opts_trans", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 6,
              numericInput("basePar_R0_student_to_student",
                           name2lab("R0_student_to_student", all_labs), 0),
              numericInput("basePar_R0_saf", name2lab("R0_saf", all_labs), 0)
            ),
            column(
              width = 6,
              numericInput("basePar_R0_on_to_on",
                           name2lab("R0_on_to_on", all_labs), 0),
              numericInput("basePar_community",
                           name2lab("community", all_labs), 0)
            )
          )
        )
      )
    )

  })

  param_base <- reactive({
    params <- param()
    ids <- keep(names(input), ~ grepl("basePar_", .x))
    names <- map_chr(ids, ~ strsplit(.x, "basePar_")[[1]][2])
    params[names] <- map(ids, ~ input[[.x]])

    params$beta_student_to_student <- params$R0_student_to_student / params$infectious
    params$beta_on_to_on <- params$R0_on_to_on / params$infectious
    params$beta_saf <- params$R0_saf / params$infectious
    params$N <- input$baseIni_N_on + input$baseIni_N_off + input$baseIni_N_saf

    params
  })

  init_base <- reactive({
    inits <- init()
    ids <- keep(names(input), ~ grepl("baseIni_", .x))
    names <- map_chr(ids, ~ strsplit(.x, "baseIni_")[[1]][2])
    inits[names] <- map(ids, ~ input[[.x]])

    inits$S_on <- inits$N_on - (inits$E_on + inits$I_on + inits$R_on)
    inits$S_off <- inits$N_off - (inits$E_off + inits$I_off + inits$R_off)
    inits$S_saf <- inits$N_saf - (inits$E_saf + inits$I_saf + inits$R_saf)

    inits[c("N_on", "N_off", "N_saf")] <- NULL
    inits
  })

  control_base <- reactive({
    controls <- control()
    controls$nsteps <- input$baseCon_nsteps

    controls
  })

  df_base <- reactive({
    df <- dcm(param_base(), init_base(), control_base()) %>%
      as_tibble() %>%
      mutate(scenario = base_scenario_name)

    df
  })

  df_interv <- reactive({
    if (input$mainPlot_test_int == 0 && input$mainPlot_screen_int == 0) {
      df <- tibble()
    } else {
      param <- param()
      param$testing <- interval2rate(input$mainPlot_test_int)
      param$screening <- interval2rate(input$mainPlot_screen_int)

      df <- dcm(param, init_base(), control_base()) %>%
        as_tibble() %>%
        mutate(scenario = "Intervention Model")
    }

    df
  })

  df_both <- reactive(bind_rows(df_base(), df_interv()))

  df_summ <- reactive({
    df_cum <- df_both() %>%
      group_by(scenario) %>%
      filter(time == max(time)) %>%
      summarize(
        student_n = S_on + E_on + I_on +  P_on + R_on + Q_on - Dcum_on +
                    S_off + E_off + I_off +  P_off + R_off + Q_off - Dcum_off,
        student_cases = Icum_on + Icum_off,
        student_hosps = Hcum_on + Hcum_off,
        student_isos = Pcum_on + Pcum_off,
        student_quas = Qcum_on + Qcum_off,
        student_deaths = Dcum_on + Dcum_off,
        saf_n = S_saf + E_saf + I_saf +  P_saf + R_saf + Q_saf - Dcum_saf,
        saf_cases = Icum_saf,
        saf_hosps = Hcum_saf,
        saf_deaths = Dcum_saf,
        tests = Test
      ) %>%
      mutate(tests_pc = tests / (student_n + saf_n))

    df_peak <- df_both() %>%
      group_by(scenario) %>%
      summarize(
        student_cases_peak = max(I_on + I_off, na.rm = TRUE),
        student_isos_peak = max(P_on + P_off, na.rm = TRUE),
        student_isos_days = sum(P_on + P_off, na.rm = TRUE),
        student_quas_peak = max(Q_on + Q_off, na.rm = TRUE),
        student_quas_days = sum(Q_on + Q_off, na.rm = TRUE),
        saf_cases_peak = max(I_saf, na.rm = TRUE),
        )

    df_out <- full_join(df_cum, df_peak, by = "scenario") %>%
      pivot_longer(-scenario, names_to = "measure", values_to = "value") %>%
      mutate(value = format_nb(value)) %>%
      pivot_wider(id_cols = measure, names_from = scenario, values_from = value)

    df_out <- left_join(
      tibble(measure = names(summ_labs)),
      df_out,
      by = "measure"
    ) %>%
      mutate(measure = summ_labs[measure])

    df_out
  })

  df_clean <- reactive({
    df_both() %>%
    pivot_longer(-c(time, scenario)) %>%
    separate(name, c("measure", "pop"), sep = "_", fill = "right") %>%
    replace_na(list(pop = "all")) %>%
    pivot_wider(names_from = pop, values_from = value) %>%
    mutate(
      stu = on + off ,
      all = if_else(is.na(all), on + off + saf, all)
    ) %>%
      pivot_longer(- c(time, measure, scenario), names_to = "pop")
  })

  df_plot <- reactive({
    df_clean() %>%
      filter(
        measure %in% input$mainPlot_measures,
        pop %in% input$mainPlot_pop
      ) %>%
      mutate(
        measure = all_labs[measure],
        pop = all_labs[pop]
      )
  })

  output$mainPlot <- renderPlotly({
    p <- ggplot(df_plot(), aes(x = time, y = value,
                               col = scenario, label = measure)) +
      geom_line() +
      facet_grid(rows = vars(measure), cols = vars(pop), scales = "free_y") +
      theme(panel.border = element_rect(color = "black", fill = NA)) +
      xlab("Days") +
      ylab("Value")


    ggplotly(p, tooltip = c("y", "label", "colour", "x")) %>%
      layout(legend = list(
        orientation = "h",
        y = 1.1
      ))
  })

  output$mainDL <- downloadHandler(
    filename = function() {
      paste0("simulation_data.tsv")
    },
    content = function(file) {
      df <- df_clean() %>%
        mutate(
          measure = all_labs[measure],
          pop = all_labs[pop]
        )

      write.table(df, file=file,
                  quote=FALSE, sep='\t', row.names = FALSE)
    }
  )

  output$mainTable <- function() {
    req(df_summ())

    df_summ() %>%
      kable_sum()
  }


  output$introductionText <- renderUI({
    includeMarkdown("introductionText.Rmd")
  })

  observe({
    res_main()
    update_init_vals_pattern("baseIni_", input, session)
    update_init_vals_pattern("basePar_", input, session)
    updateSliderInput(session, "baseCon_nsteps", value = input$nsteps)
  })

# sens -----------------------------------------------------------------------

  res_sens <- reactiveVal()
  output$ui_sens <- renderUI({
    reset <- input$reset_sens
    res_sens(runif(1))
    list(
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, title = "Run simulations",
            status = "primary", solidHeader = TRUE,

            column(
              width = 2,
              p(strong("Run Sensitivity Analysis")),
              actionButton("sens_run", "Run Simulations")
            ),
            column(
              width = 4,
              numericInput("sens_set_size", "Number of Samples", 50)
            ),
            column(
              width = 6,
              uiOutput("sensText")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, title = name2lab("model_plots", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 10,

              plotlyOutput("sensPlot", height = 500),
              ),
            column(
              width = 2,
              checkboxGroupInput(
                "sensPlot_pop",
                name2lab("Plot_pop", all_labs),
                choiceValues = names(pop_labs),
                choiceNames = unname(pop_labs),
                selected = c("stu", "saf")
              ),
              checkboxGroupInput(
                "sensPlot_measures",
                name2lab("Plot_measures", all_labs),
                choiceValues = names(cp_labs),
                choiceNames = unname(cp_labs),
                selected = c("I", "Icum")
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL, title = name2lab("model_summary", all_labs),
            status = "primary", solidHeader = TRUE,

            tableOutput("sensTable")
          ),
          ),
        column(
          width = 6,
          box(
            width = NULL, title = name2lab("model_scenario", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 6,
              sliderInput(
                "sensPlot_test_int",
                name2lab("test_int", all_labs),
                0, 28, 4
              )
            ),
            column(
              width = 6,
              sliderInput(
                "sensPlot_screen_int",
                name2lab("screen_int", all_labs),
                0, 180, 30
              )
            )
          ),
          box(
            width = NULL, title = name2lab("model_opts", all_labs),
            status = "primary", solidHeader = TRUE,

            column(
              width = 6,
              numericInput("sensIni_N_off", name2lab("N_off", all_labs), 0),
              numericInput("sensIni_N_saf", name2lab("N_saf", all_labs), 0),
              downloadButton("sensDL", name2lab("dl_btn", all_labs))
            ),
            column(
              width = 6,
              numericInput("sensIni_N_on", name2lab("N_on", all_labs), 0),
              sliderInput(
                "sensCon_nsteps",
                name2lab("nsteps", all_labs),
                0, 365, 180
              ),
              actionButton("reset_sens", name2lab("reset_button", all_labs))
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, title = name2lab("model_range", all_labs),
            status = "primary", solidHeader = TRUE,
            column(
              width = 3,
              sliderInput(
                "sensRange_R0_student_to_student",
                name2lab("R0_student_to_student", all_labs),
                0, 10, c(0.7, 2.5)
              ),
              sliderInput(
                "sensRange_R0_on_to_on",
                name2lab("R0_on_to_on", all_labs),
                0, 10, c(0.3, 1.4)
              ),
              sliderInput(
                "sensRange_R0_saf",
                name2lab("R0_saf", all_labs),
                0, 10, c(0.15, 0.7)
              ),
              sliderInput(
                "sensRange_community",
                name2lab("community", all_labs),
                0, 0.01, c(0.00025, 0.001)
              )
            ),
            column(
              width = 3,
              sliderInput(
                "sensRange_latent",
                name2lab("latent", all_labs),
                0, 28, c(2, 4)
              ),
              sliderInput(
                "sensRange_infectious",
                name2lab("infectious", all_labs),
                0, 28, c(6, 8)
              ),
              sliderInput(
                "sensRange_contacts",
                name2lab("contacts", all_labs),
                0, 50 , c(4, 24)
              ),
              sliderInput(
                "sensRange_p_contacts_reached",
                name2lab("p_contacts_reached", all_labs),
                0, 1 , c(0.5, 1)
              )
            ),
            column(
              width = 3,
              sliderInput(
                "sensRange_p_asympt_stu",
                name2lab("p_asympt_stu", all_labs),
                0, 1 , c(0.27, 0.43)
              ),
              sliderInput(
                "sensRange_p_hosp_stu",
                name2lab("p_hosp_stu", all_labs),
                0, 0.1 , c(0.01, 0.03)
              ),
              sliderInput(
                "sensRange_p_death_stu",
                name2lab("p_death_stu", all_labs),
                0, 0.01 , c(0.0003, 0.0009)
              ),
              sliderInput(
                "sensRange_sensitivity",
                name2lab("sensitivity", all_labs),
                0, 1 , c(0.75, 0.85)
              )
            ),
            column(
              width = 3,
              sliderInput(
                "sensRange_p_asympt_saf",
                name2lab("p_asympt_saf", all_labs),
                0, 1 , c(0.41, 0.59)
              ),
              sliderInput(
                "sensRange_p_hosp_saf",
                name2lab("p_hosp_saf", all_labs),
                0, 0.1 , c(0.03, 0.07)
              ),
              sliderInput(
                "sensRange_p_death_saf",
                name2lab("p_death_saf", all_labs),
                0, 0.01 , c(0.0029, 0.0075)
              ),
              sliderInput(
                "sensRange_ili",
                name2lab("ili", all_labs),
                0.002, 0.005 , c(0.003, 0.004)
              )
            ),
            fluidRow(
              box(width = 12, uiOutput("sensRangeText"))
            )
          )
        )
      )
    )

  })

  lhs_param <- eventReactive(input$sens_run,{
    range_ids <- names(input)
    range_ids <- range_ids[grep("sensRange_", range_ids)]

    range_inputs <- map(range_ids, ~ input[[.x]])
    range_names <- map_chr(range_ids, ~ strsplit(.x, "sensRange_")[[1]][2])

    lhs_mat <- randomLHS(input$sens_set_size, length(range_inputs))

    range_params <- map2(range_inputs, as.data.frame(lhs_mat), expand_runif)
    names(range_params) <- range_names

    range_params
  })

  control_sens <- reactive({
    controls <- control()
    controls$nsteps <- input$sensCon_nsteps

    controls
  })

  init_sens <- reactive({
    inits <- init()
    ids <- keep(names(input), ~ grepl("sensIni_", .x))
    names <- map_chr(ids, ~ strsplit(.x, "sensIni_")[[1]][2])
    inits[names] <- map(ids, ~ input[[.x]])

    inits$S_on <- inits$N_on - (inits$E_on + inits$I_on + inits$R_on)
    inits$S_off <- inits$N_off - (inits$E_off + inits$I_off + inits$R_off)
    inits$S_saf <- inits$N_saf - (inits$E_saf + inits$I_saf + inits$R_saf)

    inits[c("N_on", "N_off", "N_saf")] <- NULL
    inits
  })

  df_sens <- eventReactive(input$sens_run,{
    df <- tibble()
    nsims <- input$sens_set_size

    for (i in seq_len(nsims)) {
      setProgress(
        value = 1,
        message = "Calculation in progress",
        detail = paste0("Running simulation ", i, " / ", nsims)
      )

      param <- param()

      param[names(lhs_param())] <- map(lhs_param(), ~ .x[[i]])

      df_base <- dcm(param, init_sens(), control_sens()) %>%
        as_tibble() %>%
        mutate(
          scenario = base_scenario_name,
          run = i
        )

      if (input$sensPlot_test_int == 0 && input$sensPlot_screen_int == 0) {
        df_interv <- tibble()
      } else {
        param$testing <- interval2rate(input$sensPlot_test_int)
        param$screening <- interval2rate(input$sensPlot_screen_int)

        df_interv <- dcm(param, init_sens(), control_sens()) %>%
          as_tibble() %>%
          mutate(
            scenario = "Intervention Model",
            run = i
          )
      }

      df <- bind_rows(list(df, df_base, df_interv))
    }

    setProgress(
      2,
      message = "Processing simulations",
      detail = "It may take some time"
    )

    df
  })

  df_sens_summ <- reactive({
    df_cum <- df_sens() %>%
      filter(time == max(time)) %>%
      group_by(scenario, run) %>%
      summarize(
        student_n = S_on + E_on + I_on + R_on + P_on + Q_on - Dcum_on +
                    S_off + E_off + I_off + R_off + P_off + Q_off - Dcum_off,
        student_cases = Icum_on + Icum_off,
        student_hosps = Hcum_on + Hcum_off,
        student_isos = Pcum_on + Pcum_off,
        student_quas = Qcum_on + Qcum_off,
        student_deaths = Dcum_on + Dcum_off,
        saf_n = S_saf + E_saf + I_saf + R_saf + P_saf + Q_saf - Dcum_saf,
        saf_cases = Icum_saf,
        saf_hosps = Hcum_saf,
        saf_deaths = Dcum_saf,
        tests = Test
      ) %>%
      ungroup() %>%
      mutate(tests_pc = tests / (student_n + saf_n))

    df_peak <- df_sens() %>%
      group_by(scenario, run) %>%
      summarize(
        student_cases_peak = max(I_on + I_off, na.rm = TRUE),
        student_isos_peak = max(P_on + P_off, na.rm = TRUE),
        student_isos_days = sum(P_on + P_off, na.rm = TRUE),
        student_quas_peak = max(Q_on + Q_off, na.rm = TRUE),
        student_quas_days = sum(Q_on + Q_off, na.rm = TRUE),
        saf_cases_peak = max(I_saf, na.rm = TRUE),
        ) %>%
      ungroup()

    df_out <- full_join(df_cum, df_peak, by = c("scenario", "run")) %>%
      pivot_longer(
        -c(scenario, run),
        names_to = "measure",
        values_to = "value"
      ) %>%
      group_by(measure, scenario) %>%
      summarize(
        low = quantile(value, 0.025, na.rm = TRUE),
        med = quantile(value, 0.5, na.rm = TRUE),
        high = quantile(value, 0.975, na.rm = TRUE)
      ) %>%
      mutate(value = paste0(
        format_nb(med),
        " (", format_nb(low), " - ",
        format_nb(high), ")")
        ) %>%
      pivot_wider(id_cols = measure, names_from = scenario, values_from = value)

    df_out <- left_join(
      tibble(measure = names(summ_labs)),
      df_out,
      by = "measure"
    ) %>%
      mutate(measure = summ_labs[measure])

    df_out
  })

  df_clean_sens <- reactive({
    df <- df_sens() %>%
      pivot_longer(-c(time, scenario, run)) %>%
      separate(name, c("measure", "pop"), sep = "_", fill = "right") %>%
      replace_na(list(pop = "all")) %>%
      pivot_wider(names_from = pop, values_from = value) %>%
      mutate(
        stu = on + off,
        all = if_else(is.na(all), on + off + saf, all)
      ) %>%
      pivot_longer(- c(time, measure, scenario, run), names_to = "pop") %>%
      group_by(time, measure, scenario, pop) %>%
      summarize(
        low = quantile(value, 0.025, na.rm = TRUE),
        med = quantile(value, 0.5, na.rm = TRUE),
        high = quantile(value, 0.975, na.rm = TRUE)
      )
  })

  slow_sensPlot_measures <- debounce(reactive(input$sensPlot_measures), 2000)
  slow_sensPlot_pop <- debounce(reactive(input$sensPlot_pop), 2000)

  df_plot_sens <- reactive({
    setProgress(3, detail = "Genreating plot")

    df_clean_sens() %>%
      filter(
        measure %in% slow_sensPlot_measures(),
        pop %in% slow_sensPlot_pop()
      ) %>%
      mutate(
        measure = all_labs[measure],
        pop = all_labs[pop]
      )
  })

  output$sensPlot <- renderPlotly({
    withProgress(
      min = 0, max = 4,
      {
        p <- ggplot(
          df_plot_sens(),
          aes(x = time, y = med, ymin = low, ymax = high,
              col = scenario, fill = scenario, label = measure)
        ) +
          geom_line() +
          geom_ribbon(alpha = 0.2, linetype = 0) +
          facet_grid(rows = vars(measure), cols = vars(pop), scales = "free_y") +
          theme(
            panel.border = element_rect(color = "black", fill = NA),
            legend.position = "top"
          ) +
          xlab("Days") +
          ylab("Value")


        setProgress(4, detail = "Rendering plot")
        ggplotly(p, tooltip = c("ymin", "y", "ymax", "label", "colour", "x")) %>%
          layout(legend = list(
            orientation = "h",
            y = 1.1
          ))
      }
    )
  })

  output$sensDL <- downloadHandler(
    filename = function() {
      paste0("sensitivity_data.tsv")
    },
    content = function(file) {
      df <- df_clean_sens() %>%
        mutate(
          measure = all_labs[measure],
          pop = all_labs[pop]
        )

      write.table(df, file=file,
                  quote=FALSE, sep='\t', row.names = FALSE)
    }
  )

  output$sensTable <- function() {
    req(df_sens_summ())

    df_sens_summ() %>%
      kable_sum() %>%
      footnote(general = "Median (95% Simulation Interval)",
               general_title = "Values:",
               footnote_as_chunk = T, title_format = c("italic", "underline"))
  }

  output$sensText <- renderUI({
    includeMarkdown("sensText.Rmd")
  })

  output$sensRangeText <- renderUI({
    includeMarkdown("sensRangeText.Rmd")
  })

  observe({
    res_sens()
    update_init_vals_pattern("sensIni_", input, session)
    updateSliderInput(session, "sensCon_nsteps", value = input$nsteps)
  })

  output$rawParamText <- renderUI({
    includeMarkdown("rawParamText.Rmd")
  })
}
