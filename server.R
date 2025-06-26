#
# This is the server logic of a Exposure Response Analysis Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Sys.setenv(TMPDIR = "/data/shiny-server/explorer/tmp")
  
  # Authentication -------
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials) 
  )
  
  ## To add user name in app somewhere to display
  output$username <- renderText({auth$user}) # textOutput("user")
  
  # Log handler
  app_log <- eventReactive(input$create, {
    d <- tibble(
      date = Sys.Date(),
      timestamp = Sys.time(),
      user= auth$user,
      exposure_icd10=input$exposure_icd10,
      exposure_icd9=input$exposure_icd9,
      exposure_icd8=input$exposure_icd8,
      response_icd10=input$response_icd10,
      response_icd9=input$response_icd9,
      response_icd8=input$response_icd8)
  })
  observeEvent(input$create,{
    if(!dir.exists("logs/")) dir.create("logs/")
    fil_name <- paste0("logs/", auth$user, ".csv")
    write_csv2(app_log(), file = paste0("logs/", auth$user, ".csv"), append = file.exists(fil_name))
  })
  
  
  # Dataset Creation -----
  
  ## Population definition ------
  dpop <- eventReactive(input$create, {
    healthpopR::classify_population(
      exposure_icd10 = input$exposure_icd10,
      exposure_icd9 = input$exposure_icd9,
      exposure_icd8 = input$exposure_icd8,
      exposure_src = input$exposure_src,
      response_icd10 = input$response_icd10,
      response_icd9 = input$response_icd9,
      response_icd8 = input$response_icd8,
      response_src = input$response_src,
      data_population = population,
      data_diagnoses = diagnoses,
      runtime_shiny = TRUE
    )
  })
  
  ## Exposure diagnoses --------
  exposure_diagnoses <- eventReactive(input$create, {
    healthpopR::search_diagnoses(
      regex_icd10=input$exposure_icd10, 
      regex_icd9=input$exposure_icd9, 
      regex_icd8=input$exposure_icd8, 
      registry_source=input$exposure_src,
      data_diagnoses = diagnoses
    )
  })
  
  ## Response diagnoses ------
  response_diagnoses <- eventReactive(input$create, {
    search_diagnoses(
      regex_icd10=input$response_icd10, 
      regex_icd9=input$response_icd9, 
      regex_icd8=input$response_icd8, 
      registry_source=input$response_src,
      data_diagnoses = diagnoses
    )
  })
  
  
  # Main Page -----
  
  output$about <- renderUI({
    temp_html <- tempfile(fileext = ".html")
    on.exit(unlink(temp_html))  # cleanup
    
    rmarkdown::render(
      input = "ABOUT.md",
      output_format = rmarkdown::html_fragment(),
      output_file = temp_html,
      quiet = TRUE
    )
    
    withMathJax(HTML(paste(readLines(temp_html), collapse = "\n")))
  })
  
  output$info <- renderUI({
    temp_html <- tempfile(fileext = ".html")
    on.exit(unlink(temp_html))  # cleanup
    
    rmarkdown::render(
      input = "INFO.md",
      output_format = rmarkdown::html_fragment(),
      output_file = temp_html,
      quiet = TRUE
    )
    
    withMathJax(HTML(paste(readLines(temp_html), collapse = "\n")))
  })
  
  # Exposure -------
  
  ## Plot: Age Distribution -----
  output$plot_exp_ad <- renderPlot({
    healthpopR::plot_age_distribution(
      data = dpop(),
      group = "exposure",
      subgroups = FALSE,
      colors = colors_groups[c("non-exposure", "exposure")],
      colors_shade = colorspace::lighten(colors_groups[c("non-exposure", "exposure")], amount = .3)
    )
  })
  
  ## Table: Age Distribution -----
  tbl_exp_age <- eventReactive(input$create, {
    healthpopR::table_age_distribution(
      data = dpop(),
      group = "exposure", 
      subgroups = FALSE
    )
  })
  
  ### OUTPUT -----
  output$tbl_age_distribution_exposure <- renderTable({
    tbl_exp_age()
  })
  
  ## Table: pop category variables -----
  tbl_exp_edu <- eventReactive(input$create, {
    tbl_pop_var(dpop(), "exposure", "edu")
  })
  tbl_exp_bmi <- eventReactive(input$create, {
    tbl_pop_var(dpop(), "exposure", "bmi_cat1")
  })
  output$tbl_exp_edu <- renderTable({
    tbl_exp_edu()
  })
  output$tbl_exp_bmi <- renderTable({
    tbl_exp_bmi()
  })
  
  
  ## Table: Selected Diagnoses -----
  selected_diagnoses_exposure <- eventReactive(input$create,{
    healthpopR::table_summary_diagnoses(exposure_diagnoses(), 
                                        group = "exposure", 
                                        sum_small_groups = 6)
  })
  
  ### OUTPUT -----
  output$tbl_selected_diagnoses <- DT::renderDataTable( selected_diagnoses_exposure() , 
                                                        options = list(
                                                          # order = list(6, 'desc'),
                                                          rownames = F
                                                        ))
  ## Plot: SRC Venns --------
  output$plot_venn1_exposure <- renderPlot({
    healthpopR::plot_diagnoses_src(exposure_diagnoses(), per_source = FALSE)
  })
  output$plot_venn2_exposure <- renderPlot({
    healthpopR::plot_diagnoses_src(exposure_diagnoses(), per_source = TRUE)
  })
  
  
  # Response ------
  ## Plot: Age Distribution ------
  output$plot_resp_ad <- renderPlot({
    if(input$response_radio){
      healthpopR::plot_age_distribution(
        data = dpop(),
        group = "response",
        subgroups = input$response_radio,
        colors = unname(colors_groups[c("non-exposure", "exposure")]),
        colors_shade = colorspace::lighten(unname(colors_groups[c("non-exposure", "exposure")]), amount = .3)
      )  
    }else{
      healthpopR::plot_age_distribution(
        data = dpop(),
        group = "response",
        subgroups = input$response_radio,
        colors = unname(colors_groups[c("response")]),
        colors_shade = colorspace::lighten(unname(colors_groups[c("response")]), amount = .3)
      )
    }
    
  })
  
  ## Table: Response Group Age Distribution ------
  tbl_resp_age <- eventReactive(input$create, {
    healthpopR::table_age_distribution(
      data = dpop(),
      group = "response", 
      subgroups = TRUE
    )
  })
  
  ### OUTPUT -----
  output$tbl_age_distribution_response <- renderTable({
    tbl_resp_age()
  })
  
  ## Table: pop category variables
  tbl_resp_edu <- eventReactive(input$create, {
    tbl_pop_var(dpop(), "response", "edu")
  })
  tbl_resp_bmi <- eventReactive(input$create, {
    tbl_pop_var(dpop(), "response", "bmi_cat1")
  })
  output$tbl_resp_edu <- renderTable({
    tbl_resp_edu()
  })
  output$tbl_resp_bmi <- renderTable({
    tbl_resp_bmi()
  })
  
  ## Table: Selected Response diagnoses ------
  selected_diagnoses_response <- eventReactive(input$create,{
    healthpopR::table_summary_diagnoses(response_diagnoses(), 
                                        group = "response", 
                                        sum_small_groups = 6)
  })
  
  ### OUTPUT -----
  output$tbl_selected_diagnoses2 <- DT::renderDataTable( selected_diagnoses_response() , 
                                                         options = list(
                                                           # order = list(6, 'desc'),
                                                           rownames = F
                                                         ))
  ## Plot: Response SRC Venns ------
  output$plot_venn1_response <- renderPlot({
    healthpopR::plot_diagnoses_src(response_diagnoses(), per_source = FALSE)
  })
  output$plot_venn2_response <- renderPlot({
    healthpopR::plot_diagnoses_src(response_diagnoses(), per_source = TRUE)
  })
  
  ## Summary Exp - Resp ------
  tbl_exp_resp <- eventReactive(input$create, {
    healthpopR::summary_exp_resp_order(dpop())
  })
  output$summary_exp_resp <- renderTable({
    tbl_exp_resp()  
  })
  
  ## PLOT EXP < RESP ------
  output$crosstable <- renderUI({
    tab <- healthpopR::summary_exp_resp_crosstabulation(dpop(), output = "viewer")
    tab <- HTML(tab$knitr)
    return(tab)
  })
  
  
  
  # Health -----
  
  ## Table: ICD10 comparison -------
  tbl_comparison <- eventReactive(input$create,{
    healthpopR::tbl_icd10_diff_by_exposure(
      data = dpop(), 
      diagnoses = diagnoses, 
      exposure_icd10 = input$exposure_icd10, 
      exposure_src = input$exposure_src
    )
  })
  ### OUTPUT -----
  output$tbl_comparison <- DT::renderDataTable( tbl_comparison() , 
                                                options = list(
                                                  # order = list(6, 'desc'),
                                                  rownames = F
                                                ))
  ## Plot: Spider Profile ------
  health_icd10_profile <- eventReactive(input$create,{
    healthpopR::classify_icd10_profile(
      data = dpop(), 
      diagnoses = diagnoses, 
      exposure_icd10 = input$exposure_icd10, 
      exposure_src = input$exposure_src
    )
  })
  ### Output ----
  output$plot_health_profile <- renderPlot({
    healthpopR::plot_health_icd10_profile(
      data = health_icd10_profile(),
      colors_exposure_groups = colors_groups[c("non-exposure", "exposure")],
      colors_exposure_groups_shade = colorspace::lighten(colors_groups[c("non-exposure", "exposure")], amount = .3)
    )
  })
  
  ## Plot ICD10 comparison ----
  output$plot_icd10_comparison <- renderPlot({
    # plot_icd10_comparison(tbl_comparison())
    healthpopR::plot_icd10_diff_by_exposure(
      tbl_comparison(),
      limit = 10,
      colors = unname(colorspace::lighten(colors_groups[c("non-exposure", "exposure")], amount = .3)) 
    )
  })
  
  
  
  # Survival -----
  dsurv <- eventReactive(input$create,{
    healthpopR::create_dsurv(
      data= dpop(),
      censoring_date = data_censoring_date,
      filter_early_responses = input$newdiag_before
    )
  })
  
  ## Plot: Kaplan Meier -----
  output$plot_kaplanmeier <- renderPlot({
    healthpopR::plot_survival_km(dsurv())
  })
  
  ## Plot: Competing Risk -----
  output$plot_competingrisk <- renderPlot({
    healthpopR::plot_survival_cr(
      data = dsurv(), 
      colors = unname(colors_groups[c("response", "dead")])
    )
  })
  
  
  
  # Cox Modelling --------
  
  ## Cox Data ---------
  data_cox <- eventReactive(input$cox_action, {
    healthpopR::cox_create_data(
      data = dpop(), 
      data_dates = ostpre_vastpaiv,
      data_socioeconomic = population_variables,
      reference_values = list("bmi_cat1" = input$cox_reference_bmicat1, 
                              "bmi_cat2" = input$cox_reference_bmicat2, 
                              "edu" = input$cox_reference_edu),
      censoring_date = data_censoring_date
    )
    
  })
  
  ## Cox Model 1 & Output -----
  cox_model1 <- eventReactive(input$cox_action, {
    normal_vars <- input$cox1_vars[input$cox1_vars %in% cox_normal_vars]
    spline_vars <- input$cox1_vars[input$cox1_vars %in% cox_spline_vars]
    healthpopR::create_cox_model(
      data = data_cox(),
      normal_vars = normal_vars,
      spline_vars = spline_vars,
      surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure",
      id_var = "ID"
    )
  })
  output$cox_model1 <- renderPrint({
    broom::tidy(cox_model1(), exponentiate = TRUE, conf.int = TRUE) %>%
      filter(!grepl(pattern = "^splines", x = term))
  })
  
  ## Cox Model 2 & Output----
  cox_model2 <- eventReactive(input$cox_action, {
    normal_vars <- input$cox2_vars[input$cox2_vars %in% cox_normal_vars]
    spline_vars <- input$cox2_vars[input$cox2_vars %in% cox_spline_vars]
    healthpopR::create_cox_model(
      data = data_cox(),
      normal_vars = normal_vars,
      spline_vars = spline_vars,
      surv_formula = "Surv(tstart, tstop, diagnose) ~ exposure",
      id_var = "ID"
    )
  })
  output$cox_model2 <- renderPrint({
    broom::tidy(cox_model2(), exponentiate = TRUE, conf.int = TRUE) %>%
      filter(!grepl(pattern = "^splines", x = term))
  })
  
  ## Stats: Patients Freq ------
  output$stat_cox_n <- renderPrint({
    paste0("Patient n: ", length(unique(data_cox()$ID)))
  })
  
  ## Plot: Surv -----
  output$plot_cox_overall <- renderPlot({
    healthpopR::cox_plot_overall(
      data = data_cox(),
      type = input$cox_plot_overall,
      colors_exposure_groups = unname(colors_groups[c("non-exposure", "exposure")]), 
      legend_labels = c("No exposure", "Exposure"),
      conf_int = TRUE,
      risk_table = FALSE
    )
  })
  
  ## Plot: Splines ------
  
  ### Show/Hide Splines ------
  # To check, if checkbox has changed
  cox_helper <- reactive({
    c(input$cox_plot_model, input$cox1_vars, input$cox2_vars)
  })
  observeEvent(cox_helper(), {
    
    shinyjs::hide(id = "coxcard_splines_bmi")
    shinyjs::hide(id = "coxcard_splines_age")
    if(("bmi" %in% c(input$cox1_vars) & input$cox_plot_model == "Model 1") | ("bmi" %in% c(input$cox2_vars) & input$cox_plot_model == "Model 2")){
      shinyjs::show(id = "coxcard_splines_bmi")
    }
    if(("age_bs" %in% c(input$cox1_vars) & input$cox_plot_model == "Model 1") | ("age_bs" %in% c(input$cox2_vars) & input$cox_plot_model == "Model 2")){
      shinyjs::show(id = "coxcard_splines_age")
    }
  })
  
  ### Splines plot AGE -----
  output$plot_cox_spline_age <- renderPlot({
    if( (input$cox_plot_model == "Model 1" & "age_bs" %in% input$cox1_vars) | (input$cox_plot_model == "Model 2" & "age_bs" %in% input$cox2_vars) ){
      if(input$cox_plot_model == "Model 1") d <- cox_model1()
      if(input$cox_plot_model == "Model 2") d <- cox_model2()
      healthpopR::cox_plot_spline(
        d,
        spline_var = "age_bs",
        title = "Spline effect",
        xlab = NULL,
        ylab = "Predicted risk",
        color = "#4C6EF5"
      )
    }
  })
  
  ## Splines plot BMI ----
  output$plot_cox_spline_bmi <- renderPlot({
    if( (input$cox_plot_model == "Model 1" & "bmi" %in% input$cox1_vars) | (input$cox_plot_model == "Model 2" & "bmi" %in% input$cox2_vars) ){
      if(input$cox_plot_model == "Model 1") d <- cox_model1()
      if(input$cox_plot_model == "Model 2") d <- cox_model2()
      healthpopR::cox_plot_spline(
        d,
        spline_var = "bmi",
        title = "Spline effect",
        xlab = NULL,
        ylab = "Predicted risk",
        color = "#4C6EF5"
      )
    }
  })
  
  ### Residual Test ------
  cox_test <- eventReactive(input$cox_action, {
    if(input$cox_plot_model == "Model 1") mdl <- cox_model1()
    if(input$cox_plot_model == "Model 2") mdl <- cox_model2()
    test <- cox.zph(mdl)
    return(test)
  })
  
  ### Table: Residual Test -----
  output$cox_diagnostic_test1 <- renderPrint({
    cox_test()
  })
  
  ## Plot: Forestplot -------
  ## TODO miten saa korkeutta lisaa plottiin?
  output$plot_cox_forest <- renderPlot({
    dat <<- data_cox() ## TODO tämä vähän huono idea, mutt coxph tarvitsee.
    if(input$cox_plot_model == "Model 1") model <- cox_model1()
    if(input$cox_plot_model == "Model 2") model <- cox_model2()
    testmodel <- coxph(as.formula(paste(deparse(model$formula), collapse = " ")),
                       data =  dat)
    ggforest(testmodel)
  })
  
  ## Plot: Diagnostic -----
  output$plot_cox_diagnostics <- renderPlot({
    ggcoxzph(cox_test())
  })
  
  
  
  
  # Poisson ------
  
  ## DG and Mortality
  results1 <- eventReactive(input$create,{
    d1 <- healthpopR::pirr_data(
      d_exposure = exposure_diagnoses(),
      d_response= response_diagnoses(),
      d_population = dpop()
    )
    final <- healthpopR::pirr_results(d1, 
                                      colors=colors_groups[c("non-exposure", "exposure")],
                                      limits =  input$poisson_limits)
    
    return(final)
  })
  
  ## Extra Responses; Fractures
  results2 <- eventReactive(input$create,{
    d2 <- healthpopR::pirr_data(
      d_exposure = exposure_diagnoses(),
      d_response= diagnoses %>% dplyr::filter(DGREG == "FRACTURES"),
      d_population = dpop(),
      dg_list = list(any_fracture = "ankle+forearm+hip+humerus+vertebral",
                     Osteoporotic = "forearm+hip+humerus+vertebral"
      )
    )
    d2[["Death"]] <- NULL
    final <- healthpopR::pirr_results(d2, 
                                      colors=unname(colors_groups[c("non-exposure", "exposure")]),
                                      limits =  input$poisson_limits)
    
    return(final)
  })
  
  ## Diagnoses -------
  output$plot_poisson_response1 <- renderPlot({
    results1()[["DG"]][["plot1"]]
  })
  output$plot_poisson_response2 <- renderPlot({
    results1()[["DG"]][["plot2"]]
  })
  output$table_poisson_response <- renderTable({
    results1()[["DG"]][["table"]]
  })
  
  ## mortality -------
  output$plot_poisson_mortality1 <- renderPlot({
    results1()[["Death"]][["plot1"]]
  })
  output$plot_poisson_mortality2 <- renderPlot({
    results1()[["Death"]][["plot2"]]
  })
  output$table_poisson_mortality <- renderTable({
    results1()[["Death"]][["table"]]
  })
  ## fractures any -------
  output$plot_poisson_fractures_any1 <- renderPlot({
    results2()[["any_fracture"]][["plot1"]]
  })
  output$plot_poisson_fractures_any2 <- renderPlot({
    results2()[["any_fracture"]][["plot2"]]
  })
  output$table_poisson_fractures_any <- renderTable({
    results2()[["any_fracture"]][["table"]]
  })
  ## ankle -------
  output$plot_poisson_fractures_ankle1 <- renderPlot({
    results2()[["ankle"]][["plot1"]]
  })
  output$plot_poisson_fractures_ankle2 <- renderPlot({
    results2()[["ankle"]][["plot2"]]
  })
  output$table_poisson_fractures_ankle <- renderTable({
    results2()[["ankle"]][["table"]]
  })
  ## forearm -------
  output$plot_poisson_fractures_forearm1 <- renderPlot({
    results2()[["forearm"]][["plot1"]]
  })
  output$plot_poisson_fractures_forearm2 <- renderPlot({
    results2()[["forearm"]][["plot2"]]
  })
  output$table_poisson_fractures_forearm <- renderTable({
    results2()[["forearm"]][["table"]]
  })
  ## hip -------
  output$plot_poisson_fractures_hip1 <- renderPlot({
    results2()[["hip"]][["plot1"]]
  })
  output$plot_poisson_fractures_hip2 <- renderPlot({
    results2()[["hip"]][["plot2"]]
  })
  output$table_poisson_fractures_hip <- renderTable({
    results2()[["hip"]][["table"]]
  })
  ## humerus -------
  output$plot_poisson_fractures_humerus1 <- renderPlot({
    results2()[["humerus"]][["plot1"]]
  })
  output$plot_poisson_fractures_humerus2 <- renderPlot({
    results2()[["humerus"]][["plot2"]]
  })
  output$table_poisson_fractures_humerus <- renderTable({
    results2()[["humerus"]][["table"]]
  })
  ## vertebral -------
  output$plot_poisson_fractures_vertebral1 <- renderPlot({
    results2()[["vertebral"]][["plot1"]]
  })
  output$plot_poisson_fractures_vertebral2 <- renderPlot({
    results2()[["vertebral"]][["plot2"]]
  })
  output$table_poisson_fractures_vertebral <- renderTable({
    results2()[["vertebral"]][["table"]]
  })
  ## osteoporotic -------
  output$plot_poisson_fractures_osteoporotic1 <- renderPlot({
    results2()[["Osteoporotic"]][["plot1"]]
  })
  output$plot_poisson_fractures_osteoporotic2 <- renderPlot({
    results2()[["Osteoporotic"]][["plot2"]]
  })
  output$table_poisson_fractures_osteoporotic <- renderTable({
    results2()[["Osteoporotic"]][["table"]]
  })
  
  
  # Data Avaibility ------
  output$plot_datavailability1 <- renderPlot({
    plot_datavaibility_all(dpop())
  })
  
  output$plot_datavailability2 <- renderPlot({
    plot_datavaibility_original(dpop())
  })
  
  output$plot_datavailability3 <- renderPlot({
    plot_datavaibility_extra(dpop())
  })
  
  
  ## Report Download -----
  output$ostpre_report <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".", input$report_file)
    },
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        rfn <- "report_main.Rmd"
        ## Choosing format
        output_format <- switch(input$report_file,
                                docx = "word_document",
                                pdf  = "pdf_document",
                                html = "html_document")
        ## Temp file and copy
        tempReport <- file.path(tempdir(), rfn)
        file.copy(rfn, tempReport, overwrite = TRUE)
        ## TODO here saving to first to temp folder messes up global.R scripts usage. That is why using rfn.
        outfile <- file.path(tempdir(), paste0("report_output.", input$report_file))
        ## Render Report
        rmarkdown::render(rfn,
                          output_format = output_format,
                          output_file = outfile,
                          params = list(report_format = input$report_file,
                                        exposure_icd10 = healthpopR::.regex_clean(input$exposure_icd10),
                                        exposure_icd9 = healthpopR::.regex_clean(input$exposure_icd9),
                                        exposure_icd8 = healthpopR::.regex_clean(input$exposure_icd8),
                                        exposure_src = input$exposure_src,
                                        response_icd10 = healthpopR::.regex_clean(input$response_icd10),
                                        response_icd9 = healthpopR::.regex_clean(input$response_icd9),
                                        response_icd8 = healthpopR::.regex_clean(input$response_icd8),
                                        response_src = input$response_src,
                                        newdiag_before = input$newdiag_before,
                                        report_sections = input$report_sections,
                                        cox1_vars = input$cox1_vars,
                                        cox2_vars = input$cox2_vars,
                                        cox_plot_model = input$cox_plot_model,
                                        cox_plot_overall = input$cox_plot_overall,
                                        cox_reference_bmicat1 = input$cox_reference_bmicat1,
                                        cox_reference_bmicat2 = input$cox_reference_bmicat2,
                                        cox_reference_edu = input$cox_reference_edu,
                                        response_extra = input$response_extra,
                                        poisson_limits = input$poisson_limits,
                                        rendered_by_shiny = TRUE),
                          envir = new.env(parent = globalenv())
        )
        ## Copy file
        file.copy(outfile, file)
      })
    }
  )
  
}) ## END LINE
