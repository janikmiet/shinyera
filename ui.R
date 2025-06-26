#
# Shiny Application: Exposure Response Analysis
# This is the user-interface definition of a Exposure Response Analysis Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#

library(shiny)
library(bslib)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles-auth.css")
    ),
    tags$head(
      tags$style(HTML("
    .sidebar .form-group label {
      font-size: 12px !important;
    }
    .sidebar .form-group input, 
    .sidebar .form-group select, 
    .sidebar .form-group textarea {
      font-size: 12px !important;
    }
    .sidebar .checkbox label {
      font-size: 12px !important;
    }
  "))
    ),
    auth_ui(id = "auth"),
    theme = bs_theme(
      bootswatch = "yeti",  # Apply a predefined Bootswatch theme
      primary = "#007bff",   # Customize primary color
      secondary = "#6c757d", # Customize secondary color
      success = "#28a745",   # Customize success color
      base_font = font_google("Roboto"), # Use Google Font
      heading_font = font_google("Poppins"),
      code_font = font_google("Fira Code")
    ),
    page_sidebar(
      title = "Exposure Response Diagnosis Analysis",
      
      ## Inputs -------
      sidebar = sidebar(
        # "Analysis Inputs", 
        width = 500,
        position = "right", 
        card(
          card_header("Exposure Parameters"),
          textInput(inputId = "exposure_icd10", label = "ICD-10:" , value = "^E11"),
          textInput(inputId = "exposure_icd9", label = "ICD-9:" , value = "^250A"),
          textInput(inputId = "exposure_icd8", label = "ICD-8:" , value = "^250"),
          checkboxGroupInput(inputId = "exposure_src", 
                             label = "Registry Sources:", 
                             choices = src_choices, 
                             selected = src_choices, 
                             inline = TRUE,
                             width = "100%"),
          #TODO kun uudet mukaan
          # textInput(inputId = "exposure_extra", label = "Extra regex:" , value = ""),
          # checkboxGroupInput(inputId = "exposure_src_extra", 
          #                    label = "Extra Sources:", 
          #                    choices = src_choices_extra, 
          #                    selected = "",
          #                    inline = TRUE,
          #                    width = "100%"),
          
        ),
        card(
          card_header("Response Parameters"),
          textInput(inputId = "response_icd10", label = "ICD-10:" , value = "^I2[0-5]"),
          textInput(inputId = "response_icd9", label = "ICD-9:" , value = "^41[0-4]"),
          textInput(inputId = "response_icd8", label = "ICD-8:" , value = "^41[0-4]"),
          checkboxGroupInput(inputId = "response_src", 
                             label = "Registry Sources:", 
                             choices = src_choices,
                             selected = src_choices,
                             inline = TRUE,
                             width = "100%"),
          #TODO kun uudet mukaan
          # textInput(inputId = "response_extra", label = "Extra regex:" , value = ""),
          # checkboxGroupInput(inputId = "response_src_extra", 
          #                    label = "Extra Sources:", 
          #                    choices = src_choices_extra, 
          #                    selected = "",
          #                    inline = TRUE,
          #                    width = "100%"),
          
        ),
        card(
          actionButton(inputId = "create", label = "Run")
        )
      ),
      
      navset_pill_list( 
        id = "navbar",
        nav_item(div(textOutput("username"), style = "font-weight: bold; padding-right: 15px;")),
        
        ## Main Page ----
        nav_panel("Main page",
                  card(
                    card_body(
                      uiOutput("about")
                    ),
                  ),
                  card(
                    card_body(
                      uiOutput("info")
                    ),
                  ),
        ),
        ## Exposure --------
        nav_panel("Exposure", 
                  card(
                    card_title("Exposure Analysis"),
                    card_body("Analysis of exposure diagnoses of total population. Input diagnoses regex code (exposure) affects only to this analysis."),
                    card_header("Population "),
                    card_body("Exposure population age distribution on first diagnose date."),
                    plotOutput("plot_exp_ad"),
                    card_header("Table: Exposure Age Distribution Statistics", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    # card_header("Table: Exposure Age Distribution Statistics"),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"tbl_age_distribution_exposure"})
                      )),
                    card_header("Table: Exposure EDU", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body("Education on baseline."),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"tbl_exp_edu"})
                      )),
                    card_header("Table: Exposure BMI", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body("BMI on baseline."),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"tbl_exp_bmi"})
                      )),
                    card_header("Exposure Diagnoses"),
                    card_body("Exposure diagnoses includes following diagnoses. In this table all cases are listed (for example if same person has ICD-8, ICD-9 and ICD-10 diagnose, all those cases will be listed). If there are less than 6 person in diagnose group, those are summarized in XX-group."),
                    # card_header("Table: All Exposure Diagnoses"),
                    card_header("Table: All Exposure Diagnoses", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body(
                      min_height = 500,
                      div(
                        DT::dataTableOutput("tbl_selected_diagnoses")
                      )),
                    card_header("Diagnose Registry Sources"),
                    card_body("Venn diagrams shows how the exposure diagnose codes are found in selected source registries. In first venn plot only first diagnose per person is listed and in the second plot we can find how diagnoses are found in all selected source registries (for example person has same diagnose in cancer and hilmo registries, both are listed)."),
                    card_header("Venn: Diagnoses Registry Overlap (first diagnose)", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_venn1_exposure"),
                    # card_body("Venn diagram shows how the exposure diagnose codes are found in different registries. In this plot only we can see in which registries we can find selected diagnoses per patient."),
                    card_header("Venn: Diagnoses Registry Overlap (whole follow up timeline)", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_venn2_exposure"),
                  ),
        ), 
        ## Response --------
        nav_panel("Response",
                  card(
                    card_title("Response Analysis"),
                    card_body("Analysis of response diagnoses of total population. Diagnose input codes of response (and exposure in case of comparison) affects to this analysis."),
                    card_header("Population "),
                    card_body("Response population age distribution on first diagnose date. Total population size is patients with the response diagnose regardless the exposure diagnose."),
                    plotOutput("plot_resp_ad"),
                    ## TODO siisti tata
                    inputPanel(
                      card_body(radioButtons(inputId = "response_radio", width = "500px",
                                             label = "Show Exposure Groups:", 
                                             choices = c(TRUE, FALSE), selected = FALSE)),
                    ),
                    card_header("Table: Response Age Distribution Statistics", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"tbl_age_distribution_response"})
                      )),
                    card_header("Table: Response EDU", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body("Education on baseline."),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"tbl_resp_edu"})
                      )),
                    card_header("Table: Response BMI", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body("Education on baseline."),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"tbl_resp_bmi"})
                      )),
                    card_header("Response Diagnoses"),
                    card_body("Response diagnoses includes following diagnoses. In this table all cases are listed (for example if same person has ICD-8, ICD-9 and ICD-10 diagnose, all those cases will be listed). If there are less than 6 person in diagnose group, those are summarized in XX-group."),
                    card_header("Table: All response Diagnoses", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body(
                      min_height = 500,
                      div(
                        DT::dataTableOutput("tbl_selected_diagnoses2")
                      )),
                    card_header("Diagnose Registry Sources"),
                    card_body("Venn diagrams shows how the exposure diagnose codes are found in selected source registries. In first venn plot only first diagnose per person is listed and in the second plot we can find how diagnoses are found in all selected source registries (for example person has same diagnose in cancer and hilmo registries, both are listed)."),
                    card_header("Venn: First Diagnose Registry Source", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_venn1_response"),
                    card_header("Venn: Diagnoses Registry Overlap (whole follow up timeline)", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_venn2_response"),
                    card_header("Exposure - Response"),
                    card_body(
                      min_height = 100,
                      div(
                        tableOutput({"summary_exp_resp"})
                      )),
                    card_body(uiOutput("crosstable")),
                  ),
        ), 
        ## Health --------
        nav_panel("Health (ICD-10)", 
                  card(
                    card_title("Health Analysis"),
                    card_body("View for displaying population ICD-10 health profile. This section compares diagnoses (ICD-10) on exposure population to population, which don't have the exposure. Analysis plots diagnoses which are more common for the exposure population."),
                    
                    card_header("ICD-10 Health Profile"),
                    card_body("Spiderplot compares main classes of ICD-10 diagnoses between groups (exposure & non-exposure)."),
                    plotOutput("plot_health_profile"),
                    
                    card_header("Top ICD-10 Diagnoses"),
                    card_body("Analysis of diagnoses which has over 10 % gap between groups (exposure and non-exposure), when diagnoses are aggregated to 3 letters."),
                    plotOutput("plot_icd10_comparison"),
                    card_header("Table: difference between groups (exposure & non-exposure) in ICD-10 diagnoses", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    card_body(
                      min_height = 500,
                      div(
                        DT::dataTableOutput({"tbl_comparison"})
                      )),
                  )), 
        ## Survival --------
        nav_panel("Survival Analysis", 
                  card(
                    card_title("Survival Analysis"),
                    card_body("Survival analysis compares risk to have response diagnose."),
                    inputPanel(
                      card_body(radioButtons(inputId = "newdiag_before", width = "500px",
                                             label = "Remove earlier cases:", 
                                             choices = c(TRUE, FALSE), selected = FALSE)),
                    ),
                    card_body("If option *remove earlier cases* is FALSE, response diagnoses that occurred before exposure diagnose, are set to date 1. If you change the setting, you need to run results again.", style = "font-size: 0.675rem; color: #6c757d; font-weight: normal;"),
                    card_header("Kaplan-Meier"),
                    card_body("Kaplan-Meier survival analysis tells how many will get the response diagnose after the exposure diagnose during the time."),
                    plotOutput("plot_kaplanmeier"),
                    card_header("Competing Risk Analysis"),
                    card_body("Competing Risk analysis compares risk after the exposure diagnose to the death and to the response diagnose."),
                    plotOutput("plot_competingrisk"),
                  ), 
        ), 
        
        ## Cox model ----
        nav_panel("Cox Model",
                  card(
                    card_title("Cox Proportional Hazard Model"),
                    card_body("Cox Proportional Hazard Model compares risk to have response diagnose and estimates effects of the exposure diagnosis and extra parameters (age, bmi, etc.). This analysis is only applied to base cohort (n=14274), because not all necessary variables are available for full cohort. You can compare two different models simultaneously."),
                    ## Section 1: Settings
                    card_header("Model Settings"),
                    card(
                      ## Parameters Settings
                      card_header("Parameters", style = "font-size: 0.775rem; color: #6c757d; font-weight: normal;"),
                      checkboxGroupInput(inputId = "cox1_vars",
                                         label = "Model 1:",
                                         choices = c(cox_spline_vars, cox_normal_vars),
                                         selected = c(cox_spline_vars, cox_normal_vars)[1],
                                         inline = TRUE,
                                         width = "100%"),
                      checkboxGroupInput(inputId = "cox2_vars",
                                         label = "Model 2:",
                                         choices = c(cox_spline_vars, cox_normal_vars),
                                         selected = c(cox_spline_vars, cox_normal_vars)[c(1,2,5)],
                                         inline = TRUE,
                                         width = "100%"),
                      
                    ),
                    ## Reference Classes Settings
                    card(
                      card_header("Reference Classes", style = "font-size: 0.775rem; color: #6c757d; font-weight: normal;"),
                      layout_columns(
                        radioButtons("cox_reference_bmicat1",
                                     label = "bmi_cat1:",
                                     choices = ref_bmi_cat1,
                                     selected = ref_bmi_cat1[2]),
                        radioButtons("cox_reference_bmicat2",
                                     label = "bmi_cat2:",
                                     choices = ref_bmi_cat2,
                                     selected = ref_bmi_cat2[1]),
                        radioButtons("cox_reference_edu",
                                     label = "edu:",
                                     choices = refs_edu,
                                     selected = refs_edu[1]),
                      ),
                    ),
                    ## Plot Settings
                    card(
                      card_header("Plot Settings", style = "font-size: 0.775rem; color: #6c757d; font-weight: normal;"),
                      layout_columns(
                        radioButtons(inputId = "cox_plot_model",
                                     label = "Plotted Model:",
                                     choices = c("Model 1", "Model 2"),
                                     selected = "Model 2"),
                        radioButtons(inputId = "cox_plot_overall",
                                     label = "Overall Survival Plot:",
                                     choices = c("event", "cumhaz", "pct"),
                                     selected = "event"),
                      ),
                    ),
                    actionButton("cox_action", label = "Refresh Models"),
                  ),
                  ## Section 2: Results 
                  card_header("Model Results"),
                  card(
                    ## Cox Model Statistics
                    card(
                      id = "coxcard_model_statistics",
                      card_header("Model 1", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                      verbatimTextOutput("cox_model1"),
                      card_header("Model 2", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                      verbatimTextOutput("cox_model2"),
                    ),
                    card(
                      id = "coxcard_model_survival",
                      card_header("Survival Element", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                      plotOutput("plot_cox_overall"),
                      verbatimTextOutput("stat_cox_n"),
                    ),
                    
                    ## Cox Splines variables
                    card(id = "coxcard_splines_age",
                         card_header("Splines Age", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                         plotOutput("plot_cox_spline_age"),
                    ),
                    card(id = "coxcard_splines_bmi",
                         card_header("Splines BMI", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                         plotOutput("plot_cox_spline_bmi"),
                    ),
                    ## Cox Forestplot
                    card(
                      id = "coxcard_forestplot",
                      card_header("Hazard Ratio Forestplot", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                      card_body(
                        min_height = 600,
                        div(
                          plotOutput("plot_cox_forest", height = 600),
                        )),
                    ),
                    ## Cox Diagnostics
                    card(
                      id = "coxcard_model_diagnostics",
                      card_header("Model Diagnostics", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                      card_body("Test the Proportional Hazards Assumption of a Cox Regression"),
                      verbatimTextOutput("cox_diagnostic_test1"),
                      plotOutput("plot_cox_diagnostics"),
                    ),
                    # ## Cox DEBUG
                    # card(
                    #   id = "coxcard_debug",
                    #   card_header("DEBUG", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    #   verbatimTextOutput("debug_data_cox"),
                    #   # plotOutput("plot_cox_diagnostics"),
                    # ),
                  ),
        ),
        
        ## Poisson --------
        nav_panel("Exposure Response Analysis (Poisson)", 
                  card(
                    # max_height = 1200,
                    # full_screen = TRUE,  
                    card_header("Exposure Response Analysis"),
                    card_body("Using Poisson Regression model to model SIR (Standardized Incidence Ratios). Standardized Incidence Ratio measures whether the response diagnoses were higher or lower than expected within the groups. Follow-up is set so that the same person could be first in non-exposure group and be diagnosed as having a response diagnose, and later while diagnosed to exposure same person could be in exposure group. "),
                    inputPanel(
                      checkboxGroupInput(inputId = "response_extra", label = "Select extra responses:", choices = c("Fractures"), selected = "Fractures"),
                      sliderInput(inputId = "poisson_limits", label = "Incidence Ratio Limits:", min = 0.0, max = 40.0, value = c(0.3, 3), step = .1, dragRange = TRUE)
                    ),
                    
                    card(
                      card_header("Response"),
                      plotOutput("plot_poisson_response1"),
                      plotOutput("plot_poisson_response2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_response"})
                        )),
                      
                      card_header("Mortality"),
                      plotOutput("plot_poisson_mortality1"),
                      plotOutput("plot_poisson_mortality2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_mortality"})
                        )),
                      
                      card_header("Any Fracture"),
                      plotOutput("plot_poisson_fractures_any1"),
                      plotOutput("plot_poisson_fractures_any2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_any"})
                        )),
                      
                      card_header("Ankle Fractures"),
                      plotOutput("plot_poisson_fractures_ankle1"),
                      plotOutput("plot_poisson_fractures_ankle2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_ankle"})
                        )),
                      
                      card_header("Forearm Fractures"),
                      plotOutput("plot_poisson_fractures_forearm1"),
                      plotOutput("plot_poisson_fractures_forearm2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_forearm"})
                        )),
                      
                      card_header("Hip Fractures"),
                      plotOutput("plot_poisson_fractures_hip1"),
                      plotOutput("plot_poisson_fractures_hip2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_hip"})
                        )),
                      
                      card_header("Humerus Fractures"),
                      plotOutput("plot_poisson_fractures_humerus1"),
                      plotOutput("plot_poisson_fractures_humerus2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_humerus"})
                        )),
                      
                      card_header("Vertebral Fractures"),
                      plotOutput("plot_poisson_fractures_vertebral1"),
                      plotOutput("plot_poisson_fractures_vertebral2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_vertebral"})
                        )),
                      
                      card_header("Osteoporotic Fractures"),
                      plotOutput("plot_poisson_fractures_osteoporotic1"),
                      plotOutput("plot_poisson_fractures_osteoporotic2"),
                      card_body(
                        min_height = 500,
                        div(
                          tableOutput({"table_poisson_fractures_osteoporotic"})
                        )),
                    ),
                  ),
        ),
        
        ## Data Availability --------
        nav_panel("Data Availability", 
                  card(
                    card_header("Data Availability"),
                    card_body("For further analysis this section shows what OSTPRE questionnaire data is available on exposure diagnose population."),
                    card_header("All Questionaries", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_datavailability1"),
                    card_header("Original Cohort", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_datavailability2"),
                    card_header("Extra Cohort", style = "font-size: 0.875rem; color: #6c757d; font-weight: normal;"),
                    plotOutput("plot_datavailability3"),
                  ), 
        ),
        
        ## Download Report --------
        nav_panel("Download Report", 
                  card(
                    card_header("Download Report"),
                    card_body("Following analyses can be downloaded here to word or html -format. Recommendation is to use html format, because listing long tables are dynamic. In the Word document long tables are shortened. Settings in Analyses child pages are used to run report, so make sure to check those page if you wish to edit inputs and outputs."),
                    inputPanel(
                      # radioButtons("format", "Document format", c("PDF", "HTML", "Word"), inline = TRUE),
                      checkboxGroupInput(inputId = "report_sections", label = "Report Sections:", choices = c("Exposure", "Response", "Health", "Survival Analysis", "Cox", "Poisson", "Data Avaibility"), selected = c("Exposure", "Response", "Health", "Survival Analysis", "Cox", "Poisson", "Data Avaibility")),
                      radioButtons(
                        inputId = "report_file",
                        label = "Output file:",
                        choices = c("HTML" = "html", "Word" = "docx", "PDF" = "pdf"),
                        selected = "html",
                        inline = TRUE
                      ),
                      downloadButton('ostpre_report')
                    )
                  ), 
        ),
        
        ## User & Logs --------
        # nav_panel("Users & Logs",
        #           card(
        #             card_header("Users and Logs"),
        #             card_body("This section is visible only to site admins. Admin can view other users logs and runs from the log files."),
        # 
        #             card_header("Logs"),
        #             card(
        #               # inputPanel(
        #                 selectInput("user_select", label = "Select User:", choices = all_users, selected = ""),
        #                 ## Solution
        #                 # shinyWidgets::pickerInput("test", NULL, letters, options = list(container = "body")),
        #               # ),
        #             ),
        #             card_body( 
        #               min_height = 500,
        #               div(
        #                 DT::dataTableOutput("tbl_logs")
        #               )),
        #           
        # 
        #             ### TODO this section maybe not
        #             # card_header("Add New User"),
        #             # card_body(""),
        #             # inputPanel(
        #             #   textInput("new_user", label = "User Name:"),
        #             #   textInput("new_password", label = "User Password:"),
        #             # ),
        #             # actionButton("add_user", label = "Add"),
        #             # 
        #             # # TODO Kun käyttäjä deletoidaan, mitä tehdään logeille?
        #             # card_header("Delete User"),
        #             # card_body(""),
        #             # inputPanel(
        #             #   # selectInput("user_delete_select", label = "Select User:", choices = c("admin"),
        #             #             # selected = ""),
        #             #   textInput("delete_user_textinput", label = "Username:")
        #             # ),
        #             # actionButton("delete_user", label = "Delete"),
        #             ),
        # 
        #           ),
        
        ## Materials -------
        nav_menu( 
          "Material",
          "ICD Codes",
          nav_item(
            a("Terveysportti ICD-10 Codes", href = "https://www.terveysportti.fi/apps/icd/", target = "_blank"),
            a("Koodistopalvelu ICD-10 Codes", href = "https://koodistopalvelu.kanta.fi/codeserver/pages/classification-view-page.xhtml?classificationKey=23", target = "_blank"),
            a("ICD-9 Codes", href = "https://www.julkari.fi/handle/10024/131850", target = "_blank"),
            a("ICD-8 Codes", href = "https://www.julkari.fi/handle/10024/135324", target = "_blank"),
          ),
          "Resources",
          nav_item(
            a("regex cheatsheet", href = "https://hypebright.nl/index.php/en/2020/05/25/ultimate-cheatsheet-for-regex-in-r-2/", target = "_blank"),
          ),
          
          "----", 
          "Source Code", 
          nav_item( 
            a("GitHub", href = "https://github.com/janikmiet/shiny_era", target = "_blank") 
          ),
          
        )
      )
    ),
    includeHTML("footer.html")
  ))
