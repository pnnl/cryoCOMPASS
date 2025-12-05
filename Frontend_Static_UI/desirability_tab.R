desirability_tab <- function(){
   div(
      fluidRow(
         tabPanel("Desirability Analysis",
                  sidebarLayout(
                    sidebarPanel(
                      # File Input for uploading CSV or Excel files
                      fileInput("exp_upload", "Choose CSV or Excel File", 
                                accept = c(".csv", ".xlsx", ".xls")),
                      uiOutput("desirability_predictors_UI"),
                      uiOutput("desirability_numeric_predictors_yes_no_UI"),
                      uiOutput("desirability_numeric_predictors_select_UI"),
                      uiOutput("desirability_responses_UI"),
                      #uiOutput("run_analysis_UI"),
                      actionButton("check_stats", "Check Statistical Analysis"),
                      actionButton("go_analysis","Run Statistical Analysis",disabled = TRUE),
                      actionButton("stats_help_btn", "Help: How to Interpret Results")  # New Help button
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        id = "desirability_analysis_tabs",
                        tabPanel("Data",
                                 tableOutput("data_table")
                        ),
                        tabPanel("Linear Regression Model",
                                 gt_output("anova_results_gt"),
                                 gt_output("coefficients_results_gt")),
                        tabPanel("Desirability Plots",
                                 h5("Note: Desirability plots are calculated assuming the responses have a range between -3 and 3 with a desired target of 0. If responses fall outside of that range, some desirability results may not be as accurate."),
                                 gt_output("desirability_optim"),
                                 uiOutput("desirability_plot_tabs_UI")
                                 # uiOutput("desirability_plots_UI"),
                                 # plotlyOutput("desirability_plots")
                                 )
                        )
                      )
                    )
                  )
         ),
      bsModal("htmlModal", "How to Interpret Results", "stats_help_btn", size = "large",
              htmltools::tags$iframe(src = "statistical_interpretation.html", width = '100%',  height = 1000,  style = "border:none;"))
      )
}