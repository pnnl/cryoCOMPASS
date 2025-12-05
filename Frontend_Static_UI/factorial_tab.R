factorial_tab <- function(){
   div(
      fluidRow(
         tabPanel(title = "Fractional Factorial Setup",
                  sidebarLayout(
                    sidebarPanel(
                      h3("Experimental Design Setup"),
                      h4("Predictors"),
                      numericInput("numVars", "Number of Predictors:", value = 2, min = 2),
                      uiOutput("variableInputs"),
                      h4("Other Experimental Design Conditions"),
                      tags$span(
                        icon("question-circle"),  # question mark icon
                        id = "name_help"
                      ),
                      bsTooltip("name_help", 
                                "Note: If the desired number of replicates does not fit evenly into the desired number of samples, the CryCOMPASS will round the total number of samples down to the highest number below the given number into which the number of replicates will divide evenly. For example, if the desired number of samples is 13 and the desired number of replicates is 2, the experimental design will include 12 samples with 2 replicates each of 6 unique sets of conditions.",
                                placement = "right", trigger = "hover"),
                      textInput("num_samples","Desired Number of Samples"),
                      textInput("num_replicates", "Desired Number of Replicates"),
                      textAreaInput("response_names", 
                                    "Enter all possible Response Names (each value should be a new line):", 
                                    value = "", 
                                    rows = 5),
                      actionButton("check_factorial","Check Setup for Factorial Design"),
                      actionButton("go_factorial","Run Setup for Factorial Design", disabled = TRUE)
                    ),
                    mainPanel(
                      uiOutput("download_design_table"),
                      gt_output("design_gt")
                    )
                  )


         )
      )
   )
}