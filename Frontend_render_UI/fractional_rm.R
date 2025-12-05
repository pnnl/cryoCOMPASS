observeEvent(input$check_factorial,{
  req(input$check_factorial)
  req(allVars())
  req(input$response_names)
  req(input$num_replicates, input$num_samples)
  
  allvar = allVars()
  
  fact_levels <- purrr::map(allvar,function(onevar){
    # get the factor levels
    possible_levels = stringr::str_split_1(onevar$values,"\n")
    # in case people accidentally do another space
    possible_levels <- possible_levels[possible_levels != ""]
  })
  
  var_names <- purrr::map_chr(allvar, function(onevar){
    return(onevar$name)
  })
  
  factor_type <- purrr::map_chr(allvar, function(onevar){
    return(onevar$type)
  })
  
  # will need to add a check to make sure that num_replicates can be evenly divided into num_samples
  # determine number of unique trials we want
  num_samples = as.numeric(input$num_samples)
  num_replicates = as.numeric(input$num_replicates)
  num_trials = num_samples/num_replicates
  
  # make the expanded grid
  f.design <- expand.grid(fact_levels)
  names(f.design) <- var_names
  
  # get the degrees of freedom for each factor
  df_var <- purrr::map_dbl(fact_levels, function(onefact){
    return(length(onefact) - 1)
  })
  
  nTrials_minimum = sum(1,df_var)
  nTrials_maximum = nrow(f.design)
  
  # add response columns
  response_names <- stringr::str_split_1(input$response_names,"\n")
  response_names <- response_names[response_names != ""]

  # determine if numeric columns are incorrectly classified
  numeric_problems <- purrr::pmap_lgl(list(var_names,fact_levels,factor_type),function(var_name,fact_level,fact_type){
    if(fact_type == "Numeric"){
      fact_level_updated = as.numeric(fact_level)
    } else {
      fact_level_updated = as.character(fact_level)
    }
    
    num_problems <- ifelse(any(is.na(fact_level_updated)),TRUE,FALSE)
    return(num_problems)
  })
  
  if(any(numeric_problems)){
    # if we have numeric values correctly specified create a separate message for this issue
    p_message <- purrr::map2_chr(allvar, fact_levels, function(onevar, onefact){
      p_mess = paste0("Variable Name: ", onevar$name, " - ", onevar$type, " with values ", paste0(onefact, collapse = ", "))
    })
    
    bad_vars = var_names[numeric_problems]
    p_errors = paste0("The following variable(s) were categorized as numeric, but contained non-numeric factor levels: ", paste0(bad_vars,collapse = ", "))
    
    concated_message = paste(paste0(p_message, collapse = "\n"),p_errors,sep = "\n")
    message_type = "error"
  } else {
    # otherwise we do the standard message system
    # add message to shiny alert
    p_message <- purrr::map2_chr(allvar, fact_levels, function(onevar, onefact){
      p_mess = paste0("Variable Name: ", onevar$name, " - ", onevar$type, " with values ", paste0(onefact, collapse = ", "))
    })
    
    p_response = paste0("There are ", length(response_names), " desired responses: ", paste0(response_names, collapse = ", "))
    
    p_trials = paste0("Given these predictors, the number of possible distinct conditions are: ", nTrials_maximum)
    p_replicates = paste0("You have selected ", num_samples, " samples and ", num_replicates, " replicates per sample indicating a total of ", num_samples, "/", num_replicates, " = ", num_trials, " unique conditions")
    p_space = paste0("")
    p_errors = ifelse(num_trials < nTrials_minimum,paste0("This is too low for this data given that there are ", nTrials_minimum-1, " degrees of freedom in this analysis. Please ensure that the number of unique trials is greater than or equal to the number of degrees of freedom plus 1, which in this case is ", nTrials_minimum),
                      ifelse(num_trials > nTrials_maximum,paste0("This number is too high for this dataset given the number of predictors combined with the number of factor levels for each predictor. Please ensure that the number of unique conditions is no more than the possible number of distinct conditions, which in this case is ", nTrials_maximum),
                             ""))
    p_warnings = ifelse(num_samples %% num_replicates != 0 & p_errors == "",
                        paste0("Warning: The number of replicates does not fit evenly into the specified desired number of samples. 
                        CryoCOMPASS will round the total number of samples down to the highest number below the given number into which the 
                        number of replicates will divide evenly. For this instance, there will be ", num_samples - (num_samples %% num_replicates), " total samples (instead of the desired ", num_samples, ")."),
                        "")
    
    message_type = ifelse(p_errors != "","error",
                          ifelse(p_warnings != "", "warning","info"))
    concated_message = paste(  paste0(p_message, collapse = "\n"),p_response,p_trials,p_space,p_replicates,p_space,p_errors,p_warnings,sep = "\n")
  }
  
  if(message_type != "error"){
    make_table_good(TRUE)
    enable("go_factorial")
  } else {
    make_table_good(FALSE)
    disable("go_factorial")
  }
  
  shinyalert("Notes",paste(concated_message), type = message_type)
})

fractional_df <- eventReactive(input$go_factorial,{
  req(input$go_factorial)
  req(allVars())
  req(input$response_names)
  req(input$num_replicates, input$num_samples)
  
  allvar = allVars()
  
  fact_levels <- purrr::map(allvar,function(onevar){
    # get the factor levels
    possible_levels = stringr::str_split_1(onevar$values,"\n")
    # in case people accidentally do another space
    possible_levels <- possible_levels[possible_levels != ""]
  })
  
  var_names <- purrr::map_chr(allvar, function(onevar){
    return(onevar$name)
  })
  
  # will need to add a check to make sure that num_replicates can be evenly divided into num_samples
  # determine number of unique trials we want
  num_samples = as.numeric(input$num_samples)
  num_replicates = as.numeric(input$num_replicates)
  num_trials = num_samples/num_replicates
  
  # make the expanded grid
  f.design <- expand.grid(fact_levels)
  names(f.design) <- var_names
  
  # get the degrees of freedom for each factor
  df_var <- purrr::map_dbl(fact_levels, function(onefact){
    return(length(onefact) - 1)
  })

  # get optimized design
  opt.design <- optFederov(data = f.design, nTrials = num_trials)
  opt.design_df <- opt.design$design
  
  # add in the replicates
  opt.design_df <- opt.design_df[rep(1:nrow(opt.design_df), each = num_replicates),]
  rownames(opt.design_df) <- c(1:nrow(opt.design_df))
  
  # add response columns
  response_names <- stringr::str_split_1(input$response_names,"\n")
  response_names <- response_names[response_names != ""]
  
  # Function to add multiple blank columns to a data frame
  add_blank_columns <- function(df, new_columns) {
    for (col in new_columns) {
      df[[col]] <- NA
    }
    return(df)
  }
  
  # Add new columns to the data frame
  opt.design_df <- add_blank_columns(opt.design_df, response_names)
  # convert numeric columns to be numeric
  var_types <- sapply(allvar, function(df) df[["type"]])
  var_names <- sapply(allvar, function(df) df[["name"]])
  
  if("Numeric" %in% var_types){
    numeric_names <- var_names[var_types == "Numeric"]
    opt.design_df <- convert_columns_to_numeric(opt.design_df, numeric_names)
    
  }
  
  disable("go_factorial")
  
  return(opt.design_df)
})

output$design_gt <- render_gt({
  req(fractional_df())  # Ensure there is JSON data
  fractional_df() %>% gt()
})

output$download_design_table <- renderUI({
  req(fractional_df())
  downloadButton("download_design_button",label = "Download Design Table as Excel")
})


output$download_design_button <- downloadHandler(
  filename = function() {
    paste0("cryo_em_fractional_factorial_design_", Sys.time(),".xlsx")
  },
  content = function(file) {
    write_xlsx(fractional_df(), file)  
  }
)


# Generate input fields for each variable
output$variableInputs <- renderUI({
  numVars <- input$numVars
  
  lapply(1:numVars, function(i) {
    wellPanel(
      textInput(paste0("varName", i), paste("Variable", i, "Name:")),
      radioGroupButtons(
        paste0("varType", i), "Variable Type:",
        choices = c("Numeric", "Categorical")),
      # selectInput(paste0("varType", i), "Variable Type:",
      #             choices = c("Numeric", "Categorical")),
      uiOutput(paste0("varSpecificInput", i))
    )
  })
})

# Dynamically generate additional inputs based on the variable type
observe({
  lapply(1:input$numVars, function(i) {
    output[[paste0("varSpecificInput", i)]] <- renderUI({
      varType <- input[[paste0("varType", i)]]
      if (varType == "Categorical") {
        textAreaInput(paste0("varValues", i), paste("Variable", i, "For categorical values, please supply all possible values for this variable (each value should be a new line):"), rows = 5)
      } else {
        textAreaInput(paste0("varValues", i), paste("Variable", i, "For numeric variables, please supply two values: the minimum and maximum value possible for this variable (each value should be a new line):"), rows = 5)
      }
    })
  })
})

# Reactive expression to collect all variable information
allVars <- reactive({
  numVars <- input$numVars
  varList <- lapply(1:numVars, function(i) {
    varType <- input[[paste0("varType", i)]]
    if(varType == "Categorical") {
      list(
        name = input[[paste0("varName", i)]],
        type = varType,
        values = unlist(strsplit(input[[paste0("varValues", i)]], ","))
      )
    } else {
      list(
        name = input[[paste0("varName", i)]],
        type = varType,
        values = input[[paste0("varValues", i)]]
      )
    }
  })
})