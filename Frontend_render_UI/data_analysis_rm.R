
# Reactive expression to read the uploaded file
exp_df <- reactive({
  req(input$exp_upload)  # Ensure the file is uploaded
  
  # Get the file extension
  file_ext <- tools::file_ext(input$exp_upload$name)
  
  # Read the file depending on its extension
  if (file_ext == "csv") {
    read.csv(input$exp_upload$datapath)  # Read CSV
  } else if (file_ext %in% c("xls", "xlsx")) {
    read_excel(input$exp_upload$datapath)  # Read Excel
  }
})

# Render the exp_df table in the UI
output$data_table <- renderTable({
  exp_df()  # Display the reactive data frame of the experiment
})

output$desirability_predictors_UI <- renderUI({
  req(exp_df())
  
  # column names
  col_names <- colnames(exp_df())
  
  selectInput("predictors_columns", "Select Predictor Variables", 
              choices = col_names,
              selected = NULL,
              multiple = TRUE)
})

output$desirability_numeric_predictors_yes_no_UI <- renderUI({
  req(exp_df())
  
  radioGroupButtons("desirability_numeric_predictors_yes_no", "Any Numeric Predictors?", choices = c("Yes","No"), selected = "No")
})

output$desirability_numeric_predictors_select_UI <- renderUI({
  req(exp_df())
  req(input$desirability_numeric_predictors_yes_no == "Yes")
  req(input$predictors_columns)
  
  col_names <- input$predictors_columns
  
  selectInput("predictors_numeric_columns", "Select Numeric Predictors", 
              choices = col_names,
              selected = NULL,
              multiple = TRUE)
})

output$desirability_responses_UI <- renderUI({
  req(exp_df())

  # column names
  col_names <- colnames(exp_df())
  # columns selected as predictors
  predictors <- input$predictors_columns
  # remaining options
  remainingChoices <- setdiff(col_names, predictors)
  selectInput("response_columns", "Select Response Variables: ",
              choices = remainingChoices, selected = NULL,
              multiple = TRUE)
})

observeEvent(input$check_stats,{
  req(exp_df())
  req(input$predictors_columns)
  req(input$response_columns)
  req(input$desirability_numeric_predictors_yes_no)
  
  data_df <- exp_df()
  # set up numeric values where we need 
  if(!is.null(input$predictors_numeric_columns) && length(input$predictors_numeric_columns) > 0){
    data_df <- convert_columns_to_numeric(data_df, input$predictors_numeric_columns)
  }
  
  pred_data_df <- data_df[input$predictors_columns]
  numNA <- colSums(is.na(pred_data_df))
  numeric_problems <- ifelse(numNA > 0, TRUE, FALSE)
  
  # determine class of responses
  response_class_type <- purrr::map_chr(data_df[input$response_columns],class)
  
  if(any(numeric_problems)){
    bad_predictors <- names(numeric_problems[numeric_problems == TRUE])
    p_message <- paste0("The following variables have been classified as numeric, but do not have numeric columns in the table: ", paste0(bad_predictors, collapse = ", "), ". Please adjust accordingly.")
    concated_message = paste(paste0(p_message, collapse = "\n"),sep = "\n")
    message_type = "error"
  } else {
    
    if("character" %in% response_class_type){
      bad_responses <- names(response_class_type)[response_class_type == "character"]
      p_message <- paste0("The following responses do not have numeric values: ", paste0(bad_responses, collapse = ", "), ". Linear regression can only be ran on responses with numeric values. Please adjust accordingly.")
      concated_message = paste(paste0(p_message, collapse = "\n"),sep = "\n")
      message_type = "error"
    } else {
      if(input$desirability_numeric_predictors_yes_no == "Yes" && !is.null(input$predictors_numeric_columns)){
        num_vars <- input$predictors_columns[input$predictors_columns %in% input$predictors_numeric_columns]
        cat_vars <- input$predictors_columns[!input$predictors_columns %in% input$predictors_numeric_columns]
      } else {
        cat_vars <- input$predictors_columns
        num_vars <- c()
      }
      
      if(length(cat_vars) == 0){cat_vars = "None"}
      if(length(num_vars) == 0){num_vars = "None"}
      
      p_categorical <- paste0("Categorical Predictors: ", paste0(cat_vars,collapse = ", "))
      p_numeric <- paste0("Numeric Predictors: ", paste0(num_vars,collapse = ", "))
      p_response <- paste0("Responses: ", paste0(input$response_columns, collapse = ", "))
      
      message_type = "info"
      concated_message = paste(p_categorical,p_numeric,p_response,sep = "\n")
    }
  }
  
  if(message_type != "error"){
    make_stats_good(TRUE)
    enable("go_analysis")
  } else {
    make_stats_good(FALSE)
    disable("go_analysis")
  }
  
  shinyalert("Notes",paste(concated_message), type = message_type)
})

# output$run_analysis_UI <- renderUI({
#   req(exp_df())
#   req(input$predictors_columns, input$response_columns)
#   
#   actionButton("go_analysis","Run Analysis",disabled = TRUE)
# })

# linear regression information
lm_model <- eventReactive(input$go_analysis,{
  req(exp_df())
  req(input$response_columns)
  req(input$predictors_columns)
  req(input$desirability_numeric_predictors_yes_no)
  
  disable("go_analysis")
  
  list_response <- as.list(input$response_columns)
  data_df <- exp_df()
  # first make everything categorical
  # convert to categorical where we need
  data_df <- convert_columns_to_categorical(data_df, input$predictors_columns)
  # set up numeric values where we need 
  if(!is.null(input$desirability_numeric_predictors_yes_no) && (input$desirability_numeric_predictors_yes_no != "No") && !is.null(input$predictors_numeric_columns) && length(input$predictors_numeric_columns) > 0){
    data_df <- convert_columns_to_numeric(data_df, input$predictors_numeric_columns)
  }
  
  
  
  model_lm_list <- purrr::map(list_response, function(resp){
    res = paste0("`",resp,"`")
    
    pred = paste(paste0("`",input$predictors_columns,"`"), collapse = " + ")
    
    model_formula <- as.formula(paste(res, "~",pred))
    
    mod_lm <- lm(model_formula, data = data_df)
  })
  
  summary_lm_list <- purrr::map(list_response, function(resp){
    res = paste0("`",resp,"`")
    
    pred = paste(paste0("`",input$predictors_columns,"`"), collapse = " + ")
    
    model_formula <- as.formula(paste(res, "~",pred))
    
    mod_lm <- lm(model_formula, data = data_df)
    
    summary(mod_lm)
  })
  
  anova_lm_list <- purrr::map(list_response, function(resp){
    res = paste0("`",resp,"`")
    
    pred = paste(paste0("`",input$predictors_columns,"`"), collapse = " + ")
    
    model_formula <- as.formula(paste(res, "~",pred))
    
    mod_lm <- lm(model_formula, data = data_df)
    
    anova_res <- try(Anova(mod_lm, type = "II"),silent = TRUE)
    
    anova_res
  })
  
  model_results <- list(
    model_lm = model_lm_list,
    summary_info = summary_lm_list,
    anova_info = anova_lm_list
  )
})

regression_gt <- eventReactive(input$go_analysis,{
  req(lm_model())
  mod_info = lm_model()
  
  list_response <- as.list(input$response_columns)
  
  df_list <- purrr::map2(mod_info$summary_info, list_response, function(sum_info,lr){
    coef_df <- try(data.frame(sum_info$coefficients) %>%
                     tibble::rownames_to_column(var = "Coefficient") %>%
                     dplyr::rename(SE = Std..Error,
                                   t_value = t.value,
                                   P_value = Pr...t..) %>%
                     dplyr::mutate(Response = lr), silent = TRUE)
    coef_df
  })
  
  if("try-error" %in% map_chr(df_list,class)){
    bad_analysis_nums = grep("try-error",map(df_list,class))
    df_bind <- df_list[-bad_analysis_nums] %>% dplyr::bind_rows()
    if(nrow(df_bind) == 0){
      final_coef_df <- df_bind %>%
        gt::gt() %>%
        gt::tab_caption(caption = paste0("Linear regression could not be ran on the following responses: ", paste(list_response[bad_analysis_nums],collapse = ", "), ". This is likely due to collinearity or one predictor perfectly matching with the response(s) of interest."))
    } else {
      final_coef_df <- df_bind %>%
        dplyr::group_by(Response) %>%
        gt::gt() %>%
        gt::data_color(
          columns = P_value,
          colors = scales::col_bin(
            palette = c("blue", "transparent"),
            bins = c(0, 0.05, 1),
            na.color = "transparent"
          )
        ) %>%
        # gt::fmt_number(
        #   columns = c(Estimate,SE,t_value,P_value),
        #   decimals = 5
        # ) %>%
        gt::tab_caption(caption = paste0("Linear regression could not be ran on the following responses: ", paste(list_response[bad_analysis_nums],collapse = ", "), ". This is likely due to collinearity or one predictor perfectly matching with the response(s) of interest."))
    }
  } else {
    df_bind <- df_list %>% dplyr::bind_rows()
    final_coef_df <- df_bind %>%
      dplyr::group_by(Response) %>%
      gt::gt() %>%
      gt::data_color(
        columns = P_value,
        colors = scales::col_bin(
          palette = c("blue", "transparent"),
          bins = c(0, 0.05, 1),
          na.color = "transparent"
        )
      )# %>%
    # gt::fmt_number(
    #   columns = c(Estimate,SE,t_value,P_value),
    #   decimals = 5
    # )
  }
  final_coef_df %>% gt::tab_header("Regression Coefficients")
})

output$coefficients_results_gt <- render_gt({
  req(lm_model())
  req(regression_gt())
  
  regression_gt()
})

anova_gt <- eventReactive(input$go_analysis,{
  req(lm_model())
  mod_info = lm_model()
  list_response <- as.list(input$response_columns)
  
  df_list <- purrr::map2(mod_info$anova_info, list_response, function(sum_info,lr){
    anova_df <- try(data.frame(sum_info) %>%
                      tibble::rownames_to_column(var = "Coefficient") %>%
                      dplyr::rename(SS = Sum.Sq,
                                    DF = Df,
                                    F_value = F.value,
                                    P_value = Pr..F.) %>%
                      dplyr::mutate(Response = lr), silent = TRUE)
    anova_df
  })
  
  if("try-error" %in% map_chr(df_list,class)){
    bad_analysis_nums = grep("try-error",map(df_list,class))
    df_bind <- df_list[-bad_analysis_nums] %>% dplyr::bind_rows()
    if(nrow(df_bind) == 0){
      final_anova_df <- df_bind %>%
        gt::gt() %>%
        gt::tab_caption(caption = paste0("ANOVA could not be ran on the following responses: ", paste(list_response[bad_analysis_nums],collapse = ", "), ". This is likely due to collinearity or one predictor perfectly matching with the response(s) of interest."))
    } else {
      final_anova_df <- df_bind %>%
        dplyr::group_by(Response) %>%
        gt::gt() %>%
        gt::data_color(
          columns = P_value,
          colors = scales::col_bin(
            palette = c("blue", "transparent"),
            bins = c(0, 0.05, 1),
            na.color = "transparent"
          )
        ) %>%
        gt::fmt_number(
          columns = c(SS,F_value,P_value),
          decimals = 5
        ) %>%
        gt::tab_caption(caption = paste0("ANOVA could not be ran on the following responses: ", paste(list_response[bad_analysis_nums],collapse = ", "), ". This is likely due to collinearity or one predictor perfectly matching with the response(s) of interest."))
    }
  } else {
    df_bind <- df_list %>% dplyr::bind_rows()
    final_anova_df <- df_bind %>%
      dplyr::group_by(Response) %>%
      gt::gt() %>%
      gt::data_color(
        columns = P_value,
        colors = scales::col_bin(
          palette = c("blue", "transparent"),
          bins = c(0, 0.05, 1),
          na.color = "transparent"
        )
      ) %>%
      gt::fmt_number(
        columns = c(SS,F_value,P_value),
        decimals = 5
      )
  }
  final_anova_df %>% gt::tab_header("ANOVA Summary")
})

output$anova_results_gt <- render_gt({
  req(lm_model())
  req(anova_gt())
  
  anova_gt()
})

desirability_gt <- eventReactive(input$go_analysis,{
  req(lm_model())
  req(exp_df())
  req(input$predictors_columns)

  list_response <- as.list(input$response_columns)

  # create range for target
  dTarget_list <- purrr::map(list_response,function(resp){
    dTarget(-3,0,3, lowScale = 1, highScale = 1)
  })

  # determine if we are working with categorical or numeric
  # and get the possible values for newdata
  data_df <- exp_df()
  # convert to categorical where we need
  data_df <- convert_columns_to_categorical(data_df, input$predictors_columns)
  # set up numeric values where we need
  if(!is.null(input$desirability_numeric_predictors_yes_no) && (input$desirability_numeric_predictors_yes_no != "No") && !is.null(input$predictors_numeric_columns) && length(input$predictors_numeric_columns) > 0){
    data_df <- convert_columns_to_numeric(data_df, input$predictors_numeric_columns)
  }
  # get only predictors
  pred_df <- data_df[input$predictors_columns]

  pred_predictors <- map(pred_df,desirability_predictors_values)

  # get all unique_values
  unique_values <- map(pred_predictors,function(pp){
    return(pp$unique_values)
  })

  # create a grid of all possible values of predictors
  optim_df <- expand.grid(unique_values)

  # work with responses
  # get the linear models
  lm_mods <- lm_model()$model_lm

  # predict on newdata
  predMods <- purrr::map(lm_mods,function(mod){
    predict(mod, newdata = optim_df)
  })

  # obtain desirability
  desList = purrr::map2(dTarget_list,predMods, function(des,pred_mod){
    desResponse = predict(des,pred_mod)
  })

  # calculate overall desirability
  if(length(desList) > 1){
    overallD <- sqrt(Reduce("*", desList))
  } else {
    overallD <- desList[[1]]
  }

  # add in desirability
  optim_df$desirability <- overallD

  # find the optimal value
  optimal_setup <- optim_df[which.max(overallD),] %>%
    gt()

})

observeEvent(input$go_analysis,{
  req(lm_model())
  shinyalert("Linear Model Created",
             paste("Please view the tab 'Linear Regression Model' for ANOVA results and linear regression coefficients.
                   Select the tab 'Desirability Plots' to view desirability analysis results."),
             type = "success")
})


output$desirability_optim <- render_gt({
  req(lm_model())
  req(desirability_gt())
  
  desirability_gt()
})

# output$desirability_plots_tab_UI <- renderUI({
#   
# })
# 
# 
# output$plot_tabs <- renderUI({
#   req(analysis_result())
#   
#   tabs <- lapply(names(analysis_result()), function(pred) {
#     tabPanel(title = pred, plotOutput(outputId = paste0("plot_", pred)))
#   })
#   
#   do.call(tabsetPanel, tabs)  # Correctly build tabset from list of tabPanel()s
# })
# 
# 
# output$desirability_plots_UI <- renderUI({
#   req(lm_model())
#   req(input$predictors_columns)
#   
#   selectInput("predictors_columns_plotting", "Select Predictor for x-axis", 
#               choices = input$predictors_columns)
# })
# 
desirability_plots_reactive <- eventReactive(input$go_analysis,{
  req(lm_model())
  req(input$predictors_columns)
  #req(input$predictors_columns_plotting)

  list_response <- as.list(input$response_columns)

  # create range for target
  dTarget_list <- purrr::map(list_response,function(resp){
    dTarget(-3,0,3, lowScale = 1, highScale = 1)
  })

  # get only predictors
  pred_df <- exp_df()[input$predictors_columns]

  pred_df <- convert_columns_to_categorical(pred_df, input$predictors_columns)
  # set up numeric values where we need
  if(!is.null(input$desirability_numeric_predictors_yes_no) && (input$desirability_numeric_predictors_yes_no != "No") && !is.null(input$predictors_numeric_columns) && length(input$predictors_numeric_columns) > 0){
    pred_df <- convert_columns_to_numeric(pred_df, input$predictors_numeric_columns)
  }

  # determine if we are working with categorical or numeric
  # and get the possible values for newdata
  pred_predictors <- map(pred_df,desirability_predictors_values)

  # get all unique_values
  unique_values <- map(pred_predictors,function(pp){
    return(pp$unique_values)
  })

  # create a grid of all possible values of predictors
  optim_df <- expand.grid(unique_values)

  # work with responses
  # get the linear models
  lm_mods <- lm_model()$model_lm

  # predict on newdata
  predMods <- purrr::map(lm_mods,function(mod){
    predict(mod, newdata = optim_df)
  })

  # obtain desirability
  desList = purrr::map2(dTarget_list,predMods, function(des,pred_mod){
    desResponse = predict(des,pred_mod)
  })

  # calculate overall desirability
  if(length(desList) > 1){
    overallD <- sqrt(Reduce("*", desList))
  } else {
    overallD <- desList[[1]]
  }

  # add in desirability
  optim_df$desirability <- overallD

  # find the optimal value
  optimal_setup <- optim_df[which.max(overallD),]
  
  
  possible_x_predictors <- input$predictors_columns
  
  plot_list <- purrr::map(possible_x_predictors,function(xaxis_col){
    other_predictors <- input$predictors_columns[input$predictors_columns != xaxis_col]
    other_vec <- optimal_setup[names(optimal_setup) %in% other_predictors]
    
    if(length(other_predictors) > 0){
      # Generic function to filter based on a named vector
      filter_data <- function(df, filter_vector) {
        # Create the filtering condition dynamically
        conditions <- purrr::map2(names(filter_vector), filter_vector, ~ df[[.x]] == .y)
        
        # Combine the conditions using & (logical AND)
        filter_condition <- Reduce(`&`, conditions)
        
        # Apply the filter to the data frame
        df_filtered <- df[filter_condition, ]
        
        return(df_filtered)
      }
      
      # Use the function to filter the data frame
      optim_df_filt <- filter_data(optim_df, other_vec)
      
      p1 <- ggplotly(optim_df_filt %>%
        ggplot(aes_string(x = paste0("`",xaxis_col,"`"), y = "desirability")) +
        geom_line() +
        geom_point() + 
        theme_bw())
    } else {
      optim_df_filt <- optim_df
      
      p1 <- ggplotly(optim_df_filt %>%
        ggplot(aes_string(x = paste0("`",xaxis_col,"`"), y = "desirability")) +
        geom_line() +
        geom_point() + 
        theme_bw())
    }
    
    
  })
  
  names(plot_list) <- input$predictors_columns
  plot_list
})

output$desirability_plot_tabs_UI <- renderUI({
  req(desirability_plots_reactive())
  
  tabs <- lapply(names(desirability_plots_reactive()), function(pred) {
    tabPanel(title = pred, 
             plotlyOutput(outputId = paste0("plot_",pred))
             )
  })
  
  do.call(tabsetPanel, tabs)
  
})

observe({
  req(desirability_plots_reactive())
  
  lapply(names(desirability_plots_reactive()), function(pred) {
    local({
      pred_local <- pred  # isolate loop var
      output[[paste0("plot_",pred_local)]] <- renderPlotly({
        desirability_plots_reactive()[[pred_local]]
      })
    })
  })
})

