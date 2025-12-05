# set up data.frame
df <- data.frame(
  Concentration = c(1,1,0.25,0.25,0.25,0.25,1,1,1,1,0.25,0.25),
  BlotTime = c(9,9,5,5,5,5,9,9,5,5,9,9),
  DetergentType = factor(c("CHAPSO","CHAPSO","CHAPSO","CHAPSO","None","None","None","None","FOM","FOM","FOM","FOM"),
                         levels = c("None","CHAPSO","FOM")),
  IceThickness = c(-1.5,-1.5,-1,1.5,1,2,-1.5,-1.5,2,1,-2,1),
  ParticleDensity = c(-2.5,-2.5,-3,-3,-2,-2,-1,-0.5,-1,-1,-2.5,-2.5),
  PreferredOrientation = c(0,0,0,0,1,1,1,1,0,0,0,0)
)

# run linear regression model on the data
mod_ice <- lm(IceThickness ~ Concentration + BlotTime + DetergentType, data = df)
summary(mod_ice)

# Function to create predictions and confidence intervals
predict_with_ci <- function(model, predictor_name, predictor_values, fixed_data, factor_levels) {
  preds_list <- lapply(factor_levels, function(level) {
    fixed_data$DetergentType <- level
    new_data <- fixed_data[rep(1, length(predictor_values)), ]
    new_data[[predictor_name]] <- predictor_values
    preds <- predict(model, newdata = new_data, interval = "confidence")
    data.frame(
      predictor = predictor_values,
      detergent_level = level,
      fit = preds[, "fit"],
      lwr = preds[, "lwr"],
      upr = preds[, "upr"]
    )
  })
  do.call(rbind, preds_list)
}


# Generate sequences for predictors
concentration_seq <- seq(min(df$Concentration), max(df$Concentration), length.out = 100)
blottime_seq <- seq(min(df$BlotTime), max(df$BlotTime), length.out = 100)
detergent_levels <- unique(df$DetergentType)
mean_values <- data.frame(
  Concentration = mean(df$Concentration,na.rm = T),
  BlotTime = mean(df$BlotTime,na.rm = T)
)

# Predict and calculate confidence intervals
concentration_preds <- predict_with_ci(mod_ice, "Concentration", concentration_seq, mean_values, detergent_levels)
blottime_preds <- predict_with_ci(mod_ice,"BlotTime",blottime_seq, mean_values, detergent_levels)

# desirability values
desirability_response <- function(y_pred, low = -3, target = 0, high = 3, importance = 1) {
  if (y_pred <= low || y_pred >= high) return(0)
  if (y_pred == target) return(1)
  if (y_pred < target) return((y_pred - low) / (target - low))^importance
  if (y_pred > target) return((high - y_pred) / (high - target))^importance
}

#  Calculate desirability for each predicted response
concentration_preds <- concentration_preds %>%
  mutate(desirability = sapply(fit, desirability_response, low = -3, target = 0, high = 3, importance = 1))
blottime_preds <- blottime_preds %>%
  mutate(desirability = sapply(fit, desirability_response, low = -3, target = 0, high = 3, importance = 1))


# Function to plot prediction profiler for a predictor
plot_prediction_profiler <- function(predictor_preds, predictor_name) {
  p1 <- ggplot(predictor_preds, aes_string(x = "predictor", y = "fit", color = "detergent_level")) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = detergent_level), alpha = 0.2) +
    labs(title = paste("Model for", predictor_name),
         y = "Response",
         x = predictor_name) +
    theme_minimal()
  
  p2 <- ggplot(predictor_preds, aes_string(x = "predictor", y = "desirability", color = "detergent_level")) +
    geom_line() +
    labs(title = paste("Desirability for", predictor_name),
         y = "Desirability",
         x = predictor_name) +
    theme_minimal()
  
  return(list(p1, p2))
}

# Plot prediction profilers for x1 and x2
concentration_plots <- plot_prediction_profiler(concentration_preds, "Concentration")
blottime_plots <- plot_prediction_profiler(blottime_preds, "Blot Time")


# Combine plots
gridExtra::grid.arrange(
  grobs = c(concentration_plots, blottime_plots),
  ncol = 2
)
