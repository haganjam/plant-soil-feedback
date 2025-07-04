
compare_models_loocv <- function(data, formulas) {
  # Load necessary packages
  require(caret)
  require(dplyr)
  require(splines)
  
  # Specify the cross-validation method
  ctrl <- trainControl(method = "LOOCV")
  
  # Leave-One-Out CV Results
  loo_list <- list()
  for (name in names(formulas)) {
    # run the leave-one-out cross-validation
    x <- train(
      formulas[[name]],
      data = data,
      method = "lm",
      trControl = ctrl
    )
    # collect results
    loo_list[[name]] <- bind_cols(
      tibble(model = name),
      as_tibble(x$results)
    )
  }
  
  # Bind and sort by RMSE
  loo_df <- 
    bind_rows(loo_list) |>
    arrange(RMSE)
  
  # Fit models for later use
  model_fits <- list()
  for (name in names(formulas)) {
    model_fits[[name]] <- lm(
      formula = formulas[[name]],
      data = data
    )
  }
  
  # add the AIC value
  loo_df$AIC <- sapply(model_fits[loo_df$model], AIC)
  
  # Return as a list
  return(list(
    LOO_est = loo_df,
    Model_fits = model_fits
  ))
}
