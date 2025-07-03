

library(splines)
library(caret)


data <- dat_ex1_noM_Inv
data <- dat_ex1_noM_Nat

data <- dat_ex1_M_Inv
data <- dat_ex1_M_Nat



# create list of model formulas
model_list <- list(
  m1 = B ~ N,
  m2 = B ~ bs(N, degree = 2),
  m3 = B ~ bs(N, degree = 3),
  m3 = B ~ bs(N, degree = 4)
)

formulas <- model_list

# specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

loo_list <- list()
for (name in names(formulas)) {
  # run the leave one out cross validation
  x <- 
    train(formulas[[name]], 
          data = data,
          method = "lm", trControl = ctrl)
  # bind into a data.frame and add to list
  loo_list[[name]] <-
    dplyr::bind_cols(dplyr::tibble(model = name),
                     dplyr::as_tibble(x$results))
}

# bind into a data.frame
loo_df <- dplyr::bind_rows(loo_list)

# sort by RMSE
loo_df <- dplyr::arrange(loo_df, RMSE)

# check the models
loo_df

# we fit all the models so that the objects are present
model_fits <- list()
for (name in names(formulas)) {
  model_fits[[name]] <- lm(
    formula = formulas[[name]],
    data = data,
  )
  }


# predict new data from the best model
new_dat <- dplyr::tibble(N = seq(min(data$N), max(data$N) + 0.1, 0.1))
pred_dat <- predict(model_fits[[loo_df$model[1]]], newdata = new_dat, interval = "confidence")

# bind into a data.frame
pred_dat <- dplyr::bind_cols(new_dat,
                             dplyr::as_tibble(pred_dat))

ggplot() +
  geom_point(data = data,
             mapping = aes(x = N, y = B)) +
  geom_line(data = pred_dat,
            mapping = aes(x = N, y = fit)) +
  geom_ribbon(data = pred_dat,
              mapping = aes(x = N, ymin = lwr, ymax = upr),
              alpha = 0.1)
  












