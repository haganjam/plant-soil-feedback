
# function to figure out which are categorical versus continuous

# load plotting theme
source(here::here("functions/plot_theme.R"))

# load relevant packages
library(rlang)
library(ggplot2)
library(modelr)

# function to detect the type of variable
detect_variable_type <- function(x, max_unique_threshold = 20, unique_ratio_threshold = 0.05) {
  # get basic properties
  n <- length(x)
  unique_vals <- unique(x)
  nunique <- length(unique_vals)
  class_x <- class(x)
  
  # handle missing values
  x_non_na <- x[!is.na(x)]
  
  # rule 1: Character, factor, or logical types are categorical
  if (class_x %in% c("character", "factor", "logical")) {
    return("categorical")
  }
  
  # rule 2: Integer or numeric types - use heuristics
  if (is.numeric(x) || is.integer(x)) {
    # rule 2a: Check if binary or small integer range (e.g. 0/1, 1:10)
    if (all(x_non_na %% 1 == 0)) {  # All integers
      if (all(x_non_na %in% c(0, 1))) {
        return("categorical")
      }
      if (nunique <= max_unique_threshold) {
        return("categorical")
      }
    }
    
    # rule 2b: Check if number of unique values is low relative to sample size
    if ((nunique / n) < unique_ratio_threshold || nunique <= max_unique_threshold) {
      return("categorical")
    }
    
    # rule 2c: Otherwise treat as continuous
    return("continuous")
  }
  
  # rule 3: Default fallback
  return("unknown")
}



# plot summary statistics
eda_summary_stat <- function(data) {
  
  # extract the vars object
  vars <- names(data)
  
  sum_list <- list()
  for (i in seq_along(vars)) {
    # calculate the summary statistics
    x <- summary(data[[vars[i]]])
    # wrap into a data.frame
    y <-
      dplyr::tibble(group = names(x),
                    value = x)
    # rename the values
    names(y) <- c("summary_stat", vars[i])
    # add to a list
    sum_list[[i]] <- y
    
  }
  
  # return the list
  sum_list
  
}

eda_is_na <- function(data) {
  
  # extract the vars object
  vars <- names(data)
  
  is_na_list <- list()
  for (i in seq_along(vars)) {
    
    is_na_list[[i]] <- 
      dplyr::tibble(variable = vars[i],
                    na_count = sum(is.na(data[[vars[i]]])))
    
  }
  
  dplyr::bind_rows(is_na_list)
  
}

# plot histograms of the variables in vars
eda_plot_hist <- function(data, vars) {
  
  plot_list <- list()
  for (i in seq_along(vars)) {
    
    plot_list[[i]] <- 
      ggplot(data = data,
             mapping = aes(x = !!sym(vars[i]))) +
      geom_histogram() +
      theme_meta()
    
  }
  
  plot_list
  
}

# bivariate plots
eda_plot_bivariate <- function(data) {
  
  # extract the variable-type of each variable
  var_types <- sapply(data, function(x) detect_variable_type(x))
  
  # get all pairwise combinations of the variables
  var_combn <- combn(names(data), 2)
  
  bi_plot_list <- list()
  for (i in seq_len(ncol(var_combn))) {
    
    # select the two-variable combination
    var_select <- var_combn[, i]
    
    # select the variable types of these two variables
    var_types_select <- var_types[var_select]
    
    bi_plot_list[[i]] <-
      
      if (all(var_types_select == "categorical")) {
        
        ggplot(data = data) +
          geom_count(mapping = aes(x = !!sym(var_select[1]), y = !!sym(var_select[2]))) +
          theme_meta()
        
      } else if (sum(var_types_select == "categorical") == 1) {
        
        x <- names(sort(var_types_select))
        ggplot(data = data) +
          geom_point(mapping = aes(x = !!sym(x[1]), y = !!sym(x[2]))) +
          theme_meta()
        
      } else if (all(var_types_select == "continuous")) {
        
        ggplot(data = data) +
          geom_point(mapping = aes(x = !!sym(var_select[1]), y = !!sym(var_select[2]))) +
          theme_meta()
        
      } else {
        
        NA
        
      }
    
  }
  
  bi_plot_list

}

# bivariate + covariate plot
eda_plot_bivariate_covariate <- function(data,
                                         x_var,
                                         y_var,
                                         covariates) {
  
  # construct the formula
  model_formula <- as.formula(paste(y_var, "~", paste(c(covariates), collapse = " + ")))
  
  # Fit the model
  model <- lm(model_formula, data = data)
  
  # extract residuals
  data_mod <-
    data |>
    add_residuals(model)
  
  ggplot(data = data_mod) + 
    geom_jitter(mapping = aes(x = !!sym(x_var), y = resid)) +
    ylab(paste0("Residuals (", y_var, ")")) +
    ggtitle(paste0("Covariates: ", paste(c(covariates), collapse = " + "))) +
    theme_meta()
  
}











