
# set default params
set_default_params <- function(defaults = list()) {
  if (!exists("params", inherits = FALSE)) {
    assign("params", defaults, envir = .GlobalEnv)
  }
}
