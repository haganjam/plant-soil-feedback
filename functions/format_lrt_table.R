
# format an LRT table
format_lrt_table <- function(table_output,
                             model_names = c("Reduced model", "Full model")) {
  
  # broom format the model
  lrt_tbl <- 
    table_output |>
    broom::tidy()
  
  # remove the deviance column if present
  lrt_tbl <- lrt_tbl[, !(names(lrt_tbl) %in% "deviance")]
  
  # rename the models
  lrt_tbl$term <- model_names
  
  lrt_tbl <- 
    lrt_tbl |>
    dplyr::rename(
      Model = "term",
      `Residual DF` = "df.residual",
      `Residual SS` = "rss",
      `DF` = "df",
      `SS` = "sumsq",
      `P-value` = "p.value"
    ) |>
    dplyr::mutate(
      `P-value` = format.pval(`P-value`, digits = 3, eps = .001)
    ) |>
    dplyr::mutate(
      `P-value` = ifelse(`P-value` == "NA", NA, `P-value`)
    )
  
  lrt_tbl
  
}

