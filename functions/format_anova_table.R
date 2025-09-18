
# format car::Anova() table
format_anova_table <- function(table_output) {
  
  # convert to a tibble
  aov_tbl_tidy <- broom::tidy(aov_tbl)
  
  # make column names cleaner
  aov_tbl_tidy <- aov_tbl_tidy |>
    dplyr::rename(
      Term     = term,
      df       = df,
      `Sum Sq` = sumsq,
      `F`      = statistic,
      `P-value`= p.value
    ) |>
    dplyr::mutate(
      Term = stringr::str_replace_all(Term, "bs\\([^)]*\\)", "N")
    )
  
  
  # inspect
  aov_tbl_tidy
  
}