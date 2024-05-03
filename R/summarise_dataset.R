#' Summarises a Dataset
#'
#' @import tidyverse
#' @param tsvfuke A string with the path to a TSV file with data we would like to import
#'
#' @return A tibble with 4 variables, summarising average, min and max of each variable
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_dataset("/my/path/to/dataset.tsv")
#' }
summarize_dataset <- function(tsvfuke) {
  data <- read_tsv(tsvfuke)
  summary_table <- tibble()
  for (var in names(data)) {
    if (is.numeric(data[[var]])) {
      summary_table <- bind_rows(
        summary_table,
        bind_cols(
          variable = var,
          average = mean(data %>% pull(var)),
          minumum = min(data %>% pull(var)),
          maximum = max(data %>% pull(var))
        )
      )
    }
  }
  return(summary_table)
}
