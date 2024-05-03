#' create a random scatterplot
#' @import tidyverse
#' @param tsvfuke string indicating path to a tsv dataset
#'
#' @return a plot object with a scatterplot from randomly selected variables
#' @export
#'
#' @examples
#' \dontrun{
#' make_scatterplot("/my/path/to/data.tsv")
#' }
make_scatterplot <- function(tsvfuke) {
  data <- read_tsv(tsvfuke)

  sample_vars <- sample(
    names(data %>% select(where(is.numeric))),
    2
  )

  plot <- data %>%
    ggplot(aes(x = .data[[sample_vars[[1]]]], y = .data[[sample_vars[[2]]]])) +
    geom_point()

  return(plot)
}
