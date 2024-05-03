test_that("multiplication works", {
  tsv_file <- system.file("extdata", "example_dataset.tsv", package = "trainingpackage")
  scatterplot + make_scatterplot(tsv_file)
  expect_true("ggplot" %in% class(scatterplot))
})
