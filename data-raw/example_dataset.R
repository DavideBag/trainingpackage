## code to prepare `example_dataset` dataset goes here


example_dataset(read_tsv("./inst/extdata/example_dataset.tsv"))


usethis::use_data(example_dataset, overwrite = TRUE)
