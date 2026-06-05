suppressPackageStartupMessages(library(fpp3))
result <- global_economy |>
  dplyr::filter(Country == "Liechtenstein") |>
  dplyr::mutate(GDP_per_capita = GDP / Population) |>
  dplyr::filter(!is.na(GDP_per_capita)) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::pull(GDP_per_capita)
cat("Q3 - Last recorded GDP per capita for Liechtenstein:\n")
print(result)
