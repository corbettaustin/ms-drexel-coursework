suppressPackageStartupMessages(library(fpp3))
mali <- global_economy |>
  dplyr::filter(Country == "Mali") |>
  dplyr::mutate(GDP_per_capita = GDP / Population) |>
  dplyr::filter(!is.na(GDP_per_capita))
lambda_mali <- mali |>
  features(GDP_per_capita, features = list(guerrero))
cat("Q4 - Guerrero lambda for Mali GDP per capita:\n")
print(lambda_mali)
