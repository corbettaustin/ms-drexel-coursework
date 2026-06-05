suppressPackageStartupMessages(library(fpp3))
gdp_avg <- global_economy |>
  dplyr::mutate(GDP_per_capita = GDP / Population) |>
  dplyr::group_by(Country) |>
  dplyr::summarise(avg_gdppc = mean(GDP_per_capita, na.rm = TRUE)) |>
  dplyr::arrange(dplyr::desc(avg_gdppc))
cat("Q1 - 6th highest avg GDP per capita:\n")
print(gdp_avg |> dplyr::slice(6) |> dplyr::pull(avg_gdppc))
cat("Q2 - Country with 6th highest avg GDP per capita:\n")
print(as.character(gdp_avg |> dplyr::slice(6) |> dplyr::pull(Country)))
