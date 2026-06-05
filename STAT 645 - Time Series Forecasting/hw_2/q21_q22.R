suppressPackageStartupMessages(library(fpp3))

bricks <- aus_production |>
  dplyr::filter(!is.na(Bricks)) |>
  dplyr::select(Quarter, Bricks)

# Q21 - SNAIVE forecast for Bricks, Q1 2010
bricks_snaive <- bricks |>
  model(SNAIVE(Bricks)) |>
  forecast(h = "5 years")

cat("Q21 - SNAIVE forecast for Bricks, Q1 2010:\n")
print(bricks_snaive |>
  dplyr::filter(Quarter == yearquarter("2010 Q1")) |>
  dplyr::pull(.mean))

# Q22 - RW with drift for Bricks, Q1 2010
bricks_rwd <- bricks |>
  model(RW(Bricks ~ drift())) |>
  forecast(h = "5 years")

cat("Q22 - RW with drift forecast for Bricks, Q1 2010:\n")
print(bricks_rwd |>
  dplyr::filter(Quarter == yearquarter("2010 Q1")) |>
  dplyr::pull(.mean))
