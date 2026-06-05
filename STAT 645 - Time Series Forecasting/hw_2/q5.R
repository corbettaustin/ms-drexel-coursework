suppressPackageStartupMessages(library(fpp3))
bulls <- aus_livestock |>
  dplyr::filter(
    Animal == "Bulls, bullocks and steers",
    State == "Victoria"
  )
lambda_bulls <- bulls |>
  features(Count, features = list(guerrero)) |>
  dplyr::pull(lambda_guerrero)
cat("Q5 - Guerrero lambda for Victorian Bulls:\n")
print(lambda_bulls)
