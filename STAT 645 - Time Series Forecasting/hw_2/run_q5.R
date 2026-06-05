suppressPackageStartupMessages({
  library(fpp3)
  library(feasts)
})

cat("==============================================\n")
cat("QUESTION 5 - Best Box-Cox lambda for canadian_gas Volume\n")
cat("==============================================\n")

lambda <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

cat(sprintf("Answer: Best lambda (Box-Cox) for canadian_gas Volume = %.4f\n\n", lambda))

png("boxcox_canadian_gas.png", width=800, height=500)
canadian_gas %>%
  autoplot(box_cox(Volume, lambda)) +
  labs(title = sprintf("canadian_gas Volume after Box-Cox (lambda = %.4f)", lambda),
       y = "Transformed Volume")
dev.off()

cat("Transformation plot saved to boxcox_canadian_gas.png\n")
