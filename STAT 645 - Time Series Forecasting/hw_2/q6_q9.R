cat("Q6 - Middle weight of 7-MA (1/7):\n")
print(1/7)

cat("Q7 - Last weight of 2x12-MA (1/24):\n")
print(1/24)

cat("Q8 - Number of terms in a 3x7-MA:\n")
print(9)

cat("Q9 - All weights of 3x7-MA:\n")
m <- 7; k <- 3
w7 <- rep(1/m, m)
w3 <- rep(1/k, k)
combined <- convolve(w3, rev(w7), type = "open")
print(combined)
cat("Q9 - Second to last weight:\n")
print(combined[length(combined) - 1])
