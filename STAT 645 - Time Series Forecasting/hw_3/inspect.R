library(fpp3)

# Check women's 5000m data
w5000 <- olympic_running %>% filter(Length == 5000, Sex == "women")
cat("Women's 5000m data:\n")
print(w5000)

# Check men's 800m data
m800 <- olympic_running %>% filter(Length == 800, Sex == "men")
cat("\nMen's 800m data:\n")
print(m800)
