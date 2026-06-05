zooSurvey <- read.csv("zoo_kids.csv")
cars <- read.csv("automobile.csv", stringsAsFactors = TRUE)
make <- cars$make
cars <- cars[, -1]
library(softImpute)

#Q1: analyze missingness patterns
print(zooSurvey) 

# rows 1, 3, 7 have 0/Na values, suggest MAR
q1 <- 2

#Q2 make is categorical, PCA requires numeric
q2 <- 2

#Q3 count variables in cars
q3 <- ncol(cars)

#Q4 replace ? with NA
cars[cars == "?"] <- NA

#count total
q4 <- sum(is.na(cars))

#Q5 convert to numeric
cars <- as.data.frame(apply(cars, 2, function(x) as.numeric(x)))

#fit model
fit <- softImpute(as.matrix(cars), rank.max = 12, lambda = 10, type = "svd")

#get complete matrix
cars_imputed <- complete(as.matrix(cars), fit)

#find Isuzu cars with missing prices
isuzuMiss <- which(make == "isuzu" & is.na(cars$price))

q5 <- cars_imputed[isuzuMiss, "price"]

#Q6 test accuracy first
q6 <- 1

#Q7 remove rows with NAs
make <- make[complete.cases(cars)]
cars <- cars[complete.cases(cars), ]

#correlation analysis
cor_matrix <- cor(cars)
print(cor_matrix)

q7 <- 4

#Q8 PCA for variance explanation
pca_result <- prcomp(cars, scale. = TRUE)

#calculate proportion of variance explained
Variance_prop <- summary(pca_result)$importance[3, ]

#components needed for 90%
q8 <- which(Variance_prop >= 0.90)[1]

#variance plot
plot(Variance_prop, type = "b",
     xlab = "Principal Component",
     ylab =  "Cumulative Proportion of Variance Explained",
     main = "PCA Vairiance Explained")
abline(h = 0.90, col = "red", lty = 2)

#Q9 
#create biplot
biplot(pca_result, scale = 0, cex = 0.6)

#engine size and price correlate, higher mpg and smaller engine
q9 <- c(2, 3)

#Q10
#minimum PC1
pc_scores <- pca_result$x

#find index
leftmost_index <- which.min(pc_scores[, 1])

#get make
leftmost_make <- make[leftmost_index]

#answer
print(leftmost_make)
q10 <- 3





