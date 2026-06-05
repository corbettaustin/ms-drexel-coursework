# Loading a dataset -------------------------------------------------------

# Load external functions
source("plot_fcns.R")

# Read CSV file and store it in variable named: dat
dat <- read.csv("default.csv", stringsAsFactors = TRUE)

# Getting to know the data ------------------------------------------------

# Find out dimensions (size, and number of variables in dataset)
dim(dat)
# Number of rows only
nrow(dat)
# Number of columns only
ncol(dat)

# Names of variables in data.frame
names(dat)

# Finding about the variable dat
class(dat$defaulted)

# structure function 
str(dat)
# `summary` provides summary statistics
summary(dat)

# Using head and tail to get datasets insights
head(dat, 20) # shows the top 20 items
tail(dat, 10) # shows the bottom 10 items


# Answering basic questions about the data --------------------------------

# Has an individual defaulted?
dat$defaulted == "Yes"

# How many customers defaulted?
sum(dat$defaulted=="Yes")
# Proportion of customers that defaulted = P(defaulted = Yes)
sum(dat$defaulted=="Yes")/nrow(dat)
# or equivalently:
mean(dat$defaulted=="Yes")

# How many non-students have defaulted?
sum(dat$defaulted == "Yes" & dat$student=="No")

# How many students have credit card balance over $1000?
sum(dat$student == "Yes" & dat$balance > 1000)

# How many customers have an income lower than $10000?
sum(dat$income < 10000)

# How many customers are students AND have an income lower than $10000?
sum(dat$student == "Yes" & dat$income < 10000)

# How many customers are either students OR have an income lower than $10000?
sum(dat$student == "Yes" | dat$income < 10000)


# Conditional probabilities and Bayes rule--------------------------------

# P(X=x|Y=y) = \frac{P(X=x, Y=y)}{P(Y=y)}
# Probability of income < $20000 and student="Yes" (P(X=x, Y=y))
sum(dat$income < 20000 & dat$student == "Yes") / nrow(dat)

# Probability student = "Yes" (P(Y=y) part)
sum(dat$student == "Yes") / nrow(dat)

# Probability of income < $20000 given that student = "Yes"
sum(dat$income < 20000 & dat$student == "Yes") / sum(dat$student == "Yes")

# What is the proportion of non-students that have defaulted
sum(dat$defaulted == "Yes" & dat$student == "No") / sum(dat$student == "No")
# What is the proportion of students that have defaulted
sum(dat$defaulted == "Yes" & dat$student == "Yes") / sum(dat$student == "Yes")


# Simple visualizations for classification --------------------------------


# Barplot -----------------------------------------------------------------
# Simple barplot of defaulted
barp(dat,"defaulted")

# Relative frequencies (estimated probabilities) by modifying `freq` option
barp(dat,"defaulted", freq="relfreq")


# Storing Figures as Images -----------------------------------------------

# First declare that figure is to be stored in PDF format
pdf("barplot1.pdf",width = 7,height = 5)
# Create the figure you want
barp(dat,"defaulted")
# Finalize plotting and store figure in specified file
dev.off()


# Barplots of Class Conditional Distributions -----------------------------

# Conditional probability of student given defaulted P(X|Y)
cc_barplot(data = dat, "student","defaulted", freq = "condprob")
# the same as running earlier (case student = "No")
sum(dat$student == "No" & dat$defaulted == "No") / sum(dat$defaulted == "No")
sum(dat$student == "No" & dat$defaulted == "Yes") / sum(dat$defaulted == "Yes")

# Conditional probability of defaulted given student (P(Y|X))
cc_barplot(data = dat, "student","defaulted", freq = "relfreq")
# The same as running earlier (case defaulted = "Yes")
sum(dat$defaulted == "Yes" & dat$student == "No") / sum(dat$student == "No")
sum(dat$defaulted == "Yes" & dat$student == "Yes") / sum(dat$student == "Yes")


# Histograms --------------------------------------------------------------

# Histogram of balance
hist(dat$balance)

# Histogram of balance with 20 bins
hist(dat$balance, breaks = 20, col="red", main="", xlab="Credit Card Balance")

# Comparing two distributions with Sturges rules for bin size
cc_hist(dat,"balance", "defaulted")
# Comparing two distributions with manual set bins to 20
cc_hist(dat,"balance", "defaulted", breaks = 20)

# install.packages("lattice")
library(lattice)
# Plot of the density of "balance"
densityplot(~ balance, data = dat)
# Plot of conditional density p("balance"|"defaulted")
densityplot(~ balance, data = dat, groups = defaulted, auto.key=TRUE)


# Boxplots ----------------------------------------------------------------

# Boxplot of balance variable
boxplot(dat$balance, col = "red", notch = TRUE)

# Boxplot of class conditional distribution p(balance—defaulted)
cc_boxplot(dat, "balance", "defaulted")
# Boxplot of class conditional distribution p(income—defaulted)
cc_boxplot(dat, "income", "defaulted")

# Simple Scatterplot ----------------------------------------------------------

# Scatterplot of income against balance
plot(dat$balance, dat$income)

# Scatterplot of income against balance, with colors illustrating defaulted
plot(dat$balance, dat$income, col = dat$defaulted)

# Scatterplot of income against balance, with colors illustrating defaulted
plot(dat$balance, dat$income, col = dat$defaulted, xlab = "Balance", ylab = "Income",
     main = "Income against Balance")


# Scatterplot matrix ------------------------------------------------------

# Simplest possible scatterplot matrix
plot(dat, col = dat$defaulted)

# defaulted is the first variable (i.e. column) in default
names(dat)
# Exclude column(s) using the "-" notation: default[ ,-1]
plot(dat[, -1], col = dat$defaulted)

