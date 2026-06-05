#setup
dat <- read.csv("default.csv", stringsAsFactors = TRUE)
source("plot_fcns.R")
dat$month <- dat$income/12
dat$time <- dat$balance / dat$month

#Q1: calculate last 100 rows and mean balance
q1 <- mean(dat$balance[(nrow(dat)-99):nrow(dat)])
q1

#Q2:third highest income
q2 <- sort(dat$income, decreasing = TRUE)[3]
q2

#Q3: customer with balance > income
q3 <-sum(dat$balance > dat$month)
q3

#Q4: % of students who are high balance customers
#filter customers where balance > monthly income
subset_high_balance <- dat[dat$balance > dat$month, ]

#calculate proportion of students
q4 <- mean(subset_high_balance$student == "Yes")
q4

#Q5: customers where payoff time > 3 months
q5 <- sum(dat$time > 3)
q5

#Q6: customers who defaulted vs not bar plot
barplot(table(dat$defaulted),
        main = "Distribution of Defaulted Customers",
        xlab = "Defaulted",
        ylab = "Count")

#since most customers have not defaulted, value 2
q6 <- 2

#Q7 create grouped bar plot where default status is primary, not secondary
barplot(table(dat$student, dat$defaulted),
        beside = FALSE,
        main = "Student Status by Default Status",
        legend = TRUE)
#looks like about 1/3 defaults are students, value 1
q7 <- 1

#Q8 default rate comparison students vs non
default_rate_students <- mean(dat$defaulted[dat$student == "Yes"] == "Yes")
default_rate_nonstudents <- mean(dat$defaulted[dat$student == "No"] == "Yes")

#compare with bar plot
barplot(c(Students = default_rate_students,
          NonStudents = default_rate_nonstudents),
        main = "Default Rates: Students vs Non-Students",
        ylab = "Default Rate",
        col = c("blue", "red"))
#calculate difference
prob_difference <- abs(default_rate_students - default_rate_nonstudents)
prob_difference

#looking at comparison, students actually have higher rate but difference is small, value 3
q8 <- 3

#Q9 create scatter plot matrix to gain insights on certain variables
pairs(dat[, c("time", "balance", "income", "student")],
      col = ifelse(dat$defaulted == "Yes", "red", "blue"),
      main = "Scatterplot Matrix by Default Status")
#interpret - balance and time are strong indicators of default, the latter are not (T,F,T,T)
q9 <- c(1, 3, 4)

#Q10: box plot of balance by student status
boxplot(balance ~ student,
        data = dat,
        main = "Credit Balance by Student Status",
        xlab = "Student",
        ylab = "Balance",
        col = c("lightblue", "lightgreen"))
#interpret - boxplots show median, students have higher median balance. value 2
q10 <- 2
