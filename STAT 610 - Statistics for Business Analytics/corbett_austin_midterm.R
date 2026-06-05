## Stat 610 Fall 2025
## Midterm Exam solutions
## Corbett Austin

##Question 1.
##From a survey of students attending a university, it was found that 22 were living off campus, 39 were undergraduates, 32 were graduate students, and 11 were undergraduates living off campus. Find the number of these students who were undergraduates, were living off campus, or both?

#given
off_campus <- 22
undergrads <- 39
grad_students <- 32
undergrad_off_c <- 11

#use the inclusion-exclusion principle
undergrad_or_oc <- undergrads + off_campus - undergrad_off_c
undergrad_or_oc

##Question 2.
##From a survey of students attending a university, it was found that 28 were living off campus, 43 were undergraduates, 37 were graduate students, and 7 were undergraduates living off campus. Find the number of these students who were undergraduates living on campus.

#given
off_campus <- 28
undergrads <- 43
grad_students <- 37
undergrad_off_c <- 7

#calculate number of undegraduates on campus
undergrad_on_campus <- undergrads - undergrad_off_c
undergrad_on_campus

##Question 3.
##From a survey of students attending a university, it was found that 30 were living off campus, 46 were undergraduates, 40 were graduate students, and 12 were undergraduates living off campus. Find the number of these students who were graduate students living on campus

#given
off_campus <- 30
undergrads <- 46
grad_students <- 40
undergrad_off_c <- 12

#calculate grad students off campus
grad_off_c <- off_campus - undergrad_off_c

#calculate grad students on campus
grad_on_c <- grad_students - grad_off_c
grad_on_c

##Question 4.
## A computer program runs using subroutine A 39% of the time, subroutine B 29% of the time and subroutine C the rest of the time.  If subroutine A is used then there is a 0.65 probability that the program will complete its task before its time limit is exceeded.  If subroutine B is used the probability of completion before exceeding the time limit is 0.41. If subroutine C is used the probability of completion before exceeding the time limit is 0.37.  If the computer does not complete its task prior to exceeding the time limit, what is the (posterior) probability that subroutine B was used? Enter your answer to 5 decimal places.

#define priors
P_A <- 0.39
P_B <- 0.29
P_C <- 1 - P_A - P_B
P_C

#calculate conditional of completed
PA_complete <- 0.65
PB_complete <- 0.41
PC_complete <- 0.37

#calculate conditional of not completed
PA_not_complete <- 1 - PA_complete
PB_not_complete <- 1 - PB_complete
PC_not_complete <- 1 - PC_complete
PA_not_complete
PB_not_complete
PC_not_complete

#calculate total probability of not completed
P_not_complete <- PA_not_complete * P_A +
			PB_not_complete * P_B +
			PC_not_complete * P_C
P_not_complete

#use Bayes
P_B_not_complete <- (PB_not_complete *P_B) / P_not_complete
P_B_not_complete

##Question 5.
##If two six sided-dice are rolled until the sum of 8 appears, what is the probability that it takes 2 rolls to accomplish this? Enter your answer to five decimal places.

#given
eight_combos <- 5
total_outcomes <- 6*6

#define success and fail
p_success <- eight_combos / total_outcomes
p_failure <- 1 - p_success

#calculate prob of two rolls
p_two_rolls <- p_failure * p_success
p_two_rolls

##Question 6.
##A student prepares for an exam by studying a list of ten problems. She can solve six of them. For the exam, the instructor selects five problems at random from the ten on the list given to the students. What is the probability that the student can solve exactly four of the problems on the exam? Enter your answers to 5 decimal places.

#define parameters
total_problems <- 10
can_solve <- 6
cannot_solve <- 4
exam_problems <- 5
want_to_solve <- 4

#calculate numerator and denominator
choose_4_solvable <- choose(can_solve, want_to_solve)
choose_1_unsolvable <- choose(cannot_solve, exam_problems - want_to_solve)
total_choose <- choose(total_problems, exam_problems)

#calc hypergeometric distribution
p_solve <- (choose_4_solvable * choose_1_unsolvable) / total_choose
p_solve

##Question 7.
##  A student prepares for an exam by studying a list of 21 problems. She can solve 17 of them. For the exam, the instructor selects 14 problems at random from the list given to the students.  How many question should the student expect to get correct on the exam? Enter your answer to 2 decimal places.

#define parameters
total_problems <- 21
can_solve <- 17
cannot_solve <- total_problems - can_solve
exam_problems <- 14

#calc expected value
expected_correct <- exam_problems * (can_solve / total_problems)
expected_correct

##Question 8.
##A bag of cookies is underweight if it weighs less than 500 grams. The filling process dispenses cookies with weight that follows the normal distribution with mean 505 grams and standard deviation 4 grams. What is the probability that a randomly selected bag is underweight? Enter your answer to 5 decimal places.

#define parameters
mean_weight <- 505
sd_weight <- 4
underweight <- 500

#calc normal probability
p_underweight <- pnorm(underweight, mean = mean_weight, sd = sd_weight)
p_underweight

##Question 9.
##A bag of cookies is underweight if it weighs less than 500 grams. The filling process dispenses cookies with weight that follows the normal distribution with mean 505 grams and standard deviation 4 grams. What is the weight for which 95% of the bags are greater than? Enter your answer to 4 decimal places

#define parameters
mean_weight <- 505
sd_weight <- 4
percent_greater <- 0.95

#calc weight threshold with quantile function
percentile <- 1 - percent_greater
w_threshold <- qnorm(percentile, mean = mean_weight, sd = sd_weight)
w_threshold

##Question 10.
##A bag of cookies is underweight if it weighs less than 500 grams. The filling process dispenses cookies with weight that follows the normal distribution with mean 505 grams and standard deviation 4 grams. If you randomly select 15 bags, what is the probability that fewer than 4 of them will be under weight?  Enter your answer to 5 decimal places.

#define parameters
mean_weight <- 505
sd_weight <- 4
underweight <- 500
bags <- 15
max_underweight <- 3

#calc to find probability of underweight bag using normal dist
p_underweight <- pnorm(underweight, mean = mean_weight, sd = sd_weight)
p_underweight

#calc fewer than four bags underweight using binomial dist
p_less_4 <- pbinom(max_underweight, size = bags, prob = p_underweight)
p_less_4


##Question 11.
##Customers arrive at a checkout counter in a department store according to a Poisson distribution at an average of seven per hour. During a given hour, what is the probability that no more than three customers arrive? Enter your answer to 5 decimal places

#define parameters
lambda <- 7

#calc cumulative probability of less than three arrivals as a poisson dist
p_less_than_3 <- ppois(3, lambda)
p_less_than_3

##Question 12.
##Customers arrive at a checkout counter in a department store according to a Poisson distribution at an average of seven per hour. During a given hour, what is the probability that at least two customers arrive? Enter your answer to 5 decimal places

#define parameters
lambda <- 7

#calc prob of at least two arriving using complement rule
p_less_2 <- 1 - ppois(1, lambda)
p_less_2

##Question 13.
##Customers arrive at a checkout counter in a department store according to a Poisson distribution at an average of seven per hour. During a given hour, what is the probability that exactly five customers arrive? Enter your answer to 5 decimal places.

#define parameters
lambda <- 7

#calc the prob of exactly five arriving using poission
p_exactly_5 <- dpois(5, lambda)
p_exactly_5

##Question 14.
## The weekly amount of downtime Y (in hours) for an industrial machine has a gamma distribution with α = 3 and lambda = 1/2. What is the expected down time for any randomly selected week?

#given 
alpha <- 3
lambda <- 1/2

#calc expected value in gamma dist
expected_value <- alpha / lambda
expected_value

##Question 15.
## The weekly amount of downtime Y (in hours) for an industrial machine has approximately a gamma distribution with α = 3 and lambda = 1/2 (R specifies beta as 1/lambda). Find the probability that the down time for a randomly selected week is less than 2 hours. Enter your answer to 5 decimal places

#define parameters
alpha <- 3
lambda <- 1/2

#calc cumulative prob of downtime less than two hours using pgamma
p_less_2 <- pgamma(2, shape = alpha, rate = lambda)
p_less_2


##Question 16.
##The weekly amount of downtime Y (in hours) for an industrial machine has approximately a gamma distribution with α = 3 and lambda = 1/2 (R specifies beta as 1/lambda). Find the 90th percentile for down time in any randomly selected week.Enter you answer to two decimal places.

#define parameters
alpha <- 3
lambda <- 1/2
beta <- 1/lambda

#calc the nintieth percentile of downtime using qgamma
percent_90 <- qgamma(0.90, shape = alpha, rate = lambda)
percent_90
