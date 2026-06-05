#install.packages("distr")
#library(distr)


x <- c(0,1,2,3)
f <- c(1/8, 3/8, 3/8, 1/8)

F <- cumsum(f)
F




#mean
mu <- sum(x * f)
mu


#variance
sigma2 <- sum((x-mu)^2 * f)
sigma2

#standard deviation
sigma <- sqrt(sigma2)


#install.packages("distrEx")
library(distrEx)
m <- 6
X <- DiscreteDistribution(supp = 1:m, prob = rep(1/m,m) )
E(X); var(X); sd(X)

sample(m, 10, replace = TRUE)








#binomial
dbinom(c(0:3),3,.5)
pbinom(c(0:3),3,.5)

#install.packages("distr")
library(distr)
m <- 3
p <- .5
X <- Binom(size = 3, prob = 1/2)
X
Y <- DiscreteDistribution(supp = 0:m, dbinom(0:m,m,p))
Y


p(X)(2)
p(Y)(2)

d(X)(2)
d(Y)(2)

E(X)
var(X)

#Example X ~ Binom(n = 6, p = 1/2)
n <- 6
p <- .5
#P(X = 2)
dbinom(2,n,p)
X <- Binom(size = n, prob = p)
d(X)(2)

#P(2<= X <= 5)
pbinom(5,n,p) - pbinom(1,n,p)
#or
sum(dbinom(c(2:5),n,p))
#or
diff(pbinom(c(1,5),n,p))
#or
diff(p(X)(c(1,5)))

#mean
E(X)
#or
n*p

#var
var(X)
#or
n*p*(1-p)

#Hypergeometric
dhyper(2,5,8,9)

X <- Hyper(5, 8, 9 )
X
d(X)(2)


#geometric
p <- .001
X <- Geom(p)
X
1-sum(dgeom(0:999,p))
1- sum(d(X)(0:999))
1-p(X)(999)

#expected value
E(X)
(1-p)/p



#neg-binomial
p <- .5
r <- 7

X <- Nbinom(r,p)
X

dnbinom(5,r,p)
d(X)(5)

#P(X <- 10)
pnbinom(10,r,p)
p(X)(10)


#Poisson 
lambda <- 5

X <- Pois(lambda)
X

dpois(0,lambda)
d(X)(0)

lambda <- 5*10

X <- Pois(lambda)
X


diff(p(X)(c(47,50)))

















