####################################################

#-----------------BASIC SUMULATION -----------------
# 31-08-2026
# example of distribution
runif(5,5,10) # 5 uniform numbers from 5 to 10

runif(5,0,10)

runif(5)*10

runif(5)*5 + 5

set.seed(1) #keep the same random generation when running the code

rnorm(5)

rchisq(5, df=1)

# Permutation

sample(1:5) 

# Q1: generate 10 mutually independent random binary numbers
1*(runif(10)>0.5) # independent
sample(rep(c(0,1), each=5)) # not independent

# simulation with permutation number

################
# 2nd of September 2026
# simulation of numerical Integration
n <- 10
x <- runif(n) # uniform random variable from 0 to 1
# example of a simple function: f(x) = sin(x)
true.mean <- 1 - cos(1) # our benchmark 

x <- sample(1:n) # permutation form one to n
x <- x - runif(n)
x <- x/n

# Latin hypercube sample

# function to generate 1D latin hypercube sample 
# input: sample size
lhs1 <- function(n){
  x <- sample(1:n) # permutation form one to n
  x <- x - runif(n)
  x <- x/n
  return(x)
}


x.lhs <- lhs1(n)
int.approx <- function(u){
  mean(sin(u))
}
int.approx(x.lhs)


# the simulation code 
set.seed(10)
n <- 10
nrep <- 1000
lhs.mean <- replicate(nrep, int.approx(lhs1(n))) # use replicate function


iid.mean <- apply(matrix(runif(n*nrep), nrep, n), 1, int.approx)

means <- cbind(lhs.mean, iid.mean)

 vars <- apply(means, 2, var)

 bias <- apply(means - true.mean, 2, mean)
 vars + bias^2 # this is the mean squared errors
 
 
 
 # two dimensional case
 x <- sample(1:n) # permutation form one to n
 x <- x - runif(n)
 x1 <- x/n
 
 
 x <- sample(1:n) # permutation form one to n
 x <- x - runif(n)
 x2 <- x/n
 
 cbind(x1, x2)
 
 
 
 ########################################
 # Approximating the root of the equation Ax^2 + Bx + C = 0
 
 nrep <- 100
 A <- runif(nrep)
 B <- runif(nrep)
 C <- runif(nrep)
 B^2 - 4*A*C  #  when the delta is negative so no real solution
 mean(B^2 - 4*A*C>0)
 
 ## Exercise: simulation example: question like this on the Lab Quiz
 
 set.seed(10)
 n <- 10
 A <- runif(n)
 B <- runif(n)
 C <- runif(n)
 quadratic_function <- function(x,n){
   # input: n is the number of the sample
   # x: is the unknown value that we aim to find
   A <- runif(n)
   B <- runif(n)
   C <- runif(n)
   return(A*x^2 + B*x + C)
 }
 
 