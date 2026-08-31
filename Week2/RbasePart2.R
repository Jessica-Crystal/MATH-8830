# my code
n <- 0:10
Rmat <- rbind(0.5^n, 0.5^(n-1), 0.5^(n-2) )
Rmat

# Teacher's code
n <- 10
M1 <- matrix(rep(0:(n-1), each=n), n, n) # replicate from zero to 9
M2 <- matrix(rep(0:(n-1),n), n, n)
0.5^abs(M1 - M2)


#the apply function 
x <- matrix(1:6, nrow=2, ncol=3)
apply(x, 1, mean) # apply on the row part and computing the mean for each row
apply(x, 2, mean) # apply on each column, ie calculating the mean on each column

# question for "apply" function
sample <- runif(n=100, 0, 1)
mean(sample)

# teacher's code
n <- 100
s <- 1000
mean(runif(n))
x <- matrix(runif(n*s), nrow=n, ncol=s)
xmean <- apply(x, 2, mean)
hist(xmean)


# Build function

x <- c(2, -3, 0, 8)
y <- c(1, 5, 2, 0)
sum_x <- sum(x)
csum_x <- cumsum(x)
xmax <- max(x)
pmax(x, y) # compare between x and y and take the max, pairwise max

# practice question
n <- 1000
z <- runif(n)
plot(cumsum(z)/n)

#teacher's code
n <- 1000
z <- runif(n)
ind <- 1:n
cbind(cumsum(z), 1:n)
plot(cumsum(z)/ind)


n <- 10
z <- runif(n)
ind <- 1:n
cbind(cumsum(z), 1:n)
plot(cumsum(z)/ind)


# write your own function
final <- function(x){
  homework <- x[1]
  exam <- x[2]
  project <- x[3]
  final <- homework*0.2 + exam*0.5 + project*0.3
  return(final)
}

final(c(50,30,10))
scores <- matrix(runif(100*3, 50, 100), 100, 3)
scores
apply(scores, 1, final)

# 26 Aout 2026

final <- function(x){
  #############################
  # information about the output
  # input:
  # output:
  homework <- x[1]
  exam <- x[2]
  project <- x[3]
  final <- homework*0.2 + exam*0.5 + project*0.3
  return(final)
}
# my code
mean_absolute_deviation <- function(x, n){
  # input: x is a vector
  # n is the size of the vector
  # output:
  mean_x <- mean(x)
  results <- 1/n*sum(abs(x - mean_x))
  return(results)
}


# teacher's code
x <- runif(5)
#
mean(abs(x -mean(x)))
# work
mad <- function(x){
  mean(abs(x -mean(x)))
}

# work
mad1 <- function(x){
 mad <- mean(abs(x -mean(x)))
 return(mad)
}

# doesn't return anything
mad2 <- function(x){
  mad <- mean(abs(x -mean(x)))
}
mad(x)


## the for and the while loop
# compute the first 10 elements in the fibonacci sequence
Fib <- c(1,1) # inital value of the fibonacci
for(i in 3:10){
  Fib[i] <- Fib[i-1] + Fib[i-2]
}
Fib


# Generate the Fibonacci sequence up to the largest term greater than 50
Fib <- c(1,1)
i <- 2
while(Fib[i]<50){
  Fib[i+1] <- Fib[i] + Fib[i-1]
  i <- i + 1
}
Fib
 
# Find the prime numbers
# some preparation
a <- 1
b <- 1.1
d <- "Qiong"
g <- TRUE # AUTOMATICALLY CONVERT TO 1 SO U CAN MULTIPLY BY A NUMBER
a <- 1:4
g <- c(TRUE, FALSE, FALSE, TRUE)
mean(g)
a[g] # ?? RETURN THE TRUE POSITION
which(g)
if(2>1){
  
  print("correct")
  
} 

# find the prime numbers for up to 100
N <- 100
prime <- c(FALSE, rep(TRUE, N-1))
prime
for(n in 2:sqrt(N)){
  if(prime[n]){
    for(s in 2:(N/n)){
      prime[s*n] <- FALSE
    }
  }
  
}
prime
which(prime)
# write a function called FindPrime
# input: N positive integer
# output: all the prime Numbers less than or equal to N

FindPrime <- function(N){
  prime <- c(FALSE, rep(TRUE, N-1))
  for(n in 2:sqrt(N)){
    if(prime[n]){
      for(s in 2:(N/n)){
        prime[s*n] <- FALSE
      }
    }
    
  }
  return(prime)
}
res <- FindPrime(20)
which(res)


