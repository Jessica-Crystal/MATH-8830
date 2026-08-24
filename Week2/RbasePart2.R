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
