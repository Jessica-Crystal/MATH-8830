#########################################
# R Basic: Part I
# Ref: Learning Base R: Lawrence M. Leemis
#########################################
## R as a calculator
4 + 3/10^2
4 + (3/10)^2
(4+3)/10^2
((4+3)/10)^2

37%/%7 # integer part
37%%7 # remainder

# question 1: 
100*(1 + 0.05/12)^24

# question 2: remainder of 3333 divided by 222
3333%%222

## Simple objects
x = 2
x <- 2 # assign x to 2 is similar to x= 2 but assigning is better
x <- x + 1
x = y = z = 2

 # ls() list of the variables assigned
 # rm() remove variable or remove all variables: rm(list = ls())
x = y = z = 2
x^y
 
 # choose a meaningful name: name cannot be start with number but can end with number 
CashOnHand <- 155
Cash_On_Hand <- 155
Cash.On.Hand  <- 155
CashOn12 <- 4


# Question 3

P <- 100
r <- 0.08
n <- 12
t <- 3
Fvalue <- P*(1+r/n)^(n*t) # careful with F because it refers to False 
Fvalue # or print(Fvalue)
sprintf("Final Value: $%.2f", Fvalue) # $ for place value
cat(sprintf("Final Value: $%.2f", Fvalue)) # cat to remove the quote

## Vector
## Create vectors
1:5 # sequence that increase by 1
5:10
5.1:11.3
5:-3

x <- 1:5 # you can assign a vector with a variable
y <- c(1,100,2,999) # another way to define a vector

seq(2, 20, by=2) # start = 2, end=20, increase by 2
seq(2,20, length.out = 100)

rep(1:3, 2) # replication
rep(1:3, each=2) # specify the replication
rep(1:3, 1:3)
 # question 4: create vectors

a <- seq(1, 13, by=2)
print(a)

b <- rep(seq(1,5,2), seq(5,1,-2))
b

# extracting elements of a vector
x <- c(5,1,6,7,9)
x[3] # extract the third one
x[1:3]
x[-4]
x[-c(4,1)] # Remove both 4 and 1st

# calculation
x^2
y <- x + 1
x^y

# question 5
n <- 10^(0:7)
values <- (1 + 1/n)^n
values

# create matrix
cbind(n, values) # column bind
rbind(n, values) # row bind


# matrix function

x <- matrix(1:6, nrow=2, ncol=3)

# extract value from a matrix

x[1,2] # gives you the first row and the second column
x[1,] # specify only the row
x[1:2,]
x[1:2, -2] # keep the 2 rows and remove the 2nd column
x[1:2, c(1,3)]

# operation on a matrix 

x + x # dimension has to be the same dimension
x*x 
x%*%t(x) #multiplication

rbind(5:9, 12,(9:5)^2, c(2,8,4,7,3))


##########################################################################

x = "GeeksforGeeks" # string
x1 = 255            # integer
x2 = 23.14          # float 

# string print
sprintf("%s is best", x)

# integer print
sprintf("%d is integer", x1)

# float print
sprintf("%f is float", x2)

