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


