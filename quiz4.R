# quiz4
#Variables Q1A and Q1B give each respondents
#current-life evaluation and expected future-life 
#evaluation on a numerical scale from 0 (worst possible life) to 10 (best possible life).
#After removing incomplete pairs, what is the observed mean difference of expected future
#life evaluation and current life evaluation? """

current <- mydata$Q1A 
future <- mydata$Q1B

keep_paired <- complete.cases(current, future)
current <- current[keep_paired]
future <- future[keep_paired]

z <- future - current 
n <- length(z)
z_observed <- mean(z)

# Q1: 0.509

B <- 5000 
null_mean <- numeric(B)
for (b in seq_len(B)){
  random_sign <- sample(c(-1, 1), n, replace = TRUE)
  z_star <- random_sign * abs(z)
  null_mean[b] <- mean(z_star)
}
 p_paired_permutation <- (1 + sum(null_mean<= z_observed))/(B + 1)
 
 
 #Q2:  >= 0.05 
 t.test(future, current, paired=TRUE, alternative = "less")
 # Q3: >=0.05 
 
 # Q4-Q6
 library(tidyverse)
 Q1A <- mydata |> 
   filter(!is.na(Q1A)) |> 
   select(Q1A, CITY)|>
   mutate(CITY=ifelse(CITY=="City", 1, 0)
  )

 x <- Q1A$Q1A[Q1A$CITY==1]
 y <- Q1A$Q1A[Q1A$CITY==0]
 
 delta <- mean(x) - mean(y)
 
 #Q4: -0.5 
 
 B <- 5000 
 xy <- c(x, y)
 n <- length(x)
 N <- length(xy)
 deltas <- NULL
 for(i in 1:B){
   xy_permutation <- sample(xy)
   deltas <- c(deltas,
               mean(xy_permutation[1:n])- mean(xy_permutation[(n+1): N]))
 }
mean(abs(deltas)>= abs(delta)) # there is a problem here
# Q5 >0.1

t.test(x,y)
#Q6 >0.1