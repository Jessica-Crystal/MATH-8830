#####Week7
# 28 of september 2026

library(tidyverse)

glasses <- read.csv("Dataset/glasses.csv")
z <- glasses$Without.Glasses - glasses$With.Glasses
n <- length(z)
z_bar <- mean(z)

round(c(n=n,
        mean= z_bar,
        median = median(z),
        negative = sum(z<0),
        positive = sum(z>0),
        zero = sum(z==0)), 3)
set.seed(10)
R <- 10000

random_signs <- matrix(
  sample(c(-1,1), R*n, replace=TRUE),
  nrow=R,
  ncol=n
)

random_signs[1,]*z

results <- random_signs %*% z/n
hist(results)


# what is the proba that the value of mean is less that
# or equal to z_bar = -4.76
pvalue <- mean(results <= z_bar)

# p-value: unoffcial risk of reject H0 hypothesis
# compute the proba of more exteme situation
# under distribution of null hypothesis
extreme <- sum(results <= z_bar)
(extreme+1)/(R+1)

#Question in class



# order statistics
set.seed(10)

ranks <- rank(abs(z))
sr <- sum((z<0)*ranks)

random_signs <- matrix(
  sample(c(-1,1), R*n, replace=TRUE),
  nrow=R,
  ncol=n
)

sign_ranks <- random_signs%*%ranks
extreme <- sum(sign_ranks >=sr)
(extreme+1)/(R+1)
p_value <- mean(sign_ranks<0)

#Zi <- age_without_glasses - age_with_glasses


Zi <- glasses |> mutate(difference = Without.Glasses - With.Glasses)
mean(Zi$difference)

### 30 september 2026
# COMPARISON

glasses <- read.csv("Dataset/glasses.csv")
n <- nrow(glasses)
z <- glasses$Without.Glasses - glasses$With.Glasses

# perform the t test
t.test(glasses$Without.Glasses, glasses$With.Glasses, paired=TRUE, alternative = "less")

# paired t test 
t <- mean(z)/(sd(z)/sqrt(n))

# Ha: mean < 0
#
#P(T_{n-1}<= t)
pt(t, n-1) # finding the pvalue

# binomial test
# X=number of negative Binom(n=50, p=0.5

#
z_nonzero <- z[z!=0] # remove all the zero in z 
n_nonzero <- length(z_nonzero)
x_negative <- sum(z_nonzero<0)

# calculte extreme obs based on binomial 
# exact pvalue
k <- 0:n_nonzero
null_proba <- choose(n_nonzero, k)*0.5^n_nonzero
p_sign <- sum(null_proba[k>=x_negative])


# approximation with normal
#n*p and n*(1-p)) > 5, if it's not it is not safe to use the normal approx
# X~N(np, n*(1-p)) approximate
# P(X>=x_negative)

1 - pnorm(x_negative, mean=50*0.5, sd=sqrt(50*0.5*0.5))
# independent two samples

new_method <- c(27, 49, 55, 57)
traditional <- c(23, 31, 46)
# H0: new method and trad no difference
# Ha: new method is better >= 0
# improve the location of the scores

# permutation
# mean_difference
delta_observed <- mean(new_method) - mean(traditional)
label <- c(1,1,1,1,2,2,2)
scores <- c(new_method, traditional)
sample(scores)

R <- 10000
deltas <- replicate(R,
          {
            resample_score <- sample(scores)
            mean(resample_score[1:4]) - mean(resample_score[5:7])
          })
mean(deltas>= delta_observed)
