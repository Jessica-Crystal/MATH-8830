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
