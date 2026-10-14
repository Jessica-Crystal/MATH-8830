### Simulation Setup
# switch
 n <- 100 # total number of sample
 distribution <- "normal" # change into heavy_tail or skewed
 # if dist is normal, switch to normal
 # if heavy tail switch to heavy tail
 x <- switch(
   distribution,
   normal = rnorm(n),
   heavy_tail = rt(n, df = 3)/sqrt(3),
   skewed = rexp(n) - 1
 ) 
hist(x) 

source("Week7/independent_two_sample_analysis.R")
