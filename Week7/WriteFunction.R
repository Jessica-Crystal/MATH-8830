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
# compare two methods and generate different data?:: confused here
mydata <- generate_two_sample_data(
  n_x = 100,
  n_y = 100,
  difference = 2,
  contamination = 0.3,
  outlier_size = 100
  
)
two_sample_test(mydata$x, mydata$y, method="t") # this provide the pvalue

two_sample_test(mydata$x, mydata$y, method="rank")

methods <- c("t", "rank", "permutation")
sapply(methods, function(method){
  two_sample_test(mydata$x, mydata$y, method=method)
})
