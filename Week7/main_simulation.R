# Independent two-sample simulation study
library(tidyverse)
source("Week7/independent_two_sample_analysis.R")

repetitions <- 10
alpha <- 0.05
distributions <- c("normal", "heavy_tail", "skewed")
methods <- c("t", "rank", "permutation")
settings <- list(
  n = c(10, 30),
  difference = c(0, 1),
  distribution = 1:3,
  contamination = c(0, 0.3)
)

cases <- expand.grid(settings)

results <- NULL
set.seed(10)
for(i in 1:nrow(cases)) {
   n <- cases$n[i]
   difference <- cases$difference[i]
   distribution_id <- cases$distribution[i]
   contamination <- cases$contamination[i]
   
   for(rep in 1:repetitions) {
    mydata <- generate_two_sample_data(n_x=n, n_y=n,
                            difference=difference,
                            distribution=distributions[distribution_id],
                            contamination=contamination)
   
    pvalues <- sapply(methods, function(method) {
      two_sample_test(
        mydata$x,
        mydata$y,
        method = method
      )
    })
   
    results <- rbind(results, c(n, difference, distribution_id, contamination, 
                              pvalues,rep))
   }
}

results <- data.frame(results)
names(results) <- c("n", "difference",
                    "distribution",
                    "contamination",
                    "t","rank","permutation","rep_id")

results <- results |>
  mutate(
    distribution=distributions[distribution]
  ) |>
  gather(
    method, pvalue, -(n:contamination), -rep_id
  )
save(results, file="results.RData")

