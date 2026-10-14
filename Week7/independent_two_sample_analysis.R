# 1. Generate two independent samples
generate_two_sample_data <- function(
    n_x = 30,
    n_y = 30,
    difference = 0,
    sd_x = 1,
    sd_y = 1,
    distribution = "normal",
    contamination = 0,
    outlier_size = 8) {

  draw_sample <- function(n, location, scale) {
    z <- switch(
      distribution,
      normal = rnorm(n),
      heavy_tail = rt(n, df = 3) / sqrt(3),
      skewed = rexp(n) - 1
    )

    values <- location + scale * z
    outlier <- runif(n) < contamination
    values[outlier] <- values[outlier] +
      sample(c(-1, 1), sum(outlier), replace = TRUE) * outlier_size * scale
    values
  }

  list(
    x = draw_sample(n_x, difference, sd_x),
    y = draw_sample(n_y, 0, sd_y)
  )
}


# 2. Conduct one two-sample test and return its p-value
two_sample_test <- function(
    x,
    y,
    method = "t",
    alternative = "two.sided",
    permutations = 999) {

  if (method == "t") {
    return(t.test(x, y, alternative = alternative)$p.value)
  }

  if (method == "rank") {
    return(wilcox.test(
      x, y,
      alternative = alternative,
      exact = FALSE
    )$p.value)
  }

  if (method == "permutation")   {
    observed <- mean(x) - mean(y)
    nx <- length(x)
    pooled <- c(x, y)

    permuted <- replicate(permutations, {
      shuffled <- sample(pooled)
      mean(shuffled[1:nx]) - mean(shuffled[-(1:nx)])
    })

    switch(
      alternative,
      two.sided = (sum(abs(permuted) >= abs(observed)) + 1) /
        (permutations + 1),
      less = (sum(permuted <= observed) + 1) / (permutations + 1),
      greater = (sum(permuted >= observed) + 1) / (permutations + 1)
    )
  }
}


