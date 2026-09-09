## Data Handling

# Packages: tidyverse
# filter, arrange, mutate, select, summarize

library('tidyverse')
# this doesn' work anymore conflicts_prefer(dplyr::filter()) # avoid conflict from the data.
data <- read.csv("Dataset\\Bschool.csv")

# filter the data 
filter(data, student == "MBA" )

arrange(data, size, score)

select( data, semester, size, score)

mutate(data, smallsize = 1*(size<30))

summarize(group_by(data, student), mean(score))

# pipe operator : always starting with a dataset
# %>% 
# |> works for 4.1.0
data |> dplyr::filter(student=="MBA")|> 
   arrange(score, size)|> 
   mutate(smallsize = 1*(size<30)) # I have a problem with filter function again this time:!!
 
data |> group_by(student, semester)|> 
   summarize(
     mean.score = mean(score),
     median.score = median(score)
  )
 
 # question:
data |> filter(title == "Associate Professor" & gender == "f") |>
  summarize(mean.score = mean(score))
data |> filter(title == "Professor" & gender == "f")|>
  summarize(mean(score))
 data |> filter(title == "Associate Professor" , gender == "f" )|>
   mutate(number = sum())

# 04 September 2026 

# Linear Algebra 
A <- matrix(1:6, ncol=3)
B <- A %*% t(A)
diag(B)
diag(1:5)
D <- 1:5 %o% 1:5
lower.tri(D)
solve(B)
solve(B, 1:2)
det(B)
eigen(B)
chol(B)

# Practice question: generate a positive definite matrix of size n xn
# group 3 solution
# 
n <- 5
mat <- matrix(rnorm(25), nrow=n, ncol = n)
mat_pos <- mat %*% t(mat)
eigen(mat_pos)


# identity matrix

identity <- diag(rep(1, n))
eigen(identity)

# diagonal matrix
diag_matrix <- diag(rep(runif(n)))


# example from the homework
theta <- 0.5
n <- 10
i <- matrix(rep(0:(n-1), each=n), n, n) # replicate from zero to 9
j <- matrix(rep(0:(n-1),n), n, n)
eigen(exp(-theta*(i-j)^2))
