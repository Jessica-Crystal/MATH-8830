####################
# Tidy and More III
####################

library(tidyverse)
## 14 september 2026

ggplot(iris, 
       aes(
         x=Petal.Length, y=Petal.Width
       )) +
  geom_point(aes(color=Species, shape=Species)) +
  geom_smooth()

ggplot(iris, 
       aes(
         x=Petal.Length, y=Petal.Width
       ))+
  geom_point() +
  facet_grid(.~Species)

bschool <- read_csv("Bschool.csv")


p22 <- bschool |>
  mutate(
    smallsize=1*(size<=30)
  ) |>
  ggplot(aes(x=size, y=score, group=smallsize)) +
  geom_point() +
  geom_smooth(method="lm")

p12 <- ggplot(bschool, aes(x=score))
p12 <- p12 +  geom_boxplot(aes(y=student))


library(gridExtra)
grid.arrange(p22, p12, ncol=2)

### 16 september 2026

# give a domaine on a data
table1
table2 
table3
 ## data tidy

# handling data: changing table1 to table 2 or vice versa
table2 |>
  spread(
    key = type, 
    value = count
  )
# change the table 1 into table2
table1 |> 
  gather(
    key = type,
    value = count, -country, -year
    
  )
# they are useful for ploting


# function to generate 1D latin hypercube sample
lhs1 <- function(n) {
  x <- sample(1:n)
  x <- x-runif(n)
  x <- x/n
  return(x)
}

x.lhs <- lhs1(n)

int.approx <- function(u) {
  mean(sin(u))
}



## The simulation code
set.seed(10)
n <- 100
nrep <- 10
lhs.mean <- replicate(nrep, int.approx(lhs1(n)))
iid.mean <- apply(matrix(runif(n*nrep), nrep, n), 1,
                  int.approx)
iid.data <- as.data.frame(iid.mean)
lhs.data <- as.data.frame(lhs.mean)

cbind(lhs.mean, iid.mean)

results1 <- tibble(
  rep.id = 1:nrep,
  lhs = lhs.mean,
  iid = iid.mean
)


results2 <- results1 |> gather(
  method, est, -rep.id
  
)
 # summary analysis
true_mean <- 1 - cos(1)
results2 |> 
  group_by(method)|>
  summarize(
    bias = mean(est) - true_mean,
    var = var(est),
    mse = var(est) + bias^2
  )

# graph analysis
results2 |> ggplot(aes(x = method, y=est)) + 
  geom_boxplot()

results2 |> ggplot(aes(x = method, y=est - true_mean)) + 
  geom_boxplot()+
  ylab("bias")
 library(tidyverse) 
# Graph the results2
results2 |> ggplot(aes(x=rep.id, y=est, color=method)) +
  geom_point()

library(nycflights13)
flights
weather
airlines
planes
airports
# relationship between the dataset
# missing values
# time and dates
# multiples date