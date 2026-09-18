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

## 18th september 2026. Last topic on the data handling
glimpse(flights) # gives the glimpse of the flights
# average arr_delay each day
flights |> 
  group_by(month, day) |>
  summarize(
    avg_arr_delay = mean(arr_delay)
  )
summary(flights) # to many NA values 

flights |> 
  group_by(month, day) |>
  summarize(
    avg_arr_delay = mean(arr_delay,
                         na.rm = TRUE)
  )
# IN THE EXAM? there might be NA values, so need to remove them before calculation


flights |> 
  group_by(month, day) |>
  summarize(
    avg_arr_delay = mean(arr_delay,
                         na.rm = TRUE),
    .groups = "drop"
  ) # remove the groups:month

## replace the missing values: fix the missing values

flights |> 
  group_by(month, day) |>
  mutate(
    arr_delay_na_rm = replace_na(
      arr_delay, mean(arr_delay, na.rm=TRUE)
    )
  ) # be careful using it

## illegal computation and numerical issue
# example
sqrt(-1)

## empty group
health <- tibble(
  id = 1:5,
  smoker = rep("No", 5)
)

table(health$smoker)|> barplot()
health1 <- tibble(
  id = 1:5,
  smoker = factor(rep("No", 5), levels = c("Yes", "No")
)
)
table(health1$smoker)|> barplot()
####################################################"
flights
date_time_now <- now()
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd("2017-01-31", tz="UTC")
wday("2017-01-31", label=TRUE)
a <- ymd("2017-01-31") - ymd("2016-01-31")
a/365
b <- ymd("2017-01-31") - ymd("2015-01-31")
a>b
time_length(interval(("2016-01-31"),ymd("2017-01-31")), unit="years")
time_length(interval(("2016-01-31"),ymd("2017-01-31")), unit="days")

############################
time_delay <- flights |> 
  select(year, month, day, hour, minute, arr_delay)|>
  mutate(
    departure = make_datetime(year, month, day, hour),
    weekday = wday(departure, label=TRUE),
    week = floor_date(departure, unit="week")
      
    ) # foor_date means first day of the week
  
time_delay |> 
  group_by(departure)|>
  summarize(
    mean_delay = mean(arr_delay, na.rm=TRUE)
  )|>
  ggplot(
    aes(x=departure, y=mean_delay)
  )+ 
  geom_line()

time_delay |> 
  group_by(week)|>
  summarize(
    mean_delay = mean(arr_delay, na.rm=TRUE)
  )|>
  ggplot(
    aes(x=week, y=mean_delay)
  )+ 
  geom_line()
###################################################
# new functions: ceiling_date, round_date

time_delay |> 
  group_by(weekday) |>
  summarize(
    mean_delay = mean(arr_delay, na.rm=TRUE)
  ) |>
  ggplot(
    aes(x = weekday, y=mean_delay)
  ) + 
  geom_bar(stat = "identity")
