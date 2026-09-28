library(tidyverse)
aqi_data <- load("C:/Users/FRT/Downloads/Fall-2026/MATH-8830/Dataset/pm.Rdata")
# the aqi_data contains two object data05, data14
nrow(data05)
nrow(data14)


p1 <- data05 |> group_by(Date.Local) |> summarize(average = mean(AQI))
p2 <- data14 |> group_by(Date.Local) |> summarize(average = mean(AQI, na.rm=TRUE))

p1 |> ggplot(aes(x=Date.Local, y=average)) +
  geom_line()


#Movie data

movie_data <- read.delim("Dataset/movies.tab")

p11 <- movie_data |> ggplot(aes(length)) +geom_histogram() # with outlier

#q4 which airline has the gratest number flights with arrival delays above 95th percentile
percentile_95 <- quantile(movie_data$length, prob=0.95, na.rm=TRUE)

data1 <- movie_data |> filter(
  !is.na(length),
  length <= percentile_95
)

p12 <- data1 |> ggplot(aes(length)) + geom_histogram(bins=30)
p12
