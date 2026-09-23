### Week 6
# 23 septemebr 2026
library(tidyverse)

library(nycflights13)
planes
flights
airports
weather
table(flights$dest)

# q1: distinct origin and dest pairs
flights |> distinct(origin, dest) |> nrow()


flights |> group_by(origin, dest) |> summarize(number_of_flights = n()) |> ungroup() |>
  slice(which.max(number_of_flights))

# slice_max 
# q2: bigplanes vs smallplanes
glimpse(planes)
flights |> left_join(planes, by="tailnum")|> 
  filter(!is.na(seats))|>
  mutate(
    bigplanes = 1*(seats>200)
    
  )|> group_by(bigplanes, carrier) |> summarize(
    median_delay = median(arr_delay, na.rm=TRUE)
  )|>
  spread(
    bigplanes, median_delay
  )

#q3: weather related variables have association to the delay
flights_weather <- flights |> 
  left_join(weather)|>
  select(
    arr_delay, temp:visib
  )
cor(flights_weather, use = "complete.obs")
flights_weather |> 
  gather(
    metric, value, -arr_delay
  )|>
  group_by(metric)|>
  summarize(
    r=cor(arr_delay, value, use = "complete.obs")
  )

#q4 which airline has the gratest number flights with arrival delays above 95th percentile
delay_95 <- quantile(flights$arr_delay, prob=0.95, na.rm=TRUE)

flights |> filter(
  !is.na(arr_delay),
  arr_delay > delay_95
)|>
  count(
    carrier
  )|>
  left_join(
    airlines, by="carrier"
  )|>
  slice_max(n, with_ties=TRUE)

# COMPARE ARR_DELAY OF OLD PLANES AND NEW PLANES
# WHICH FLIGHT ROUTE HAS THE HIGHST AVERAGE ARRIVAL DELAY
