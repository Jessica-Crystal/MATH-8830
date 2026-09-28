#How many unique baseball players are recorded in the People dataset
# How many unique baseball leagues (lgID) are represented in the Teams dataset?

# What is the 95th percentile of player salaries?

percentile_95 <- quantile(Salaries$salary, prob=0.95, na.rm=TRUE)

# What was the average age of players when they played their final recorded game?
 People |> select(birthYear, deathYear) |>
   mutate(
  age = deathYear - birthYear
) |> summarize(
  avg = mean(age, rm.na=TRUE)
)



#Which team has had the greatest number of unique players whose salaries were above the 95th 
#percentile of all salaries recorded in the Salaries dataset?

Salaries |> filter(
  !is.na(salary),
  salary >= percentile_95
)|> count(
  teamID
) |> slice_max(n, with_ties=TRUE)

# Which player experienced the greatest increase in total 
#annual salary between their first and last recorded seasons?
Salaries |> 
  filter(
    !is.na(salary)
  ) |> count(
    playerID
  ) left_join(
    Batting, by = "yearID"
  )|> slice_max(n, with_ties=TRUE)
