library(tidyverse)
#q1 how many unique baseball players are recorded in the Lahman People dataset
People |> summarize(number_of_players = n_distinct(playerID)) # give the number of playerID
nrow(People)

#q2: how many unique baseball leagues(say, lgID) are
# represented in the Lahman Teams dataset?

Teams |> summarize(number_of_leagues = n_distinct(lgID))

#q3: 

quantile(Salaries$salary, probs = 0.95, na.rm = TRUE)


#q4: average age of final game of all players 

People |> mutate(
  birth_date = make_date(birthYear, birthMonth, birthDay),
  final_game_date = as.Date(finalGame),
  age_at_finale_game = time_length(
  interval(birth_date, final_game_date),
  unit = "years"
),
age_at_final = (final_game_date - birth_date)/365)|>
  summarise(average_age_at_final_game = mean(age_at_finale_game, na.rm = TRUE),
            average_age = mean(age_at_final, na.rm=TRUE))

#q5 which team have had the more players in history
# with the salaries over 95 quantile of all the salaries 
# in the salary data 
# calculate the 95th pecentile of all salaries
salary_95 <- quantile(
  Salaries$salary,
  probs = 0.95,
  na.rm=TRUE
)

# count the unique high salary players by team 
high_salary_players <- Salaries |>
  filter(salary > salary_95)|>
  distinct(playerID, teamID) |>
  group_by(teamID)|>
  count()

# add team names and identify the team with the most players
team_names <- Teams |> 
  group_by(teamID)|>
  summarize(
    team_name = first(name)
  )


# combine with the previous dataset

high_salary_players |>
  ungroup()|>
  left_join(team_names, by = "teamID")|>
  slice(which.max(n))
