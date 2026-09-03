## Data Handling

# Packages: tidyverse
# filter, arrange, mutate, select, summarize

library('tidyverse')

data <- read.csv("Dataset\\Bschool.csv")
filter(data, student == "MBA" )
arrange(bschool, size, score)
