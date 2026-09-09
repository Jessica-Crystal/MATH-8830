########################
# Tidy and More II
#########################
# 9th September 2026

library('tidyverse') # loaded together with ggplot2
bschool <- read.csv("Dataset\\Bschool.csv")
save(bschool, file = "bschool.RData")
load("bschool.RData")

bschool |> slice(1:10) # slice is a dplyr function the select the rows
# is.data.frame(data)
# is_tibble(data)
# is.matrix(data)
bschool |> slice(which.max(score)) # output the max score

###########__________Graph analysis using tidyverse_____________________________
# Single column qualitative
# bschool$score
p1 <- bschool |> 
  ggplot(aes(x=score)) + # every figure start with ggplot and then after then components and the last part is the decoration
  geom_histogram()

# or 

p1 <- bschool |> 
  ggplot()
p1 + geom_density(aes(x=score))
p1+geom_boxplot(aes(x=score)) + 
  coord_flip() + 
  theme_bw() +
  xlab("value")


# single qualitative column: 
bschool |> 
  mutate(
    gender = ifelse(gender=="f", "female", "male")
  )|>
  ggplot(aes(x=gender)) + 
  geom_bar()

# relationship: quantitative vs qualitative : score vs student

p12 <- ggplot(bschool, aes(x=score))
p12 + geom_histogram()+
  facet_grid(student~.) # split the student 


p12 + geom_histogram()+
  facet_grid(.~student) # split the student 

p1 <- ggplot(bschool)
p1 + geom_density(aes(x=score,color=student, fill=student), alpha=0.5)
  # split the student 

p1 + geom_density(aes(x=score,color=semester)) +
  facet_grid(student~title)

p1 + geom_boxplot(aes(x=student, y = score))# split the student 

## qualitative vs qualitative
bschool |> 
  ggplot(aes(x=title, fill=gender))+
  geom_bar(position="dodge") # to put the things side by side

# qualitative factor with more levels
bschool |> group_by(title, gender)|>
  summarize(
    freq = n()/nrow(bschool) # n() is the count number in the data
  )|>
  ggplot(aes(x=title, y=gender))+
  geom_tile(aes(fill=freq))# similar to heatmap; useful to see the structure of a covariance matrix

       