################################################################

#28th of August 2026
# Learning Base: Part 4
# create a data set
# difference between matrix and data frame: in a data frame has different type of variables
# in a matrix, you can only have a numerical value
 Bschool <- read.csv("Dataset/BSchool.csv")
 # Bschool <- read.csv(file.choose())
 head(Bschool)
 nrow(Bschool) # gives the number of row in the dataset
 ncol(Bschool)
# Qualitative columns
 # --> title, gender, student, semester
 # Quantitative columns
 # --->  size, score 
 # year: can be a quantitative or qualitative depends on the stats problem
 
 # title
title.table <- table(Bschool$title)
barplot(title.table)
pie(title.table)
 # Quantitative: score, size
# score
# summary statistic
# location/center: mean, median
mean(Bschool$score)
median(Bschool$score)

# variability/spread: variance, std, quantile
var(Bschool$score)
sd(Bschool$score)
quantile(Bschool$score, prob=0.75)

# Q1, Q2, Q3
quantile(Bschool$score, prob=c(0.75, 0.5, 0.25))
max(Bschool$score)
# IQR: range
quantile(Bschool$score, prob=0.75) - quantile(Bschool$score, prob=0.25)


# Graphs
hist(Bschool$score)
boxplot(Bschool$score) # to plot the Quantile, min, and max after removing the layer


### Relationship of columns ######

# quantitative vs quantitative
# score ~ size 
# ---> summary statistics
cor(Bschool$score, Bschool$size)
# ---> graphs

plot(score~size, data= Bschool)
# further split one if the col in ti qualitative

# quantitative vs qualitative
# score ~student

tapply(Bschool$score, Bschool$student, mean)
tapply(Bschool$score, Bschool$student, quantile, prob = 0.75) #apply on the 3rd quantile"
boxplot(score~student, data= Bschool)


# qualitative vs qualitative 
# gender vs student
# 31- August -2026
# summary statistics
Bschool.table <- table(Bschool$gender, Bschool$student)
# plotting
barplot(Bschool.table)

# 1. total number of coures offered in Fall semester

#nb_fall_courses <- nrow(Bschool)
table(Bschool$semester)

# 2. total number of MBA courses offered on Fall semesster
#mba_fall_courses <- nrow(Bschool$student == "MBA")
table(Bschool$semester, Bschool$student)

#3. score vs title
tapply(Bschool$score, Bschool$title, mean)

# 4. SCORE VS STUDENT
tapply(Bschool$score, Bschool$student, mean)
 #######################################################
#             ---------DATA SUBSETING--------------
# example
Bschool[Bschool$size > 100, ] # subsetting size more than 100 
Bschool[Bschool$size > 100 & Bschool$score > 18,  ] # subsetting size greater than 100 and score more than 18
Bschool[Bschool$size > 100 | Bschool$score > 18,  ]
Bschool[Bschool$student == "MBA",]
 # number  MBA courses taught by female associates prof
nb <- Bschool[Bschool$title =="Associate Professor"&
          Bschool$student =="MBA"&
          Bschool$gender=="f",]
nrow(nb)

# teachers' code
sum(Bschool$title == "Associate Professor" &
Bschool$gender == "f"&
Bschool$student == "MBA")
# average evaluation score for graduate courses taught by male assist prof
mean(Bschool$score[
  Bschool$title == "Associate Professor"&
  Bschool$gender == "m" & 
  Bschool$student=="Graduate" ]
  
)

# which professor rank had the highest average evaluation score for MBA courses

