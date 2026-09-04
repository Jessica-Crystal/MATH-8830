
##############____________________QUESTION 3
# load the dataset
Bschool <- read.csv("Dataset\\Bschool.csv")

#1. Which professor rank (i.e., associate, assistant, or professor) has the highest average teaching evaluation score?

mean_score <- tapply(Bschool$score, Bschool$title, mean)
highest <- max(mean_score)
cat("The professor who has the higher rank", highest)
#2. What is the number of female associate professors? 
  
female_associate_professors <- sum(Bschool$title =="Associate Professor" &
                                     Bschool$gender == "f")
cat("The number of female associates professor:", female_associate_professors)
#3. Is median teaching evaluation score in fall semester usually higher than that in spring semester?

# No, the median teaching evaluation score in fall semester is lower than the spring semester
eva_fall <- median(Bschool$score[Bschool$semester == "fall"])
eva_spring <- median(Bschool$score[Bschool$semester == "spring"])
eva_fall >=  # this return FALSE, so the answer is nO§
cat("The median score that is high is in the spring", eva_spring)

