
##############____________________QUESTION 3
# load the dataset
Bschool <- read.csv("Dataset\\Bschool.csv")

#1. Which professor rank (i.e., associate, assistant, or professor) has the highest average teaching evaluation score?
  p1 <- mean(Bschool$score[Bschool$title == "Associate Professor"])
  p2 <- mean(Bschool$score[Bschool$title == "AssistantProfessor"])
  p3 <- mean(Bschool$score[Bschool$title == "Professor"])
  highest <- max(p1, p2, p3)
  cat("The professor who has the higher rank", highest)
#2. What is the number of female associate professors? 
  
female_associate_professors <- sum(Bschool$title =="Associate Professor" &
                                     Bschool$gender == "f")
cat("The number of female associates professor:", female_associate_professors)
#3. Is median teaching evaluation score in fall semester usually higher than that in spring semester?

# No, the median teaching evaluation score in fall semester is lower than the spring semester
eva_fall <- median(Bschool$score[Bschool$semester == "fall"])
eva_spring <- median(Bschool$score[Bschool$semester == "spring"])
max(eva_fall, eva_spring)
cat("The median score that is high is in the spring", eva_spring)

