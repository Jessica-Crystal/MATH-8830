########################################
# October 21st2026 - review
#######################################


library(tidyverse)

bschool <- read.csv("Dataset/Bschool.csv") 
#===========================================
#1. is size and score negatively correlated?
#===========================================
#H0: correlation =0 vs H1: correlation <0

cor.test(bschool$score, bschool$size, alternative = "less")
# correlation method, default method is the q distribution
# the test stat is the correlation coefficient 
cor.test(bschool$score, bschool$size, alternative = "less", method = "kendall")
cor.test(bschool$score, bschool$size, alternative = "less", method = "spearman")

#=========================================
# non parametric method
#=========================================

x <- 1:7
y <- (1:7)^2
cor.test(x,y)
cor.test(x,y, method = "kendall")
cor.test(x,y, method = "spearman")

#================================================
#2. fall and spring semester diff in mean scores?
#================================================
# semester is qualitative and score is quantitative 
boxplot(score~semester, data=bschool) # spring is higher
t.test(score~semester, data=bschool)
t.test(score~semester, data=bschool, var.equal=TRUE)
t.test(score~semester, data=bschool, var.equal=FALSE)

# non parametric method 
wilcox.test(score~semester, data=bschool)
# correct = TRUE IS THE DEFAULT;conduct continuity correction

#=============================================
#3. students groups diff in mean scores?
#==============================================

# analyse of variance
# H0: the mean scores are all the same: mu_M=mu_under=mu_grad
# H1: at least one of mu_M,mu_under, mu_grad is different from others
aov(score~student, data=bschool)
summary(aov(score~student, data=bschool))

# non parametric version of aov test 
kruskal.test(score~student, data=bschool)

#======================================
#4. female professor teaches more MBA?
#======================================
# THE association between two qualitative factors, each with two levels
# H0;there is no association
# H1: there are some association
# H0: proportion female  teach MBA = proportion male teach MBA
# H1: proportion of female teach MBA > proportion male that teach MBA
# proportion female: 27/59
# proportion male: 105/281

table(bschool$gender, bschool$student)
table(bschool$gender)

x <- c(27, 105) # collect the nb of success (female and male teaches MBA courses)
n <- c(59, 281)

prop.test(x, n, alternative="greater")

hist(runif(30))
plot(density(runif(30)))
# continuity correction
prop.test(x, n, alternative="greater", correct=FALSE)


#========================================================
#5. course assignment has association with professor rank?
#=========================================================
# student and professor var 
# association  between two qualitative factors
# H0: no association between student and professor rank
# H1: there are some association ie H0 is not true 
# conduct the chi squarre test 

chisq.test(bschool$title, bschool$student)
chisq.test(bschool$student, bschool$title)

# non parametric metric

#chisq.test(bschool$student, bschool$title)
