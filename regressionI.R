#####################################
#  23rd of october 2026
#####################################

#====================================
#      REGRESSION I
#====================================
bschool <- read.csv("Dataset/bschool.csv")

lm1 <- lm(score~size, data=bschool)

summary(lm1)

# E(score) = beta0 + beta1*size 
# H0: beta1 = 0 vs H1: beta1 not equal 0
# pvalue = 0.0109 *
# Reject H0 alpha = 0.05

# score ~gender 

lm2 <- lm(score~gender, data=bschool)
summary(lm2)
# E(score) = beta0 + beta1*I(gender=m)
# mean score for male professor = beta0 + beta1
# mean score for female professor = beta0 + 0

# H0: mean score for male and female are the same
# H1: mean score for male and female are not the same

# H0: beta0 + beta1 = beta0 ----> beta0 = 0
# H1: beta1 not equal to zero 
# pvalue: 0.377
# Do not reject H0


#====================================
# How this is related to cor.test?
#====================================

#=====================================
# is the above approach related to two sample t.test?
#=====================================


# score ~student
lm3 <- lm(score~student, data=bschool)
summary(lm3)
# E(score) = beta0 + beta1*I(student=MBA) + beta2*I(student=undergraduate)
# mean score for graduate level: beta0
# mean score for MBA: beta0 + beta1
# mean score for under: beta0 + beta2

# H0: mean score for MBA and graduate are the same 
# H1: H0 is not true 

# H0: beta1 = 0
# H1: beta # 0

# H0: mean score for MBA and undergraduate are the same ?

# p-value: 1.11e-07, ?HO

#===============================================
# 26 October 2026
#==============================================

fit_scalar <- function(x,y){
  xbar <- mean(x)
  ybar <- mean(y)
  sxy <- sum((x - xbar)*(y-ybar))
  sxx <- sum((x-xbar)^2)
  beta0 <- sxy/sxx
  c(intercept = ybar - beta0*xbar, slope = beta0)
}

x <- bschool$size
y <- bschool$score 

fit_scalar(x,y)

#===================================
# compare the scalar and matrix code
#====================================
fit_matrix <- function(predictors, y){
  predictors <- as.matrix(predictors)
  X <- cbind(Intercept=1, predictors)# why is this one?
  beta.hat <- solve(t(X)%*%X, t(X)%*%y) # betahat = (X^tX)^-1X^Ty
  drop(beta.hat)
}
#beta_matrix <- fit_matrix(data.frame(size=x), y)
beta_matrix <- fit_matrix(data.frame(x), y)
beta_matrix # give the same results

#===========================================
# Inference under normal errors
#===========================================
test_statistic <- function(x, y){
  
}
xbar <- mean(x)
ybar <- mean(y)
MSE <- sum((y-yhat)^2)
Sxx <- sum((x-xbar)^2)

#=========================================
# correlation test
#=========================================
cor.test(bschool$size, bschool$score)
# is the above approach related to two.sample t.test 
# 1. paired t 
# 2. independent two sample t with variance equal
# 3. independent two sample t with variance not equal
# 4. none of the above 
# ---> the answer is nb 2

#================================================
# 28 october 2026
#===============================================
# H0: mean score for MBA and undergrade are the same
# H1: H0 is not true

# H0: beta1=0
# H1 beta1 # 0
# pvalue= 5.04e-08 ***
# reject the H0 alpha = 0.05

# H0:(0,1,0)(beta0, beta1, beta2)^T=0

# H0: mean score for MBA and undergrade are the same 
# H1 H0 is not true
library(tidyverse)

bschool.r1 <- bschool |> 
  mutate(
    student.r1 = factor(student,
                        levels=c("Undergraduate",
                                 "Graduate",
                                 "MBA")
  ))



lm4 <- lm(score~student.r1, data=bschool|>
            mutate(
              student.r1=factor(student,
                                levels=c("Undergraduate",
                                         "Graduate",
                                         "MBA"))
              
            ))
summary(lm4)

# model with interactions 
bschool$smallsize <- 1*(bschool$size<30)
lm5 <- lm(score~size + smallsize, data=bschool)
summary(lm5)

# smallsiz1 = 0
# E(score = beta0 + beta1*size 

# smallsize = 1"
# E(score = beta0 + beta2 + (beta1+beta3)*size 



# E(score) = beta0 + beta1*size + beta2*I(smallsize = 1)
plot(score~size, data=bschool)
# fit the model 
lm6 <- lm(score~size*smallsize,data=bschool)
summary(lm6)

# E(score) = beta0 + beta1*size + beta2*I(smallsize=1) + beta3*size*I(smallsize=1)
# if smallsize =0 so all the indicator gone 
# E(score) = beta0 + beta1*size 

# if smallsize = 1 all the indicator function is equal to 1
# E(score) = (beta0 + beta2) + (beta1 + beta3)*size

# H0: smallsize class and nonsmallsize class have the same slopes 
# slope for smallsize class: beta1 + beta3
# slope for nonsmall size: beta1
# H0: beta3 = 0

# what's the null hypothesis for the 
# F-statistic: 6.767 on 3 and 336 DF,  p-value: 0.0001921
# H0: none of the predictors(does not include intercept) are useful in the model 
# H0: beta1=beta2=beta3=0


lm7 <- lm(score~student*title, data=bschool)
summary(lm7)

# Expected value E(score) = beta0 + beta1*I(s=MBA) + beta2*I(s=u) + beta3*I(gender=m)+
# beta4*I(gender=m, s=MBA) + beta5*I(gender=m, s=under)

# gender = female
# E(score) = beta0 + beta1*I(s=MBA) + beta2*I(s=u) 

# gender = male 
# E(score) = (beta0 + beta3) + (beta1+beta4)*I(s=MBA) + (beta2 + beta5)*I(s=under)

# H0: mean score of MBA courses are the same for male and female professor 
# mean score MBA with female:beta0 + beta1
# mean score MBA with male: beta0+beta3+beta1+beta4
# H0: beta1+beta4=0

# H0: beta4 = 0
# H0: beta5=0
# translate into statements