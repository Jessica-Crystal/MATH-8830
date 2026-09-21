rep(c(0,1), 2)
ncol(mydata)
nrow(mydata)

mydata |> group_by(QSB)|> nrow() 


n<- 1000000
k <- 1:n
sum(1/(n+k)) - log(2)

j <- 2:100
i <- 3:20
sum(i^2/(7+4*j))
for(i in 2: 100){
  for(i in 3:20){
    x <- i^2/(7+4*j)
   
  }
  sum(x)
}
 
z <- runif(1:3)
pnorm(3) - pnorm(1)

#Column "QS4" records the area (e.g., rural, suburb, etc.) of each row. 
#Calculate the proportion of row in each area type for every community.
#Which community has the highest proportion of rows from a suburb?

val2 <- mydata |> group_by(QSB, QS4) |> summarize(mean_val = mean(n()))
# What is the mean of the column "LOYALTY" after removing missing values?
val1 <- mydata |> group_by(QSB) |>  summarize(mean_val = mean(LOYALTY), rm.na=TRUE)

#How many communities contain exactly 400 rows?
 val <- mydata |> group_by(QSB) |> summarize(mean_value = n())
 
 
 
 #Among the columns  "LEADERSH", "EDUCATIO", "SAFETY", "AESTHETI", and 
 #"ECONOMY", which has the highest correlation with the column  LOYALTY?
 
 mydata |> group_by(LEADERSH, EDUCATIO, SAFETY, AESTHETI, ECONOMY) |>
   ggplot(aes(x = LOALTY)) +
   facet_grid()
   
 
A<- runif(100)
B<- runif(100)
C <- runif()
dis <- B^2 - 4*A*C

proba <- mean(dis>0)
41/216  
43/216 
45/216 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 