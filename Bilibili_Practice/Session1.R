##session 1 bilibili
install.packages() #instaill packages
library(ggplot2) #get out! package_name!
update.packages() # update all packages available

v= c(2,1,2,3,4,5,6,3,9) # name a vector "v", which appear in the right window
?c # use "?' to call help desk

a=c(1:5, 10.5, "next")
b<- 1:4

v[c(2,3,4)] #1. to call objects in the vector
v[c(2:4)] #2. to call objects in the vector
v[c(2,4,3)] #3. to call objects in the vector
v[-2] #delete the 2nd object in v
v[-2:-4] #delete the 2nd, 3rd and 4th  object in v. If you wanna get v back to priginal, use "="

#You don't have to re-write another line of "v=...", just find the original line and run it!!! Both r and sql
#(I am afraid most or even all programming languaes) are executed by your order, not automatically from upwards to downwards!)

v[v<4] #call the 1st, 2nd and 3rd objectives in v, i.e., the first 3 objectives in v.

which(v==3) #we use "==" to give value to v, and find object values of 3. They are the 3rd and 7th one.
which.max(v) # the number R retuns is the sequence of the vetor, not the value!
which.min(v) #the 2nd one is the smallest in the vector

set.seed(250)
a=runif(3, min=0,max=100 ) # get randum numbers
rnorm(4,1,2) #get four numbers from normal distribution

floor(a) # get integer by removing all decimal part
ceiling(a) # get integer by removing all decial part and add 1
round(a,4) # make a with 4 digits

??bin #if one "?" does not help, get more "?"😂

data3 <- read.csv(file = "~/Desktop/CodingInR🆗/Coding1/AMZN.csv") # read data from local, or we can skip "file="

h_v <- read.csv("~/Desktop/CodingInR🆗/Coding1/hotelbookingdata.csv")
data2=read.table("/Users/wodediannao/Desktop/DE1SQL/HW1/LifeExpactancy_byCountry.csv") # Data input, error! will find later


attach(hotels_vienna) # "attach" makes every colomn/variable in the table change into vectors
city # we can use city as a vector(like "v")

#
set.seed(123)
x=rnorm(100, mean = 100,sd=10)
set.seed(234)
y=rnorm(100, 100, 10)
hist(x,breaks = 20, col="pink") #we use "histogram" to draw the frenquency disgram

library(ggplot2)
library(tidyverse)
plot(density(x)) # we use plot to draw density disgram
plot(x) #for scatter plot
plot(x,type = "o") # add parameters to change its form
boxplot(x,y) # we draw box plot
qqnorm(x) #see quantiles  

##session 2 bilibili

