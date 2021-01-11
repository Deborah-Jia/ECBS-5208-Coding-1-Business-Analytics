#### random numbers and fuctions
rm(list = ls())
library(tidyverse)

# Random numbers
#   Random numbers are used in:
#     - get a random (sub)-sample
#     - bootstrapping
#     - other 'stochastic' optimization or  “随机”
#     - in some estimation (typically with ML)

# 1) random uniform distribution random sampling
n <- 10
x <- runif(n, min = 0, max = 10)
x

#2)set the seeds to make sure all PCs have the same result( you can run the whole chunk or seperately)
set.seed(123) #within() should be numbers, then it's totally OK
x <- runif(n, min = 0, max = 10)
x

rm(x, n)

#play around with no
n <- 10000 #the larger number, the closer the distribution
y <- rnorm(n, mean = 1, sd =2)
df <- tibble(var1 = y) #automatically generate step of 1
ggplot(df, aes(x= var1)) + 
  geom_histogram(aes(y = ..density..), fill = 'purple') +
  stat_function(fun = dnorm, args = list(mean = 1, sd = 2), color = 'red', size = 1.5)

# there are some other type of distrubutions
# rbinom, rexp, rinorm, etc.


# random sampling from a dataset or variable(without replacement)
sample_1 <- sample(y, 1000, replace = FALSE)

#with replacement- useful for bootstrap
sample_2 <- sample(y, 1000, replace = TRUE)


###functions
 mi_fuk <- function(x) {
   sum_x <- sum(x)
   sum_x / length(x)
 }
# use this function
 mi_fuk(y)

#you can save the output of the fuction
 mean_y <- mi_fuk(y)

#2) calculate mean and std
 my_fun2 <- function(x) {
   sum_x <- sum(x)
   sum_x / length(x)
   std_x <- sd(x) # it only gives you the last formula result unless required
 }

 #check the output
 what_y <- my_fun2(y)
 what_y
 
 # 3) control the output
 my_fun3 <- function(x) {
   sum_x <- sum(x)
   mean_x <- sum_x / length(x)
   std_x <- sd(x) # it only gives you the last formula result unless required
   return(mean_x)
 }
 my_fun3(y)
 
# 4) 
 my_fun4 <- function(x) {
   sum_x <- sum(x)
   mean_x <- sum_x / length(x)
   std_x <- sd(x) # it only gives you the last formula result unless required
   out <- list('sum' = sum_x, 'mean' = mean_x, "sd" = std_x) #  give output as you want
   return(out) #we can write the result into a table/csv/xlsx in a file!
 }
my_fun4(y)

#5 multi input
my_CI_fun <- function(x, CI = 0.95){
  # mean of x
  mean_x <- mean(x, na.rm = TRUE)
  # std
  sd_x = sd(x, na.rm = TRUE)
  # calculate the number of observations in x
  n_x <- sum(!is.na(x)) # add all "true"
  #calculate the theoretical SE for mean of x
  se_mean_x <- sd_x / sqrt(n_x) # you can also n()
  #calculate the CI
  if(CI ==0.95){
    CI_mean <- c(mean_x - 2*se_mean_x, mean_x + 2 *se_mean_x)
  }else if(CI ==0.99){
    CI_mean <- c(mean_x -2.6*se_mean_x, mean_x + 2.6 *se_mean_x)
  } else {
    stop('No such CI implemented, use 0.95 or 0.99')
  }
  out <- list('mean'= mean_x, "CI_mean" = CI_mean)
  return(out)
}

# get some CI values
my_CI_fun(y, CI = 0.99)

## smpling distribution
set.seed(100)
x <- runif( 10000, min = 0, max = 2 )

# write a function which creates the sampling distributions for:
#- for the mean
#-1st- t-statistics where H0 =1 ((mu-1)/SE(mu))
#-2nd- t-statistics where H0 =0 ((mu)/SE(mu))

# in the fuction you should add input as :
# x- vector
# rep_num <- how many time it should sample 
# sample_size <- how many obs to sample from x

# note: use sample(), use tibble as output
#here we go!

set.seed(100)
x <- runif( 10000, min = 0, max = 2 )
rep_num <- 0
sample_size <- 0

for (x <- runif( 10000, min = 0, max = 2 )) {
  
}

# plot these distributions
ggplot(df_x, aes(x = mean_stat)) +
  geom_density(color = 'red') +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = mean(x), color = "purple")

# plot these distributions: H0 is true
ggplot(df_x, aes(x = t_stat_1)) +
  geom_density(color = 'red') 

# plot these distributions: H0 is not true
ggplot(df_x, aes(x = t_stat_2)) +
  geom_density(color = 'red') 

# use function to create your own distribution; expandsssssss

  