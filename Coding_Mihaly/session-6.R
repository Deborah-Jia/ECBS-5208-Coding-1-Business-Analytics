# Exercise 1 Write a function that calculates number a to the power of b, 
# but let b have a default value of 2.
b <- 2
# --- my solution
myfun_1 <- function(a) {
  return(a**b)
}
myfun_1(2) # when your code doesn't seem to work, try to open r!

# instrutor's solution
powerof <- function(a, b=2) { # b is given a value in the bracket!
  return(a^b)
}

powerof(2,4)



# Exercise 2 Write a function that changes the value of pi in the R global environment to 
# whatever you specify as the argument. Note: it is not recommended to re-define the value of “pi” 
# in a real-life R program.
## use <<- to set global environment value that cross scripts!!!

chpi <- function(aa) {
  unlockBinding("pi", globalenv())
  pi <<- aa
  return(pi)
}
chpi(3.141592653589793238)
pi 
## from this exercise, I got to know:
# there is local and global environment, constant values
# how to lock and unlock values, espiecially by locking values globally so it can be used anywhere without worrying about tempering
# developed the understanding of fuction structure

## finally, lock pi's value for future exploitation
pi <- 3.141592653589793238
lockBinding("pi", globalenv())



# Exercise 3 Create a function which return a data frame like the example. 
# The input of the function will be the number of rows of the data frame
library('lubridate')


# my solution: correct but rather clumsy
random_df <- function(x) {
  df <- data.frame(
    weight = sample(45:100, 100, replace=TRUE), #should have name row numbers as n the variable
    height = sample(145:200, 100, replace=TRUE),
    class =  sample(LETTERS, 100, replace = TRUE),
    birth_date = sample(seq(as.Date('1910/01/01'), as.Date('2000/01/01'), by="day"), 100),
    age_days = today() - birth_date,#birth_date as a new column should be created later
    age_year = year(now()) - year(birth_date)
    ) 
  stringsAsFactors = FALSE
  head(df, x)
}
random_df(50)

## instructor's suggestion:
random_df <- function(n) {
  df <- data.frame(
    'weight' = sample(45:100, n, replace=TRUE), #should have name row numbers as n the variable
    'height' = sample(145:200, n, replace=TRUE),
    'class' =  sample(LETTERS, n, replace = TRUE),
    'birth_date' = sample(seq(as.Date('1910/01/01'), as.Date('2000/01/01'), by= "day"), n, replace = TRUE))
    df$age_days <- today() - df$birth_date #birth_date as a new column should be created later
    df$age_year <- year(now()) - year(df$birth_date) #also use df$birthdate as the cloumn, otherwise it won't recognize
    
  return(df)
}

random_df(50)


# Exercise 4 Create a function which print a random walk 
# just like in previous class and return with the dataframe.

library(ggplot2)
library(tidyverse)


go_home <- function(forward_length, backward_length, distance) {
  all_steps <- NULL #remember that null should be in system setting!
  
  while (TRUE) {  
      t_step <- sample(c(forward_length, (backward_length*-1) ), 1 )
      all_steps <- c(all_steps, t_step) 
      
      if (tail(cumsum(all_steps), 1) >= distance) {
        
        break
      }  
    }
   print(paste0('I need ', length(all_steps), ' steps to get home.'))
    df <- data.frame('steps'= 1:length(all_steps), 'progress'= cumsum(all_steps) )
    
    plot(ggplot(df, aes(steps, progress)) +
           geom_line() +
             theme_classic())
}

go_home(forward_length = 1, backward_length = 0.9, distance = 3000)
            