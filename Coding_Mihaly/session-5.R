# Exercise 1 Write a for loop that iterates over the numbers 1 to 7 
# and prints the cube of each number using print().

for (i in 1:7) {
  print(i^3)
}

# Exercise 2 Write a for loop that iterates over the column names of the inbuilt iris data set 
# and print each together with the number of characters in the column name in parenthesis. 
# Example output: Sepal.Length (12). Use the following functions print(), paste0() and nchar().

iris
for (i in c(colnames(iris))) { # it automatically assigns index to each column names
  print(paste0(i," (",nchar(i),")")) # nchar() can calculate number of characters
}

# Exercise 3 Write a while loop that prints out standard random normal numbers 
# (use rnorm()) but stops (breaks) if you get a number bigger than 1.

while (x <- rnorm(1) ) 
{ x <- rnorm(1)
  if (x > 1) {
    print( paste0(x, " is bigger than 1 the loop stopped")
    )
    break 
  } else {
      print(x)
    }
}

# Exercise 4 Using next adapt the loop from last exercise so that doesn’t print negative numbers.

while (x <- rnorm(1) ) 
{ x <- rnorm(1)
  if (x < 0) {
    next
  }
if (x > 1)
  {
  print(paste0(x, " is bigger than 1 the loop stopped"))
  break 
  } 
  else {
  print(x)}
}

# Exercise 5 Using a for loop simulate the flip a coin twenty times, keeping track of the individual outcomes (1 = heads, 0 = tails) in a vector that you preallocte.

rsl <- c()
for (i in 1:20){
  rsl <- c(rsl,sample(0:1,1) )
}
print(rsl)

# Exercise 6 Use a while loop to investigate the number of terms required before the product 1⋅2⋅3⋅4⋅… reaches above 10 million.

pdt <- 1
i <- 1

while (pdt < 10000000){
    pdt = pdt * i
    i = i+1
if (pdt >= 10000000){
print(paste0("The solution is:",i-1," the product is:", pdt))
  break
} else {
  print(i)
}
}
## instructor's choice
all_product <- 1
t_number <- 1
while (TRUE) {  ## use while true to form an infinite cycle
  print(t_number)
  
  all_product <- all_product*t_number
  if (all_product>10000000) {
    print(paste0('The solution is: ', t_number, ' the product is: ',all_product ))
    break()
  }
  t_number <-t_number +1
  
  
}

# Exercise 7 Drunk people theory.
# You need to go home which is 1500m far from you right now.
# In one step you can go 1m closer or 0.9m back (you are drunk)
# How many steps you need to reach home?
# Plot your result.
# Simulate it 10 times
library(tidyverse)
library(ggplot2)

prg <- c() #progress
stp <- c() #step
while (sum(prg) < 1500) {
  
  step_lgt <- sample(x = c(1, -0.9), size = 1, replace = TRUE) #step length
  prg =c(prg,step_lgt)
  stp = c(stp, length(prg)) #calculating two vectors affected whole speed
if (sum(prg)  >= 1500){
  
  home_df <- data.frame(
    "steps" = stp,
    "progress" =cumsum(prg)
  ) 
  
  p <- ggplot(data = home_df, aes(x= steps, y= progress)) 
  p <- p + geom_line() 
  print(p) ##use print to print ggplot
  
  print(paste0("I need ", tail(stp, 1), " steps to get home.")) 
  
  break
}
  
}

## instructor's choice
for (i in 1:10) {
  
  
  
  all_steps <- NULL
  while (TRUE) {  #double loops
    t_step <- sample(c(1, -0.9), 1)
    all_steps <- c(all_steps, t_step) # all varibles are based on one variavble 't_step'
    t_progress<- tail(cumsum(all_steps), 1)
    if (t_progress >=1500) {
      print(paste0('I need ', length(all_steps), ' steps to get home.'))
      break()
    }  
  }
  
  df <- data.frame('steps'= 1:length(all_steps), 'progress'= cumsum(all_steps) )
  
  plot(ggplot(df, aes(x=steps, y=progress))+ #you must use plot() within the loop to plot!
         geom_line()+
         theme_bw()
  )
}



# Exercise 8
# Implement a multiplication game. A while loop that gives the user two random numbers 
# from 2 to 12 and asks the user to multiply them. Only exit the loop after five correct answers. 
# Try using as.integer(readline()) instead of scan() this time.

#instructor's choice




# Exercise 9
# Make a lotery simulation. Ask 5 number from the user and simulate the lottery draw from 90 
# ball, and write the result back to the user.


