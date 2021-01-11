## set memory
rm(list=ls())

#ask the pc to do A if B is true and do somrhing else if B is not true

# case 1)
x <- 5
if (x > 6){
  print('positive number')
} 



#case 2)
if (x > 0){
  print('positive number')
} else {
  print('negative number')
}


#case 3) multiple statements
y <- 0
if (y>0){
  print('positive')
} else if(y <0){
  print('negative')
} else {
  print('zero')
}


# case 4) multiple condition
if(c(TRUE, FALSE)) { #within () there should be only one judgement!
  z< 5
  }
#how to fix: use any/all, 
rm(z)
if (all(c(TRUE, FALSE))) { #within () there should be only one judgement!
  z < 5
}

if (y>=0 && x>=0 ){  #use && here to decide on only one judgement
  z <- y+x
} else if (y>=0 && x<0){
  z <- y-x
} else if (y<0 && x>=0) {
  z <- x-y # -y +x
} else if (y<0 && x<0){
  z <- - y - x
}

x <-  1
y <-  3
#notes:
#1) always create a variable: 
#   a) create beforehand and rename it
#   b)make sure your conitional covers all the possibilities

###################loops
#   great for repetative work in a compact format
# two types:
#     1) you know how many times you want to make something
#   2) you do not know, but you r doing this untill you get the result your want( you know what you want!)
##    discrete approach

# case 1) purest form
for ( i in 1:5) { # you name 'i' and vector(not always index number)
  print(i)
}

# case 2) 
for ( i in c(2,5, -1000, 19940211) ) { # you name 'i' an vector(not always index number)
  print(i) # can be seq() or c()
}

#  case 4) list: it's OK
for (i in list(2 ,4, "birthday", TRUE)) {
  print(i) 
}

# useful tool : seq_along()
my_v <- c(10,24,52,2)
seq_along(my_v ) #generate sequence/by order
for ( i in seq_along(my_v )) {
  print(my_v)
}

# create a cumulative sum loop

v <- seq(1, 10)
u <- c(1:10)

cs_v <- 0

for ( i in v ) {
  cs_v <- cs_v + v[i]
}

 cs_v
 cumsum(v)
 
# double() is to create double-precision vector of the specified length
f <- double(length = 3) # first it is 0 0 0
f <- c(3.5, 4.1,9.22)
f[2]

f <- c(f,2)

# create a vector with 'for loop'
cs_v2 <- double( length = length(v) ) #reconrd the length of v and set it as a number
for (i in seq_along( v )) { #here cs_v2 is 0,0,0,0,0,0,0,0,0,0
  if( i > 1){
  cs_v2[i] <- cs_v2[ i-1 ] + v[ i ] #[i-1]th number in cs_v2 + ith number in v
  } else {
    cs_v2[ 1 ] <- v[ i ]
  }
}
cs_v2 #it shows the accumulative value in it's 10 elements
# check if cs_v1 and cs_v2 are in same value
cs_v2 == cumsum(v)
cs_v2 && cumsum(v)

if (all(cs_v2 == cumsum(v))) {
  print('good job')
} else {
  print('please refine me!')
}

## measuring CPU time
# install some developer package
# install.packages("devtools")
library(devtools) # useful for installing from github
devtools::install_github("jabiru/tictoc")
library(tictoc) #tic to start cpu time and toc to end this


# sloppy way:
tic("Sloppy way")
csv_3 <- c()
iter_num <- 100000
for (i in 1:iter_num) { #we can add "by =5" to change steps
  csv_3 <- c(csv_3, i) # add length to cs_v3, cs_v3 grows: [0], [1], [1,2] ...
} #it grows; a cubersome way
toc()


#proper way: first assign a long one then assign each one into it
tic("Proper way")
csv_4 <- double( length = iter_num) #contains numeric values
for (i in 1: iter_num) { #last one adds position and number at the same time,
  csv_4[ i ] <- i #this one first assigns seats, then in loop adds number
}
toc()

# while loop: judge first if your statement is true; then run it
x <- 0
while (x < 10) {
  x <- x + 1
}
x

# a different way: use for with condition
maxiter <- 10000
x <- 0
flag <- FALSE
  
for ( i in maxiter) {
  if(x < 10) {
    x <- x + 1
  } else {
    flag <- TRUE 
    break #break the loop
  }
}
if (!flag ) {
  warning('for loop did not converged, readched maximum iteration!')
} #here we learnt 'warning' fuction!

