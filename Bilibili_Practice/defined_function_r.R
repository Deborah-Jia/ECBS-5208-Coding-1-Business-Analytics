# Exercise 1 Create a function to print square of number

get_square <- function(x) {
  x <- x*x
  return(x)
}
get_square(5)


# Exercise 2 
# Create a function to print a number raise to another with the one argument a default argument
# Don't understand the queston.
# it means to get the 1st number to the 2nd number power. but the 2nd should have a default value

get_power <- function(x, y = 2){
  result <- x **y
  return(result)
}
get_power(4,5)


# Exercise 3
# Create a function to print class of an argument

get_class <- function(argu) {
  print(class(argu))
}
get_class("a cat")


# Exercise 4
# Create a function to accept two matrix arguments and sum them up.

matrix_sum <- function(M1, M2) {
  print("matrix 1 ")
  print(M1)
  print('matrix 2')
  print(M2)
  print("Below is their sum")
  print(M1 + M2)
}

matrix_sum(M1 = matrix(1:16, ncol = 4, nrow=4),
           M2=matrix(2:7, ncol = 4, nrow=4))

# Are you fu*king kidding me?


# Exercise 5
# Create a user defined function to accept a name from the user

userread <- function() ##readline is a good fuction!
{
  str <- readline(prompt="Enter the Name: ")
}

print(userread())


# Exercise 6
# Create a user defined function to accept values from the user 
# using scan and return the values



# Exercise 7
# Create a user defined function to create a matrix and return the same.



# Exercise 8
# Create a function to take two arguments, 
# one student marks and other student names and plot a graph based on the same.



# Exercise 9
# Create a function to accept an employee data frame(Name,Gender,Age,Designation & SSN) 
# and print the First & Fifth employee as well as the Names & the Designation of 
# all the employees in the function


 
# Exercise 10
# Create a function to create an employee data frame(Name,Gender,Age,Designation & SSN) 
# and return the Name,Age & Designation of all employees.

https://www.r-exercises.com/2017/04/20/user-defined-functions-in-r-part-1-2/?__cf_chl_captcha_tk__=d2c68d66b9ea7316f6491d8391539e4b4177f5dd-1604771022-0-AaAJ7LmEMF0W8LfwOHG2ZYRYf_dVPe3JxLx5xvbPB-4c9SKy4BSjUWey-RAb1gpFwHnnX9ogwD2e8dbww64rZwakLqZwlpM4c0AFdER0I64CaJFcxigC3NO3etw0fpkoQYUfeCfGXko8bESOzfdK8T1ISbZnZT8SSE1yyVdkTwG5r0q1An9iJCZ1PmXhst_eddfV8ZACr1d9Z6O2o8Y72XITbh1JxlY6lrr4CWiYy4S8wG6yLsC4o0ofo3yRzpfM0k_QnHHNC9_SfMuPLvh1rjVafgpR8LvFzXYhgmEqtaxyVuv7Wga8DxHRPywPaQDqPANHQQmBS0Wa89CR8P7bT7ieAc424M1NH7RsWLa3RET2oIJNh5pvKqcYKb1jcIfPtYIZLt3WkR2lndqg2VCCMUAe-eMvxiGaJnbQW8kuH03nrd_w_Fl_aYElzHLDSYYjapGiSZCChrwc2lpyfLtlc-8cHrxumQUtTMC3I1ErqDxIjSj3lLbH8TJ05LDF_w61jgGU4XGsTb5qhmRgvsyqrSu5ka8X-rAD7hg_6XaY8-17lG3gIn6sNIl2FPg8D-bYx_Ib0wa1gaiB1tI4tLl9zP4



