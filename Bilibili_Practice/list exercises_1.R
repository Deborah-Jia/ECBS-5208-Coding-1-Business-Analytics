# b b c
# Exercise 4
# If Newlist <- list(a=1:10, b="Good morning", c="Hi"), 
# write an R statement that will add 1 to each element of the first vector in Newlist.

# Newlist <- list(a=1:10, b="Good morning", c="Hi")
# 
# Newlist$a <- Newlist$a + 1
# Newlist 

# Exercise 5
# If b <- list(a=1:10, c="Hello", d="AA"), 
# write an R expression that will give all elements, except the second, of the first vector of b.
# it will give 1:10 (exclude 2), i.e. the first element in b
b <- list(a=1:10, c="Hello", d="AA")
b$a[-2]

# Exercise 6
# Let x <- list(a=5:10, c="Hello", d="AA"), 
# write an R statement to add a new item z = "NewItem" to the list x.

# x <- list(a=5:10, c="Hello", d="AA")
# x$z <- "NewItem"

# Exercise 7
# Consider y <- list("a", "b", "c"), 
# write an R statement that will assign new names "one", "two" and "three" to the elements of y.

y <- list("a", "b", "c")
names(y) <- c("one", "two" , "three") #get name or change name
y

# Exercise 8
# If x <- list(y=1:10, t="Hello", f="TT", r=5:20), 
# write an R statement that will give the length of vector r of x.

# x <- list(y=1:10, t="Hello", f="TT", r=5:20)
# length(x$r)

# Exercise 9
# Let string <- "Grand Opening", 
# write an R statement to split this string into two and return the following output:
#   
#   [[1]]
# [1] "Grand"
# 
# [[2]]
# [1] "Opening"

string <- "Grand Opening"
a <- strsplit(string, " ")
list(a[[1]][[1]], a[[1]][[2]])

d# Exercise 10
# Let:
#   y <- list("a", "b", "c") and
# q <- list("A", "B", "C", "a", "b", "c").
# Write an R statement that will return all elements of q that are not in y, 
# with the following result:
#   [[1]]
# [1] "A"
# 
# [[2]]
# [1] "B"
# 
# [[3]]
# [1] "C"

y <- list("a", "b", "c") 
q <- list("A", "B", "C", "a", "b", "c")
s <- setdiff(q, y)

