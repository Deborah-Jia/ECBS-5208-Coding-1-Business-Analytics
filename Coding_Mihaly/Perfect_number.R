# exercise 1 --------------------------------------------------------------
# In number theory, a perfect number is a positive integer that is equal to the sum of its 
# positive divisors, excluding the number itself. 
# For instance, 6 has divisors 1, 2 and 3 (excluding itself), and 1 + 2 + 3 = 6, 
# so 6 is a perfect number.
# 
# https://en.wikipedia.org/wiki/Perfect_number

tnum <- 45
is_perfect <- function(tnum) {
  itdivides <- NULL
  for (i in 1:(tnum-1) {
    if(tnum%%i==0) {
      itdivides <- c(itdivides, i)
    }
  }
  if(sum(itdivides== tnum){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

perfect_numbers <- NULL
while (length(perfect_numbers) <4) {
  if(is_perfect(my_number)){
    perfect_numbers <- c(perfect_numbers, my_number)
    my_number <- my_number + 1
  }else {
    my_number <- my_number + 1
  }
}

# exercise 2 --------------------------------------------------------------
# Read the files from the bit.ly/rmentoring drive “files_to_Read” folder 
# into one file and remove the duplicated lines.
#install.packages('plyr')
library('plyr')
library(data.table)

my_files <- file_from <- list.files('/Users/wodediannao/Desktop/ECBS-5208-Coding-1-Business-Analytics/coding_practice/my_files', full.names = T)

df <- data.frame() 
for (t_path in my_files) {
  print(t_path)
  t <- read.csv(t_path) 
  print(head(t,1))
  df <- rbind.fill(df, t)
}



lapply(my_files, function(x){
  return(read.csv(x))
}


# exercise 3 --------------------------------------------------------------
# https://github.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/blob/master/Class_8/codes/life_exp_getdata.R
# 
# Download WDI data with a loop and save it into rds object.




