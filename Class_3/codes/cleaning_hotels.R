### Class 3
## Data wrangling:
# cleaning the hotels dataset

rm(list=ls())
library(tidyverse)
library(dplyr)

# Import raw data
data_in <- "~/desktop/ECBS-5208-Coding-1-Business-Analytics/Class_3/data/"
b_data <- read_csv(paste0(data_in,"raw/hotelbookingdata.csv"))

# Have glimpse on data
glimpse(b_data)

# Create a new variable
b_data <- mutate( b_data , nnights = 1 )

# Clean accommodationtype column
b_data <- separate( b_data , accommodationtype , "@" , # 把该列分为两部分，并放到两个变量里,原变量死了
                    into = c("garbage","accommodation_type") ) #把原变量替换掉了
# Remove the variable garbage
b_data <- select( b_data , -garbage ) #select负号，意思是删掉

# Correct the guestreviewrating into simple numeric variable
b_data <- separate( b_data , guestreviewsrating , "/" , 
                    into = c( "ratings" ) ) #那么这里只要前半部分
typeof(b_data$ratings)  
# Convert ratings to numeric values
b_data$ratings <- as.numeric( b_data$ratings ) # “变性”
typeof(b_data$ratings)


# How to deal with distance measure
eg1 <- "Coding is 123 fun!"
# Find numeric values in a vector and replace it
gsub("123","extra fun",eg1) #三者顺序不可替换！
# Find any numeric value
gsub("[0-9]","extra" , eg1)
gsub("[^0-9\\.]","" , eg1)

# Mutate all the distance measures
b_data$center1distance
b_data$center2distance

b_data <- mutate( b_data , 
                  distance = as.numeric(gsub("[^0-9\\.]","", center1distance ) ),
                  distance_alter = as.numeric(gsub("[^0-9\\.]","", center2distance ) ) )
b_data$distance
  
## HW: use separate() instead.
b_data <- separate(b_data, center1distance, " ", into = c("distance")) 
b_data <- separate(b_data, center2distance, " ", into = c("distance_alter"))

## Rename variables
b_data <- rename( b_data , 
                  rating_count = rating_reviewcount,
                  ratingta = rating2_ta , 
                  ratingta_count = rating2_ta_reviewcount,
                  country = addresscountryname )

## Replacing missing values
# look at key variable: stars
b_data <- rename( b_data , stars = starrating ) #新命名的变量是放在前面的
table(b_data$stars)
# Replace with Na
b_data <- mutate(b_data , stars = na_if( stars , 0 ) ) #旧变量放在前面
table(b_data$stars)

#Filter out observations which do not have id
b_data <- filter( b_data, !is.na(hotel_id) )

# Filter out duplicates
sum(duplicated(b_data)) #逻辑变量，然后加总
# Remove duplicates
b_data <- filter( b_data , !duplicated( b_data ) )
# Remove duplicates to specific variables
sub_data <- subset( b_data , select = c(country,hotel_id)) #很好用的分层函数
b_data <- filter( b_data , !duplicated( #相当于这些列组成了prime key绝对不可以有重复
                  subset( b_data , select = c( country,hotel_id,distance,
                                               stars, ratings, price, year, month,
                                               weekend, holiday ) ) ) )

# Finally hotels Vienna
b_data <- rename( b_data , city = s_city )
hotel_vienna <- filter( b_data , city == "Vienna" )

# Filter multiple conditions
hotel_vienna <- filter( hotel_vienna , 
                        year == 2017 & month == 11 & weekend == 0,
                        accommodation_type == "Hotel" , 
                        stars >= 3 & stars <= 4,
                        price < 1000 )

# Writing out csv
write_csv( hotel_vienna , paste0( data_in,
                                  "clean/hotel_vienna.csv"))

# Create descriptive table
vienna_sum_stat <- summarise( hotel_vienna , #可以通过source方程实现
                              mean = mean( price ),
                              median = median( price ),
                              std = sd( price ),
                              min = min( price ),
                              max = max( price ) )
  
  
vienna_sum_stat

