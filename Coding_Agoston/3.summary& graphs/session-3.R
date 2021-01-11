library(tidyverse)

b_data <- read.csv("hotelbookingdata.csv")

glimpse(b_data)

# add one colomn called 'nnights'
b_data <- mutate( b_data , nnights = 1)

# separate colomn$accommodationtype into two: 'accommodation_type' and 'garbage'; 
## use @ as the sign 
b_data <- separate( b_data , accommodationtype , "@" , into = c("garbage", "accommodation_type"))

#remove the variable garbage
b_data <- select( b_data , -c( garbage )) # without "-", there is one "garbage" cloomn left!

# correct the guest-review-ratings into simple numneric variable
b_data <- separate( b_data , guestreviewsrating , "/" ,
                    into = c( "ratings" )) #there is only one seat, so it only accept the 1st half of original colomn
typeof(b_data$ratings) # still 'character', what to do?

#convert ratings to numeric values
b_data$ratings <- as.numeric( b_data$ratings) # by force :)
typeof(b_data$ratings)

# how to deal with distance measure
eg1 <- "coding is 123 fun!"
eg1

# find numeric values in a vector and replace it
gsub("12", "extra fun", eg1) # find '12' in eg1 and replace it to "extra fun"

#find any numeric value 
gsub("[0-9\\.]","extra fun", eg1) # 1, 2 and 3 are all replaced by "extra fun"!
gsub("[^0-9\\.]","", eg1) # drop all that's not numeric, which is used to drop characters


#mutate all the distance measures
b_data <- mutate(b_data ,
                  distance = as.numeric(gsub("[^0-9\\.]", "", center1distance)),
                  distance_alter = as.numeric(gsub("[^0-9\\.]", "", center2distance)))

typeof(b_data$distance) #you have to use $ to get the type of variables!

# rename vriables: right is the original!!! left is after-changed!
b_data <- rename(b_data ,
                 rating_count  = rating_reviewcount ,
                 ratingta_count = rating2_ta_reviewcount,
                 country = addresscountryname )

# replacing missing values
#look at key variable :stars
y <- c("abc", "def", "", "ghi")
na_if(y, "")

b_data <- rename(b_data, stars = starrating )
table(b_data$stars)

# replacing with Na
b_data <- mutate(b_data, stars = na_if(stars , 0)) # if there is 0, change to NA
table(b_data$stars)

#filter out observations which do not have id
b_data <- filter(b_data , is.na(hotel_id))
b_data

# filter out duplicates
sum(duplicated(b_data)) #duplicated() returns false or true


b_data <- filter(b_data, !duplicated(b_data)) # drop all duplicates

# remove duplicates to specific variables
subset.data <- subset(b_data, select = c(country, hotel_id))
b_data <- filter(b_data, !duplicated(
  subset(b_data, select = c(country, hotel_id, distance, 
                            stars, ratings, price, year,
                            weekend, holiday))))

# finally hotels Vienna
b_data <- rename(b_data, city = s_city)
hotel_vienna <- filter(b_data, city == "Vienna")
hotel_vienna
# filter  multiple conditions
hotel_vienna <- filter(hotel_vienna, 
                      year == 2017 & month == 0 & weekend == 0,
                       accommodationtype == "Hotel",
                       starrating >= 3 & starrating <= 4,
                       price < 1000)
# writing  csv??? puzzled
write_csv( hotels_vienna, paste0( data_in, "AMZN copy.csv")) 

#create descriptive table
Vienna_sun_stat <- summarise(hotels_vienna , 
                             mean = mean(price),
                             median = median(price),
                             std = sd(price),
                             min = min(price),
                             max = max(price))
