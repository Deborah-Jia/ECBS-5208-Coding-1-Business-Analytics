##########################################
## Descriptive stat of Hotels in Vienna
##
## 2020-10-05

rm(list=ls()) # to remove all obs in the list # like "truncate" in sql!
library(tidyverse)

## Import data
data_in <- "~/Desktop/CodingInR🆗/Coding1/" #always remember, there is a slash in the last folder!
hotels_vienna <- read_csv(paste0(data_in,"hotels_vienna.csv"))
# we created two df, in fact

# In parenthesis
getwd()
setwd()

# glimpse on data
glimpse(hotels_vienna)

# Have a summary
summary(hotels_vienna)

# Select favorite variable
hotels_vienna$price

# The average price of hotels
mean(hotels_vienna$price)

# Number of observations in hotel price vector
length(hotels_vienna$price)

###
# Visualization
ggplot( data = hotels_vienna , aes( x = price ) ) + 
  geom_histogram()

ggplot(hotels_vienna, aes(rating)) +
  geom_histogram(biinwidth = 50)

ggplot( data = hotels_vienna , aes( x = price ) ) +
  geom_histogram( fill = "pink" )+
  labs(x="Hotel prices ($)",y="Absolute Frequency")


ggplot( data = hotels_vienna , aes( x = price) ) +
  geom_histogram( fill = "navyblue" , binwidth = 80 ) + #use 'fill' to change color
  labs(x="Hotel prices ($)",y="Absolute Frequency")

# Relative frequency graph: add 'y=..density..' in geom(aes()) !!!
ggplot( data = hotels_vienna , aes( x = price) ) +
  geom_histogram( aes( y = ..density..) , fill = "navyblue" , binwidth = 20 ) +
  labs(x="Hotel prices ($)",y="Relative Frequency")

# Kernel density estimaton: bw is binwidth, alpha is transparency
ggplot( data = hotels_vienna , aes( x = price ) ) +
  geom_density( aes( y = ..density.. ), color = "grey" , fill = "blue", bw = 10 , alpha = 0.1 ) +
  labs(x="Hotel prices ($)",y="Relative Frequency")


# Kernel density and histogram: combined together!!!
ggplot( data = hotels_vienna , aes( x = price ) ) +
  geom_histogram( aes( y = ..density.. ), fill = "black", binwidth = 20 ) +
  geom_density( aes( y = ..density.. ), color = "blue" , fill = "grey", bw = 15 , alpha = 0.5 ) +
  labs(x="Hotel prices ($)",y="Relative Frequency")

