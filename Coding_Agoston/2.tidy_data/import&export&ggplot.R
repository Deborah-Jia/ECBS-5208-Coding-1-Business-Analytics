# Descriptive stat of Hotels in Vienna
## 2020-10-05

rm(list=ls())

##IMPORT DATA

data_in <- "Desktop/" #folder the data are in
hotels_vienna <- read_csv(paste0(data_in, "hotels_vienna.csv")) #the data file's name

#in parenthesis
getwd()
setwd()

#glimpse on data
glimpse(hotels_vienna)

summary(hotels_vienna)

hotels_vienna$price
 
mean(hotels_vienna$price)

#number of observation in hotel price vector
length(hotels_vienna$price) #how many observations we have in price variable

#visualization
ggplot(data = hotels_vienna, aes(x = price )) + geom_histogram()

ggplot(data = hotels_vienna, aes( x = price)) +
  geom_histogram(fill = "pink")+
  labs(x="hotel prices($)", y="absolute frenquency")

ggplot(data = hotels_vienna, aes(x= price))+
  geom_histogram(fill = "turquoise", binwidth = 1)+
  labs(x= "hotel prices($)", y= "absolute frequency")

#relative frequency graph
ggplot(data = hotels_vienna, aes(x= price))+
  geom_histogram( aes(y = ..density..),fill = "turquoise", binwidth = 1)+
  labs(x= "hotel prices($)", y= "relative frequency")

#Kernel density estimator
ggplot( data = hotels_vienna, aes(x=price))+
  geom_density(aes(y = ..density..), fill = "turquoise", bw = 15, alpha = 0.5) +
  labs(x= "hotel prices($)", y= "relative frequency")


#Kernel density and histogram
ggplot( data = hotels_vienna, aes(x=price))+
  geom_histogram( aes(y = ..density..),fill = "orange", binwidth = 20)+
  geom_density(aes(y = ..density..), fill = "turquoise", bw = 15, alpha = 0.5) +
  labs(x= "hotel prices($)", y= "relative frequency")



