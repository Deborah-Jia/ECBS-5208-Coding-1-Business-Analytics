# get the scatter plot
mtcars
library(ggplot2)


ggplot(mtcars, aes(x=mpg, y = hp))+
  geom_point()+theme_get()+
  labs(x='millspergallon',y="horsepower")


library(nycflights13)
library(tidyverse)  
library(ggplot2)
df <- flights

## 1\Which airplane flought the most times?

nycflights13::airlines #to see one colomn of the table

df <- flights

df  %>% 
  group_by(tailnum) %>% 
  summarise('number_of_flight'=n()) %>%  #name a variable
  arrange(-number_of_flight) %>% #sort in a descending order
  filter(is.na(tailnum)==F) %>% 
  head(10) # show the 1st row

##2\Create the departure delay histogram
df %>% 
ggplot(aes(x= dep_delay))+
  geom_bar()+
  theme_bw()+
  labs(x='departure delay', y='number of observation', title = 'departure delay histogram')

##3\Number of flights to destination
df %>% 
  group_by(dest) %>% 
  summarise('number_of_destination'=n()) %>% 
  ggplot(aes(x=dest, y= number_of_destination))+ #you have to name a variable first
  geom_col()+
  theme_bw()+
  labs(x='Destnation',y='Number of Flights', title = 'Number of flights to destination')

##4 Top 10 most popular destination
df %>% 
  group_by(dest) %>% 
  summarise('number_of_destination'=n()) %>% 
  arrange(-number_of_destination) %>% 
  filter(is.na(dest)==F) %>% 
  head(10) %>% 

ggplot(aes(x = reorder(dest, -number_of_destination), y = number_of_destination))+
         geom_col()+
         theme_classic()+
         labs(x='destination', y='number of flights', title = 'Top 10 most popular destination')
##5 The most exact destinations (dep_delay_is the least on average top 10)
df %>% 
  group_by(dest) %>%  # average = mean!!!
  summarise('mean_delay' = mean(dep_delay, na.rm = TRUE)) %>% #firsr remove null, then calculate!
  arrange(mean_delay) %>% 
  head(10) %>% 
  ggplot(aes(x = dest, y= mean_delay)) +
  geom_point(color = "blue") +
  labs(x = "Destination",y= "average late", title = "the most exact destinations")

##6 iris data plot
iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_point(color= "black") +
  facet_wrap(~Species) +
  labs(x="Length", y="Width") +
  theme_grey()
  
