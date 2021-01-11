##pre
library(nycflights13)
library(tidyverse)
library(ggplot2)
df <- head(flights, 10000)

## carier categorized by color
df %>% 
ggplot(aes(distance,arr_delay,color = carrier)) +
  geom_point() +
  labs(y= 'Arrival Delay', x= 'Distance')


# air_time categorized by shape
df %>% 
  ggplot(aes(y = arr_delay, x = distance), size = air_time) +
  geom_point(aes(size = air_time)) +
  labs(y= 'Arrival Delay', x= 'Distance')

# arier categorized by shape
df %>% 
  ggplot(aes(y=arr_delay, x=distance,shape = carrier)) +
  geom_point() +
  labs(y= 'Arrival Delay', x= 'Distance')

#carrier in facet 
df %>% 
  ggplot(aes(y=arr_delay, x=distance)) +
  geom_point() +
  labs(y= 'Arrival Delay', x= 'Distance') +
  facet_wrap(~ carrier)

#add a blue line???
df %>% 
  ggplot(aes(x = distance, y = arr_delay)) +
  geom_point() +
  labs(x= 'Distance', y= 'Arrival Delay') +
  geom_smooth()

# draw the violin plot of arr_time and cariier
df %>% 
  ggplot(aes(carrier, arr_delay, group= carrier)) +
  geom_violin(scale = "width")

# From now use all flights data
# Which airplane was in the air in the most time?
df1 <- flights

df1 %>% 
  group_by(tailnum) %>% 
  summarise(in_air = sum(air_time)) %>% 
  arrange(-in_air)%>% 
  head(1)

## The best carriers based on arriving (on average the arr_delay is the least)
df1 %>% 
  group_by(carrier) %>% 
  summarise('avg_delay' = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(avg_delay) %>% 
  ggplot(aes(x=reorder(carrier, avg_delay), y= avg_delay )) +
  geom_col() + ##use geom_col !! don't quit! check in steps!
  labs(x= 'carrier', y= 'average delay')

## Average arrival delay to destinations by carrier ‘UA’ in August  
###########
df1 %>% 
  group_by(dest) %>% 
  summarise('avg_delay' = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(carrier == 'UA' , month == 8) %>% ##error because you only choose dest and arr_delay, 2 columns!
  arrange(avg_delay) %>% 
  ggplot(aes(x=reorder(dest, avg_delay), y= avg_delay )) +
  geom_col() + 
  labs(x= 'carrier', y= 'average delay')
########### 

df1 %>% 
  filter(carrier == 'UA' , month == 8) %>% 
  group_by(dest) %>% 
  summarise('avg_delay' = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(avg_delay) %>% 
  head(10) %>% 
  ggplot(aes(x=reorder(dest, avg_delay), y= avg_delay )) +
  geom_col() + 
  labs(x= 'carrier', y= 'average delay')

df1 %>% 
  filter(carrier == 'UA' , month == 8, dest=='MCO') 

###draw the violin & box plot plot
my_url <- 'https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/Class_4n5/data/clean_hotels/hotels-vienna-london.csv'
heu <- read.csv(my_url)
heu$dis_f <- cut( heu$distance , breaks=c(-1,2,4,100) , labels = c('close','medium','far') )


ggplot(data = heu, aes(x = city, y = price)) +
  geom_violin(color="blue", fill= "turquoise", alpha= .05)+
  geom_boxplot(width=.2, outlier.colour=NA, fill= "turquoise", alpha= .1) 

#show namuber of hotels in both cities with distance differentiated
heu %>% 
  ggplot(aes(city)) +
  geom_bar(aes(fill = dis_f), position = "dodge") +
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance') 

#change the legend position
heu %>% 
  ggplot(aes(city)) +
  geom_bar(aes(fill = dis_f), position = "dodge") + #dodge is used to to see side by side each different col in order to compare them
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance from city center') +
  theme(legend.position="top")


# plot bar chart with distance differentiated.
heu %>% 
  ggplot(aes(city)) + # bar automatically caculate numbers, so it needs x axis only
  geom_bar(aes(fill = dis_f)) +
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance from city center') +
  theme(legend.position="top")
