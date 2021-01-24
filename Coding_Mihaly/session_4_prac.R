##pre
rm(list=ls())
library(nycflights13)
library(tidyverse)
library(ggplot2)
df <- head(flights, 10000)

## carrier categorized by color
df %>% 
ggplot(aes(distance,arr_delay,color = carrier)) +
  geom_point() +
  labs(y= 'Arrival Delay', x= 'Distance')


# air_time categorized by size
df %>% 
  ggplot(aes(y = arr_delay, x = distance)) +
  geom_point(aes(size = air_time)) +
  labs(y= 'Arrival Delay', x= 'Distance')

# carrier categorized by shape #different shapes in one graph
df %>% 
  ggplot(aes(y=arr_delay, x=distance,shape = carrier)) +
  geom_point() +
  labs(y= 'Arrival Delay', x= 'Distance')

#carrier in facet # get you different graphs
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
  geom_smooth() #helps get a smooth line

# draw the violin plot of arr_time and carrier
df %>% 
  ggplot(aes(carrier, arr_delay, group= carrier)) +
  geom_violin(scale = "width")

# From now use all flights data
# Which airplane was in the air in the most time?
df1 <- flights

df1 %>% 
  group_by(tailnum) %>% 
  summarise(in_air = sum(air_time)) %>% # you'd better define a variable in summarize()
  arrange(-in_air)%>% 
  head(1)

## The best carriers based on arriving (on average the arr_delay is the least)
## To get a fine ggplot, we must use reorder() in aes(x()) part!
df1 %>% 
  group_by(carrier) %>% 
  summarise('avg_delay' = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(avg_delay) %>% 
  ggplot(aes(x=reorder(carrier, avg_delay), y= avg_delay )) +
  geom_col() + ##use geom_col !! don't quit! check in steps!
  labs(x= 'carrier', y= 'average delay')

## Average arrival delay to destinations by carrier ‘UA’ in August  
# when calculating sth, make judgment about N/A values!
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
  filter(carrier == 'UA' , month == 8) %>% # always put filter() in front of all others?
  group_by(dest) %>% 
  summarise('avg_delay' = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(avg_delay) %>% 
  head(10) %>% # first do the wrangling then the plotting
  ggplot(aes(x=reorder(dest, avg_delay), y= avg_delay )) + #with "-" you can reverse the order!
  geom_col() + 
  labs(x= 'carrier', y= 'average delay')

# a good way to debug: check every step(before each pipe and you'll see!)
df1 %>% 
  filter(carrier == 'UA' , month == 8, dest=='MCO') 

###draw the violin & box plot plot
my_url <- "~/Desktop/ECBS-5208-Coding-1-Business-Analytics/Class_4&5/data/clean_hotels/hotels-vienna-london.csv"
  
heu <- read.csv(my_url)

# useful! categorize distances into different labels
heu$dis_f <- cut( heu$distance , breaks=c(-1,2,4,100) , labels = c('close','medium','far') )

# a combination of violin plot and box plot
ggplot(heu, aes(x = city, y = price)) +
  geom_violin(color="blue", fill= "turquoise", alpha= .05)+
  geom_boxplot(width=.2, outlier.colour=NA, fill= "turquoise", alpha= .1) 

#show number of hotels in both cities with distance differentiated
heu %>% 
  ggplot(aes(city)) + #use aes() to group cities, x = city!
  geom_bar(aes(fill = dis_f), position = "dodge") + # "fill" here is for re-grouping!
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance') # use "fill" to change name of the label!

#change the legend position
heu %>% 
  ggplot(aes(city)) +
  geom_bar(aes(fill = dis_f), position = "dodge") + #dodge is used to to see side by side each different col in order to compare them
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance from city center') +
  theme(legend.position="top") # remember theme() function!


# plot bar chart with distance differentiated.
# without "dodge", it'll be stacked bars!
heu %>% 
  ggplot(aes(city)) + # bar automatically calculate numbers, so it needs x axis only
  geom_bar(aes(fill = dis_f)) +
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance from city center') +
  theme(legend.position="top")

# difference between geom_col and geom_bar
# for geom_col you can name y freely; it comes with group() and summarise()
# geom_bar will give you number of variables in each bar; no need for data wrangling
