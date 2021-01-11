#bar chart
#pie chart
install.packages("plotrix")
#fan plot
#summary

head(mtcars, n= 2)

ncol(mtcars)
nrow(mtcars)

summay(mtcars)
View(mtcars)

install.packages("nortest")
%

x <- c("green","red","blue")
summary(x)
install.packages(dplyr)
install.packages("dplyr")
library("dplyr")
View(mtcars)

mtcars %>% 
  group_by(cyl) %>% 
  summarise(n())

filter(mtcars, cyl==4) #use filter fuction to choose observations whose cyl is 4. use "==" here!

mtcars %>% 
  filter(am==0) %>% 
  select(mpg,am, cyl, disp)
#choose iobservations in mtcars where hp>= 150 and show cyl and mpg as well. name the table as mt_hp150
mt_hp150 <- mtcars %>% 
  filter(hp>=150) %>% 
  select(hp, cyl,mpg) 
  
mt_hp150

fl <- read.csv("~/Desktop/CodingInR🆗/flight.csv")

mtcars %>% 
  mutate(kilo_watt= hp*0,7457) %>%  ## add a new row
  head(10) %>% 
  filter(!is.na(am)) # [is.na] to chose the empty value, !is.na is for non-empty value
#view mean arrival time by month
fl %>% 
  group_by(dest) %>% 
  summarize(mean_arr = mean(arr_time, na.rm = TRUE))
#group by multi-colomns 
fl %>% 
  group_by(dest,origin) %>% 
  summarize(mean_arr = mean(arr_time, na.rm= TRUE))
#choose observations with LGA as origin, group by dest and tailnum, and calculate each group's mean distance
fll <- fl %>% 
  group_by(dest,tailnum ) %>% 
  filter(origin=="LGA") %>% 
  summarise(dis_mean=mean(distance, rm.na= TRUE))
fll

##