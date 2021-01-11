#chapter 16 review
library(tidyverse)
library(lubridate)
library(nycflights13)

#for explaination of nycflights13, see https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf
#get the current date and time
today()
now()

#parse your date
ymd("2018-12-20")
ymd(20171229)

#create date-time
ymd_hms("2018-12-29 20:07:25")

#add a timezone
ymd("2020-10-24", tz = "UTC")  # - and " exist together. or you don't use them all

#form a new departure colomn containing year, month, date, hour, minute
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

#use-defined fuction
squ_fu <- function(a, b){
  result <- (a^2 + b^2)^0.5
  print(result)
}
squ_fu(3,4)

flights

#return origin, dest, dep_delay, arr_delay, dep_time and sched_dep_time
## wrong answer!!!!
make_dt100 <- function(year, month, day, time){
  make_datetime(year, month, day, time%/%100, t%%100) # typo in the last one
}

fl_dt <- flights %>% 
  mutate(dep_delay = make_dt100(year, month, day, dep_delay)) %>% 
  mutate(arr_delay = make_dt100(year , month, day, arr_delay)) %>% 
  mutate(dep_time = make_dt100(year, month, day, dep_time )) %>% 
  mutate(sched_dep_time = make_dt100(year, month, day, sched_dep_time)) %>% 
select(orgin, dest, ends_with("delay"), ends_with(time) %>% 

         fl_dt #check previous steps even if errror happens in this step 
       
# right answer
make_dt100 <- function(year, month, day, time){
  make_datetime(year, month, day, time%/%100, time%%100)
}

fl_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_dt100(year, month, day, dep_time),
    arr_time = make_dt100(year , month, day, arr_time),
    sched_arr_time = make_dt100(year, month, day, sched_arr_time),
    sched_dep_time = make_dt100(year, month, day, sched_dep_time)
    ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time") )
 fl_dt
 
#visualize dep_time
 fl_dt %>% 
 ggplot(aes(dep_time)) + # it initialized an object "I will draw a graph now"
   geom_freqpoly(binwidth=86400) # one day has 86400 seconds
 
#dep_time within single day
 fl_dt$dep_date <- as.Date(fl_dt$dep_time)
 fl_dt %>% 
   filter(dep_date == as.Date("2013-01-03")) %>% 
   ggplot(aes(dep_time)) +
   geom_freqpoly(binwidth= 86400)

 ##  "==" doesn't work because left side(date+time) doesn't equal right side(only date!)
 ## it's a good way to check the line before drawing the map!
 
typeof(fl_dt$dep_time)
typeof(ymd(20130110))

## 
 
# as.datetime and as.date
 as_datetime(today())

 as_date(now())  
 
 as_datetime(60*60*24)
as_date(30*12*5+2) # all started from 1970-1-1

#exercise 1 What happens if you parse a string that contains invalid dates?
ymd(c("2010-10-10", "bananas")) #string can't prase

#exercise 2 What does the tzone argument to today() do? Why is it important?
today(tz= "America/New_York")

#exercise 3 Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
mdy(d1)

d2 <- "2015-Mar-07"
ymd(d2)
dmy(d3)
d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4[c(1,2)])
d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)
#########################################################################
#####get components of time
yday(now())

###### are more flights depart on weekdays or on weekends?
fl_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE, abbr = FALSE)) %>% 
  ggplot(aes(x=wday)) +
  geom_bar()

###create a line graph showing relations between dep_time and arr_delay within one hour
###wrong!!!
minute <- fl_dt %>%  # 'minute' was created inside mutate, not from the beginning!
  mutate(minute = minute(sched_dep_time),  #should be dep_time, not scheduled!
         na.rm = TRUE) %>%  # no need to write na.rm, it's within mean!
  group_by(minute) %>% 
  summarise(avg_arr = mean(arr_delay), na.rm= TRUE) # there should be a pipe!
  ggplot(fl_dt, aes(x = minute, y= avg_arr) ) + # x and y can be removed. no need to write fldt
  geom_line()
  
## the right answer:
fl_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, 
                     na.rm= TRUE),
                     n=n()) %>%  ## there is a pipe, as here we didn't create new df, the order will will pass through ggplot to the end
ggplot(aes(x= minute, y= avg_delay)) +
    geom_line()

## what about the relation between sche_dep_time and arr_delay?
sched_dep <- fl_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), n=n()) # here is no pipe, end of the order: we create a new df here
  ggplot(sched_dep, aes(x=minute, y=avg_delay)) + ##to make ggplot work, you must add the data source in the 1st place
  geom_line()
# no need to copy the standard answer
#dont use '-' to name a df!!!
# sche_dep(a df) was eatablished for next step
  
# another anser:
fl_dt %>% 
    mutate(minute = minute(sched_dep_time)) %>% 
    group_by(minute) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = TRUE), n=n()) %>% #here is a pipe, for ggplot no source need: it is fl_dt
  ggplot(aes(x=minute, y=avg_delay)) +
    geom_line() 

# why should we use dep-time rather than sche-dep-time?
ggplot(sched_dep, aes(minute, n)) +
  geom_line()
# Because human have preference for hour/half/quarter points!

#plot number of flights per week
fl_dt %>% 
  count(week = floor_date(dep_time, "week ")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

#show the distribution of flights across the course of the day for every day of the year
#wrong answer
fl_dt %>% 
  mutate(dep_hour = update(dep_time, yday=1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_line(binwidth=300) # wrong geom type!!!

#right answer( this is just day 1)
fl_dt %>% 
  mutate(dep_hour= update(dep_time, yday=2 )) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth=300)

#exercise 1 How does the distribution of flight times within a day change over the course of the year?

#exercise 2 Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.

#exercise 3 Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)

#exercise 4 How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
  
#exercise 5 On what day of the week should you leave if you want to minimise the chance of a delay?
  
#exercise 6 What makes the distribution of diamonds$carat and flights$sched_dep_time similar?

#exercise 7 Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.

####################################16.4 time spans
# durations : how old is Deborah
D_age <- today() - ymd(19940211)
D_age

as.duration(D_age)

dyears()

# periods: compared with ddays(), days() is more 'human'
#pay attention of minutes() and minute() !

# filter in nycflights13 those overnight flights
fl_dt %>% 
  filter(arr_time < dep_time)
## for overnight flights, change them to physically accepted flights
fl_dt <- fl_dt %>% 
  mutate(over_night = arr_time < dep_time,
         arr_time = arr_time + days(over_night *1),
         sched_arr_time = sched_arr_time + days(over_night*1)) 
  fl_dt

# intervals
  next_year <- today() + years(1)
  (today() %--% next_year) / ddays(1) # this is the interval

## exercise 1 Why is there months() but no dmonths()?
# Because days in any given month are not always same (28,29,30,31)

# exercise 2 Explain days(overnight * 1) to someone who has just started learning R. How does it work?
# if arr_time < dep_time, overnight = true; when it is calculated , we trate it as 1.

# exercise 3 Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.

# exercise 4 Write a function that given your birthday (as a date), returns how old you are in years.

# exercise5  Why can’t (today() %--% (today() + years(1))) / months(1) work?
(today() %--% (today() + years(1))) / months(1) # why mine worked? :)

#16.5 time zones
# find your current time zone
Sys.timezone()
# get all timezones
length(OlsonNames())
head(OlsonNames())
