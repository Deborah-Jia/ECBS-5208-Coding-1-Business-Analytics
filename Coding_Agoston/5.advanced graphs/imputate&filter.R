rm(list=ls()) #remove
library(tidyverse)
library(moments)
update.packages("readxl")
library(readxl)

data_in <- "~/Desktop/ECBS-5208-Coding-1-Business-Analytics/Class_4/data/"

bpp_orig <- read_excel( paste0( data_in , "raw/online_offline_ALL_clean.xls" ), guess_max = 40000 ) # get the 40,000 rows

library(tidyverse) #first run this line!
glimpse (bpp_orig)

bpp_orig <- mutate( bpp_orig , p_diff = price_online - price ) # add a new colomn

# calculate 3 values

p1_sum <- bpp_orig %>% summarise(
  mean     = mean(price),
  median   = median(price),
  std      = sd(price),
  iq_range = IQR(price), 
  min      = min(price),
  max      = max(price),
  skew     = skewness(price),
  numObs   = sum( !is.na( price ) ) )
library(moments)

p2_sum <- bpp_orig %>% summarise(
  mean     = mean(price_online),
  median   = median(price_online),
  std      = sd(price_online),
  iq_range = IQR(price_online), 
  min      = min(price_online),
  max      = max(price_online),
  skew     = skewness(price_online),
  numObs   = sum( !is.na( price_online ) ) )

p3_sum <- bpp_orig %>% summarise(
  mean     = mean(p_diff),
  median   = median(p_diff),
  std      = sd(p_diff),
  iq_range = IQR(p_diff), 
  min      = min(p_diff),
  max      = max(p_diff),
  skew     = skewness(p_diff),
  numObs   = sum( !is.na( p_diff ) ) )
##
price_summary <- p1_sum %>% add_row( p2_sum ) %>% add_row( p3_sum )
price_summary


add_column(price_summary,)
rm( p1_sum , p2_sum , p3_sum ) ## remove p1~p3


ggplot( data = bpp_orig ) +
  geom_histogram( aes( x = price ) , color = 'blue'  , alpha = 0.1 ) +
  labs(x = "Price",
       y = "Count" )


# filter data, here is pipe!
bpp <- bpp_orig %>% 
  filter(is.na(sale_online)) %>%
  filter(!is.na(price)) %>%
  filter(!is.na(price_online)) %>% 
  filter(PRICETYPE == "Regular Price")
bpp
#
bpp <- bpp %>% 
  filter( price < 1000 )
bpp

# do again
p1_sum <- bpp %>% summarise(
  mean     = mean(price),
  median   = median(price),
  std      = sd(price),
  iq_range = IQR(price), #Interquartile range
  min      = min(price),
  max      = max(price),
  skew     = skewness(price),
  numObs   = sum( !is.na( price ) ) )

p2_sum <- bpp %>% summarise(
  mean     = mean(price_online),
  median   = median(price_online),
  std      = sd(price_online),
  iq_range = IQR(price_online), 
  min      = min(price_online),
  max      = max(price_online),
  skew     = skewness(price_online),
  numObs   = sum( !is.na( price_online ) ) )

p3_sum <- bpp %>% summarise(
  mean     = mean(p_diff),
  median   = median(p_diff),
  std      = sd(p_diff),
  iq_range = IQR(p_diff), 
  min      = min(p_diff),
  max      = max(p_diff),
  skew     = skewness(p_diff),
  numObs   = sum( !is.na( p_diff ) ) )
##
price_summary <- p1_sum %>% add_row( p2_sum ) %>% add_row( p3_sum )
price_summary
rm( p1_sum , p2_sum , p3_sum )


ggplot( data = bpp ) +
  geom_density( aes( x = price ) , color = 'blue'  , alpha = 0.1 ) +
  geom_density( aes( x = price_online )  , color = 'red' , alpha = 0.1 ) +
  labs(x = "Price",
       y = "Relative Frequency" )


ggplot( data = bpp ) +
  geom_density( aes( x = p_diff ) , color = 'blue'  , alpha = 0.1 ) +
  labs(x = "Price differences",
       y = "Relative Frequency" )+
  xlim(-4,4)


chck <- bpp %>% filter( p_diff > 500 | p_diff < -500 )


bpp <- bpp %>% filter( p_diff < 500 & p_diff > -500 )
bpp
rm( chck )


#today's coding: creating factors in R
# tell r that they're nominal qualitative data
bpp$COUNTRY <- factor(bpp$COUNTRY)
table(bpp$COUNTRY) #table() makes a variable a table!!!


#calculate the mean for each country
bpp %>%  select(COUNTRY, p_diff) %>% 
  group_by(COUNTRY) %>% 
  summarise(mean =mean(p_diff),
            sd = sd(p_diff),
            num_obs = n()) # number of obsevation

##create ggplot for countries histogram
ggplot(data = bpp, aes(x=p_diff, fill = COUNTRY)) +
  geom_histogram(aes(y = ..density..), alpha =0.4) +
  labs(x= 'price', y= "relative frenquency", fill = 'country') +
  facet_wrap(~COUNTRY) # "~" tilda!!! Easy multi-panel plots!



#hw do the same with impute variable 
# 1) recode it as a string 
 #   a) if NA -> 'yes'
 #   b) if 1 -> "no'"
# 2) call factor cariable as "same_day"
# 3) create a summary by country and by same_day

# Hypothesis testing

# test: H0: the avg price difference between price_onine  - price = 0
#     HA: the avg price diff is non 0

t.test(bpp$p_diff, mu = 0)

# 0.47 difference between

#another hypothesis test 2: the online prices are smaller or equal to than off-line prices
# H0: online prices - price <= 0
# HA: price_online - price > 0

t.test(bpp$p_diff, mu =0, alternative = "less") #mu is the true value of the mean, H0 value

t.test(bpp$p_diff, mu =0, alternative = "greater")
# if we reject the null, we will have a probability of 95% of error of making an error. so we can't reject the null.

#hw filter to USA and price  < 1000 
#two-sided t-test

# multiple hypothesis testing

# don't be ashamed of tiny mistake

testing <- bpp %>% 
  select(COUNTRY, p_diff) %>% 
  group_by(COUNTRY) %>% 
  summarise(mean_pdiff = mean(p_diff) ,
            se_pdiff = 1/sqrt(n())*sd(p_diff), #standard error
            num_obs = n() ) #number of observations
testing # it is a variable, all these pipes are to display it for final result!


testing <- mutate(testing , t_stat = mean_pdiff / se_pdiff)
testing # this 'testing' line is a must after '<-' definition

testing <- mutate( testing, p_val = pt(-abs(t_stat), df = num_obs -1 ))
testing


testing <- mutate(testing, p_val = round( p_val, digits = 4))
testing


### association between online-offline prices
ggplot(bpp, aes(x= price_online, y= price)) +
  geom_point(color ='pink')+ ## add colour 
  labs(x= 'price online', y= 'price offline') +## add Modify axis, legend, and plot labels
  geom_smooth(method = 'lm', formula =y~x) ## smooth only is zig-zag, with formula and method it is straight now
# y~x is default!
# lm means linear model!

## bin-scatter

# 1) 'easy way: using equal distances

ggplot(bpp, aes(x= price_online, y= price)) +
  stat_summary_bin(fun= 'mean',binwidth = 50 ### this is a jointed function(stat&summary&bin)
                   ,geom = 'line', color='pink',
                   size = 3)
# fun can be mean or median; geom can be line or point

#group by countries
ggplot(bpp, aes(x= price_online, y= price,
                color= COUNTRY)) +
  stat_summary_bin(fun= 'mean',binwidth = 50, ### this is a jointed function(stat&summary&bin)
                  geom = 'point',
                   size = 2) +
  labs( x = 'price online', y = 'price offine' ,
     color = "Country") +
  facet_wrap(~COUNTRY, scales = 'free', ncol =1) + #scales can be fixed or free; ncol means how many colomns will these graphs be presented
  theme(legend.position = 'none') +
  geom_smooth(method = 'lm', formula = y~x)

###
# bin-scatter =2
## using percentiles instead of equally sized bins

bpp$price_online_10b <- bpp$price_online %>% 
      cut_number(10) #makes n groups with (approximately) equal numbers of observations



bs_summary <- bpp %>% 
    select(price, price_online_10b) %>% 
    group_by(price_online_10b) %>% # lst() build a list
    summarise_all(lst(p_min=min, p_max=max, #Summarise multiple columns
                    p_mean = mean,
                    p_median = median,
                    p_sd = sd,
                    p_num_obs = length ))
bs_summary # show all statistics

## 
bs_summary1 <- bs_summary %>% 
  separate(price_online_10b ,
           into = c("trash", 
                    "lower_bound",
                    "upper_bound"),
           sep = "[^0-9\\.]")
bs_summary1
## successfully separated

bs_summary1 <- bs_summary1 %>% 
  mutate( lower_bound = as.numeric(lower_bound)) %>% 
           mutate(upper_bound = as.numeric(upper_bound) ) %>%  ## Create, modify, and delete columns
  select(-trash ) #choose all except trash
bs_summary1 


bs_summary1 <- bs_summary1 %>% 
  mutate(mid_point= (lower_bound + upper_bound)/2)
bs_summary1 

## bin-scatter plot
ggplot(bs_summary1, aes(x=mid_point, y=p_mean)) +
  geom_point(size=2, color="red") +
  labs(x='online prices', y="offline prices")+
  xlim(0,100)+
  ylim(0,100)

## homework 
# do the same but for each contry(7 graphs), modifiy below 
# bpp$price_online_10b <- bpp$price_online %>% 
 # cut_number(10)

# change bins and change median and mean

# correlation and plots with factors

# calculate covariance
cov(bpp$price, bpp$price_online)

# is it symmetric
cov( bpp$price_online,bpp$price)
# yes!

#correalation
cor(bpp$price, bpp$price_online)
# extremely strong correalation

# make a correalation table for each country
corr_table <- bpp %>% 
  select(COUNTRY, price, price_online) %>% 
  group_by(COUNTRY) %>% 
  summarise(correlation = cor(price,price_online))

corr_table

## 
# graph to show the correlation pattern by each country
# fct_reorder(COUNTRY, correlation )

ggplot(corr_table, aes( x = correlation, y = fct_reorder(COUNTRY, correlation)) ) + ##Reorder factor levels by sorting along another variable
  geom_point(color= "pink", size =2) +
    labs(y="correation", x= "country")

#ggplot, geom and labs are linked together with plus mark

## https://github.com/Deborah-Jia/ECBS-5208-Coding-1-Business-Analytics/blob/master/Class_4/codes/billion_price_project.R
# read and learn to make some nice graphs