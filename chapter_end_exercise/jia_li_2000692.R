#########################
#chapter 1.1
rm(list=ls())
library(tidyverse)
data_in <- "~/Desktop/CodingInR🆗/Coding1/" 
h_vienna <- read_csv(paste0(data_in,"hotels_vienna.csv"))

sample_avg <- function(s){
  result <- mean(sample(h_vienna$price, s))
  print(result)}

sample_avg(25) 
sample_avg(50)
sample_avg(200)

#calculate average price for all
avg <- mean(h_vienna[["price"]])
print(avg)

# From the result we can see, as the size of sample increase, sample average price moves closer to population average.

#########################
#chapter 6.1 
rm(list=ls()) 
library(tidyverse)
library(moments)
update.packages("readxl")
library(readxl)

data_in <- "~/Desktop/ECBS-5208-Coding-1-Business-Analytics/Class_4/data/"

NL_bpp_origin <- read_excel( paste0( data_in , "raw/online_offline_ALL_clean.xls" ), guess_max = 40000 ) 

NL_bpp <- NL_bpp_origin %>% 
  filter(!is.na(price)) %>%
  filter(!is.na(price_online)) %>% 
  filter(PRICETYPE == "Regular Price") %>% 
  mutate(NL_bpp, p_diff = price_online - price)

# Hypothesis test

# H0: no the average price difference in Netherlands:price_online - price = 0
# HA: the avg price diff is non 0.
# we set the significance level as 1%
t.test( NL_bpp$p_diff , mu = 0 )

# p-value(29.98%) larger  than 1%, so we cannot reject the null hypothesis.
# there is no price difference between online and off-line prices in Netherlands


# Test 2: The online prices are smaller or equal to offline prices in Netherlands
#   H0: price_online - price <= 0
#   HA: price_online - price >  0
# # we set the significance level as 1%
t.test( NL_bpp$p_diff , mu = 0 , alternative = "greater" )

# P-value(85.01%) far larger than our significance level, we cannot reject null hypothesis
# In netherlands, online prices are smaller or equal to offline prices

# Test 3: The online prices in NL are larger or equal to offline prices
#   H0: price_online - price >= 0
#   HA: price_online - price <  0
# # we set the significance level as 1%

t.test( NL_bpp$p_diff , mu = 0 , alternative = "less" )
#P-value(14.99) far larger than our significance level of 1%, we can not reject the null hypothesis
# The online prices in NL are larger or equal to offline prices

## From three tests, we conclude that there is no price difference between online and off-line prices in Netherlands.
## Compared with case study, we arrived at the same conclusion. 



