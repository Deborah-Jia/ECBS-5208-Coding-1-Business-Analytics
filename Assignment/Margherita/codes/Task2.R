library("readxl")
library(tidyverse)
library(moments)
library(ggpubr)
library(cowplot)
library(dplyr)

# Import data into R using relative path

path <- "../data/team-a-pizza-data.xlsx"
pizza_data <- read_excel(path)


########## Descriptive Statistics

# Check Descriptive Statistics for pizza 
summary_pizza_price<- pizza_data %>% summarise(
  Obs      = 'pizza',
  n        = sum( !is.na( marpizza_price ) ),
  mean     = round(mean(marpizza_price),digits = 2),
  median   = median(marpizza_price),
  min      = min(marpizza_price),
  max      = max(marpizza_price),
  sd      = round(sd(marpizza_price),digits = 2),
  skew     = round(skewness(marpizza_price),digits = 2))

# Check Descriptive Statistics for beverage
summary_bev_price<- pizza_data %>% summarise(
  Obs      = 'Beverage',
  n        = sum( !is.na( beverage_price ) ),
  mean     = round(mean(beverage_price),digits = 2),
  median   = median(beverage_price),
  min      = min(beverage_price),
  max      = max(beverage_price),
  sd      = round(sd(beverage_price),digits = 2),
  skew     = round(skewness(beverage_price),digits = 2))

# Join the two descriptive statistics table
price_summary <- summary_pizza_price %>% add_row( summary_bev_price ) 

############# descriptive graphs of price distributions of your products in the whole data
# Plot a histogram for marpizza_price with a kernel density estimate
ggplot(pizza_data, aes(x = marpizza_price)) + 
  geom_histogram(aes(y =..density..), 
                 colour= "black", 
                 fill = "white",
                 binwidth = 100) +
  geom_density(alpha=.2, fill="orange") +
  geom_vline(aes(xintercept = mean(marpizza_price, na.rm=T)), # Add a vertical line for mean of marpizza_price and ignore NA values for mean
             color="orange", linetype="dashed", size=1) +
  labs(x = 'Margherita pizza Price', y = 'Density')

##### Beverage price
ggplot(pizza_data, aes(x = beverage_price)) + 
  geom_histogram(aes(y =..density..), 
                 colour= "black", 
                 fill = "white",
                 binwidth = 30) +
  geom_density(alpha=.2, fill="orange") +
  geom_vline(aes(xintercept = mean(beverage_price, na.rm=T)), # Add a vertical line for mean of beverage_price and ignore NA values for mean
             color="orange", linetype="dashed", size=1) +
  labs(x = 'Beverage Price', y = 'Density')


# Plot the pizza price difference between Inner and Outer Budapest in two histograms
ggplot(data = pizza_data, aes(x= marpizza_price, fill = Inner_Outer)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, color = 'black', fill='white', alpha =0.5) +
  geom_density(alpha=0.2, fill = 'orange')+
  labs(x= 'Pizza Price: Inner vs. Outer Budapest', y= 'Relative Frenquency') +
  facet_wrap(~Inner_Outer) +
  geom_vline(aes(xintercept = mean(marpizza_price, na.rm=T)),
             color= 'orange', linetype= 'dashed', size=1)


# hypothesis testing
inner_values <- pizza_data$marpizza_price[pizza_data$Inner_Outer == "Inner"]
# get the prices of pizzas in Outer Budapest
outer_values <- pizza_data$marpizza_price[pizza_data$Inner_Outer == "Outer"]
# create the data frame of pizza prices in  Inner and outer Budapest and their difference
pizza_df <- data.frame("inner_prices" = inner_values, "outer_prices" = outer_values, "p_diff" = inner_values - outer_values)
t.test(pizza_df$p_diff, mu = 0)

