rm(list=ls())

install.packages("WDI")
library("WDI")

a <- WDIsearch("GDP")

#narrow down our search
a <- WDIsearch("GDP.*capita.*constant")

gdp_data <-WDI(indicator = "NY.GDP.PCAP.PP.KD", country = "all", start = 2019, end = 2019)

a <- WDIsearch('population, total')
b <- WDIsearch("life expectancy at birth")

# get all the data
data_raw