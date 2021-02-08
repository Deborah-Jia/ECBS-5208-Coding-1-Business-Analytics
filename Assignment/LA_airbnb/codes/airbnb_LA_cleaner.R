# in data from local
# out: airbnb_LA_cleaned.csv

#clear memory
rm(list=ls())

# load necessary packages
library(tidyverse)
library(stargazer)
library(Hmisc)

#location folders
dir <- "~/Desktop/ECBS-5208-Coding-1-Business-Analytics/Assignment/LA_airbnb"

data_in <- paste0(dir,"/raw/")
data_out <- paste0(dir,"/clean/")

# retrieve data from raw/ folder
data <- read.csv(paste0(data_in, "LA_listings.csv"))

# take a look at all columns and tag unnecessary ones.
glimpse(data)
drops <- c("host_thumbnail_url","host_picture_url","listing_url", "picture_url","host_url",
           "last_scraped","description", "neighborhood_overview", 'scrape_id',
            "host_about", "host_response_time", "name", "host_location")

# drop useless columns
data <- data[ , !(names(data) %in% drops)]
              
#drop broken lines - where id is not a character of numbers
data$junk <- grepl("[[:alpha:]]", data$id)

data <- subset(data, data$junk==FALSE)
data <- data[1:ncol(data)-1]

#display the class and type of each columns
sapply(data, class)
sapply(data, typeof)

## choose listings that can hold 2-6 persons
table(data$accommodates)
data <- data %>% filter(between(accommodates, 2, 6))

## filter apartments
data %>% group_by(property_type) %>% summarise(n=n()) %>% arrange(-n)

data <- data %>% 
  filter(str_detect(property_type, "apartment|apt"))

# data %>% group_by(room_type) %>% summarise(n=n()) %>% arrange(-n)

# rename apartment types
data <- data %>%
  mutate(property_type = ifelse(str_detect(property_type, "Entire"), "Entire_apt", 'Room_apt'))

table(data$property_type)
# data <- data %>%  
# filter(property_type %in% c("Entire apartment", "Private room in apartment",
#                             "Entire serviced apartment", 'Shared room in apartment'))

data <- data %>%
  mutate( f_property_type = factor(property_type))
#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  data[[perc]]<-gsub("%","",as.character(data[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price")){
  data[[pricevars]]<-gsub("\\$","",as.character(data[[pricevars]]))
  data[[pricevars]]<-as.numeric(as.character(data[[pricevars]]))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified",
                 "instant_bookable", "has_availability")){
  data[[binary]][data[[binary]]=="f"] <- 0
  data[[binary]][data[[binary]]=="t"] <- 1
}

# no longer need below commented codes 
# #amenities
# data$amenities<-gsub("\\[","",data$amenities)
# data$amenities<-gsub("\\]","",data$amenities)
# data$amenities<-gsub('\\"',"",data$amenities)
# 
# data$amenities <- as.list(strsplit(data$amenities, ","))
# 
# #define levels and dummies 
# levs <- levels(factor(unlist(data$amenities)))
# data <- cbind(data, as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), 
#                                                         table))))
# # eyeballing all columns and drop those obviously meaningless ones:
# view(data)
# 
# drops <- c("amenities", " This is to get the guest started guest will need to buy more once used body soap",
#            " This is to get the guest started guest will need to buy more once used shampoo" )
# data <- data[ , !(names(data) %in% drops)]

# some trial work (for savings only) --------------------------------------
df1 <- data # for saving data values! never use df1!
df2 <- data

# first create unique list of all amenities in the df2
amenities_unique <- 
  unique(
    unlist(
      df2$amenities %>% str_extract_all('(?<=")(\\w+\\s*\\w*)(?=")')
    )
  )
# then iterate over the list and create a new column for each unique amenity and fill values with this logic:
# in the new column, if the amenity is detected as a string in the original "amenities" at this row, set value to 1
# else set value to 0
for (i in amenities_unique){
  df2 <- 
    df2 %>% 
    mutate(!! i := ifelse(str_detect(amenities, !! i), 1, 0))
}

# check words in columns and build new vector for 2nd step cleaning
# words <- strsplit(colnames(df2)," ")
# 
# w_freq <- table(unlist(words)) %>% 
#   as.data.frame() %>% 
#   arrange(desc(Freq))

# table(df2$` Pool toys`)
# df2 %>% select(contains('toys'))

df2 <- df2 %>% rename( refrigerator1 = `Kitchenaide refrigerator` ,
                Coffee_maker = `Nespresso machine` , 
                bread_maker = Toaster ,
                Barbecue_tool = `BBQ grill` )

column_names <- c('refrigerator', 'TV', 'stove', 'shampoo','conditioner',
                  'wifi', 'tub','oven', 'kitchen', 'heating', 'coffee', 'bread', 'barbecue')


# categorize dummy variables generated from the amenities
for( word in column_names){
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- df2 %>% select(contains(word),"id")
  #Go row by row to see if any of the rows have at least one '1'. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  #merge original dataframe and new_df_merge by 'id'
  df2 <- merge(df2, new_df_merge,by = "id", all = FALSE)
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  df2 <- df2 %>% select(-colnames(new_df))
  names(df2)[names(df2) == 'col_name'] <- paste0(word,"_agg")
}

df2 <- df2[ , colSums(is.na(df2)) < nrow(df2)] # remove columns that are all N/As
table(colSums(is.na(df2)) == nrow(df2))

# # eyeball all columns and drop those useless ones
# df2[,order(colnames(df2))]
# drops <- c('host_listings_count', 'neighbourhood', 'amenities', 
#            'calculated_host_listings_count_entire_homes', 
#            'calculated_host_listings_count_private_rooms', 
#            'calculated_host_listings_count_shared_rooms',
#            'Washer', 'Essentials', 'Dryer', 'Pool toys', 'license', 
#            'number_of_reviews_ltm', 
#            'review_scores_accuracy', 'availability_30', 
#            'availability_60', 'availability_90', 'availability_365', 
#            'review_scores_cleanliness', 'review_scores_value', 
#            'Host greets you', 'Single level home', 
#            'Cooking basics', 'Keypad', 'Changing table', 'host_since', 
#            'host_verifications', 'maximum_nights', 'maximum_nights', 
#            'minimum_minimum_nights', 'maximum_minimum_nights', 
#            'minimum_maximum_nights', 'maximum_maximum_nights', 
#            'minimum_nights_avg_ntm', 'maximum_nights_avg_ntm', 'number_of_reviews_l30d', 
#            'calculated_host_listings_count', 'bathrooms_text', 'host_neighbourhood', 
#            'review_scores_checkin', 'review_scores_communication', 'review_scores_communication')
# df2 <- df2[ , !(names(df2) %in% drops)]

data <- df2 %>% rename(coffee_maker = coffee_agg, 
               bread_maker = bread_agg,
               barbecue_tool = barbecue_agg)
# now we have 107 columns


# Advanced cleaning------------------------------------------------------------------------------------------------------

# set working directory for da_case_studies
setwd("/Users/wodediannao/desktop/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") 

output <- paste(dir, "out/", sep = "/") # path for case studies (extensive folders)
create_output_if_doesnt_exist(output) # customized function

options(digits = 3)


table(data$property_type)  # a glimpse of this column (check the sorts and size of each type)


# # rename Townhouse to House
# data <- data %>%
#   mutate( # if "==", then change name, or else keep it original
#     property_type = ifelse(data$property_type == "Townhouse", "House", data$property_type)) # why create a duplicate variable
# 
# data <- data %>% # '%in%' means any of 
#   filter(property_type %in% c("Apartment", "House", "Townhouse"))
# 
# data <- data %>%
#   mutate( f_property_type = factor(property_type)) # why create a duplicate variable


#Room type as factor
table(data$room_type) # like "group_by"

data <- data %>%
  mutate(f_room_type = factor(room_type)) # still don't understand

# Rename room type because it is too long (nested structure)
data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))

# # cancellation policy as factor: no need to run as there is no such column
# table(data$cancellation_policy)
# # if cancellation policy is super strict 30 or 60, rename it as strict
# data <- data %>%
#   mutate(
#     cancellation_policy = ifelse(cancellation_policy %in% c("super_strict_30", "super_strict_60"),
#                                  "strict", cancellation_policy),
#     f_cancellation_policy = factor(cancellation_policy))

# bed_type and neighbourhood_cleansed as factors
# table(data$neighbourhood_cleansed)
# rename to Couch
# data <- data %>%
#   mutate(
#     bed_type = ifelse(bed_type %in% c("Futon", "Pull-out Sofa", "Airbed"), "Couch", bed_type),
#     f_bed_type = factor(bed_type),
#     f_neighbourhood_cleansed = factor(neighbourhood_cleansed))
# data <- data %>%
#   mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

#---------------------------------------------------------------------------------------------------------------------------

## Create Numerical variables
data <- data %>%
  mutate(usd_price_day = price,
    p_host_response_rate = as.numeric(host_response_rate))
# # rename cleaning_fee column
# data <- data %>%
#   rename(usd_cleaning_fee = cleaning_fee)
#-------------------------------------------------------------------

# add new numberical columns from certain columns

numericals <- c("accommodates", 'bedrooms', 'beds', 
                "review_scores_rating", "number_of_reviews", "reviews_per_month", 
                "minimum_nights", 'host_total_listings_count')

# table(numericals %in% colnames(data))

data <- data %>% # a little bit like "lapply"
  mutate_at(vars(numericals), funs("n"=as.numeric)) # A list of columns generated by vars()
# A function fun 


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()

nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
# change location of certain columns
data <- data %>% relocate(host_is_superhost, host_has_profile_pic, host_identity_verified,
                  has_availability, instant_bookable, .before = f_room_type)
# create dummy vars
dummies <- names(data)[seq(56,144)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()

dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id, neighbourhood_cleansed, 
         room_type,property_type)

# with price info only
data <- data %>%
  drop_na(price)

write_csv(data, paste0(data_out, "airbnb_LA_workfile.csv"))

library(skimr)
##################################
# DESCRIBE

#
#####################
### look at price ###
#####################

data <- read_csv(paste0(data_out, "airbnb_LA_workfile.csv"))
df1 <- data
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 50,fill='navyblue') 

# Remove extreme values + missing from prices (this case: only 3 missing values)
data <- data %>%
  filter(price <= 375, price >= 20 ) 

# Histograms
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[5], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
R_F14_h_lnprice
ggsave(paste0(output, "R_F14_h_lnprice.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

# what's this for?
cairo_ps(filename = paste0(output, "R_F14_h_lnprice.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(R_F14_h_lnprice)
dev.off()

R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[5], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price
ggsave(paste0(output, "R_F14_h_price.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_F14_h_price.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_F14_h_price)
dev.off()

################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,400)+
  xlim(1,7)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2 = n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

# Regression 1: ln price and num of accomodates and squares
lm(ln_price ~ n_accommodates + n_accommodates2, data=data)
# Regression 2: ln price and log num of accomodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accomodates
lm(ln_price ~ n_accommodates, data=data)

## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

## beds
ggplot(data, aes(n_beds)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of beds") +
  theme_bg()

# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))


# Pool accommodations with 0,1,2,10 bathrooms # round the number according to c()

# data <- data %>%
#   mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) ) 
# 
# 
# data %>%
#   group_by(f_bathroom) %>%
#   summarise(mean_price = mean(price), n = n())

summary(data$n_number_of_reviews)
describe(data$n_number_of_reviews)
## Number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews < 135, n_number_of_reviews > 0)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.6, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-135 and >135
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,4,135,max(data$n_number_of_reviews)), 
                                   labels=c(0,4,135), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())
# Regression 1: log-price and number of reviews
reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)
# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)
## Time since
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

# Check the effect
lndays_plot <- data %>%
  filter(data$price<= 375, ln_days_since>2)

skimr::skim(data$n_number_of_reviews)
ggplot(data = data, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(80,150)+
  xlim(0,25)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()

#-Inf values
#lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data)

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(0,400)+
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bg()

# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
#leave as is

## minimum nights
lm(ln_price ~ n_minimum_nights,data=data)

describe(data$n_minimum_nights)
# Pool and categorize the number of minimum nights: 1,30, 30+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,30,max(data$n_minimum_nights)), labels=c(1,30), right = F))

lm(ln_price ~ f_minimum_nights,data=data)

###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type", "f_room_type")


for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}
#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data, paste0(data_out, "airbnb_LA_workfile_adj.csv"))

# Prediction will delete later!------------------------------------------------------------------------------------------------
#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 14
# CH14B Predicting AirBnB apartment prices: selecting a regression model
# using the airbnb dataset
# version 0.91 2020-01-05
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study

# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)


source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch14-airbnb-reg/"

# # data used
# data_in <- use_case_dir
# 
# data_out <- use_case_dir
# output <- paste0(use_case_dir,"output/")
# create_output_if_doesnt_exist(output)
# 
# options(digits = 3)



########################################
# PART I.
########################################

# !!! make sure you have run ch14_airbnb_prepare.R before


#############
# Load data #
#############

# Used area
data <-
  read_csv(paste0(data_out, "airbnb_LA_workfile_adj.csv")) %>%
  mutate_if(is.character, factor)


######################
# Quick look at data #
######################
glimpse(data)
skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x))) # what's function(x)?
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target (already did)
data <- data %>%
  drop_na(price)


# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds)
  )

# 3. drop columns when many missing not important
to_drop <- c("p_host_response_rate", "n_reviews_per_month")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    n_bedrooms =  ifelse(is.na(n_bedrooms), median(n_bedrooms, na.rm = T), n_bedrooms),
    flag_n_bedrooms =ifelse(is.na(n_bedrooms),1, 0)
  )


table(data$flag_days_since)

# Look at data
summary(data$price)

# where do we have missing variables now?
(to_filter <- sapply(data, function(x) sum(is.na(x))))
to_filter[to_filter > 0]


###################################
# Business logic- define our prediction problem
###################################

# Decision
# Size, we need a normal apartment, 2-6persons
# that's gonna be our sample
skimr::skim(data)

#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type` and the `bed_type`?
data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

# data %>%
#   group_by(f_bed_type) %>%
#   dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price) # I think I can skip this step as I already filtered price


# Distribution of price by type below 400

# Histograms
# price
g3a <- ggplot(data=data, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 15) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  #  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bg() 
g3a 
save_fig("LA_airbnb-price", output, "small")

# lnprice
ggplot(data=data, aes(x=ln_price)) +
  g3b <- geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(3, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g3b
save_fig("LA_airbnb-lnprice", output, "small")



## Boxplot of price by room type
g4 <- ggplot(data = data, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1], color[3],color[4]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = c(color[2],color[1], color[3],color[4]), fill = c(color[2],color[1], color[3],color[4]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bg()
g4
save_fig("LA-airbnb-room", output, "small")

# Boxplot (need to change the property type to two or three)
g5 <- ggplot(data, aes(x = factor(n_accommodates), y = price,
                        fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1])) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 350), breaks = seq(0,400, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
g5
save_fig("LA-airbnb-accom", output, "small")


########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since", "flag_days_since")
# poly_lev %in% colnames(data)

# Factorized variables
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)

#################################################
# Look for interactions
################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_room_type", "d_instant_bookable", "Room type", "instant_bookable")
p2 <- price_diff_by_variables2(data, "f_room_type", "f_property_type", "Room type", "Property type")
# #Look up canellation policy
# p3 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_familykidfriendly", "Cancellation policy", "Family kid friendly")
# p4 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_tv", "Cancellation policy", "TV")
#Look up property type
p3 <- price_diff_by_variables2(data, "f_property_type", "d_airconditioning", "Property type", "Air-conditioning")
p4 <- price_diff_by_variables2(data, "f_property_type", "d_wifi_agg", "Property type", "Wifi_available")
p5 <- price_diff_by_variables2(data, "f_property_type", "d_bread_maker", "Property type", "bread_maker")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_trashcompactor", "Property type", "trashcompactor")

g_interactions <- plot_grid(p1, p2, p3, p4,p5, p6, nrow=3, ncol=2)
g_interactions
#save_fig("ch14_airbnb_interactions",output,"verylarge")
save_fig("LA-airbnb-interactions",output,"verylarge")



# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type",  "f_room_type*d_instant_bookable")

# Additional interactions of factors and dummies
X2  <- c("d_wifi_agg*f_property_type", "d_bread_maker*f_property_type", "d_trashcompactor*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type) * (",
                paste(amenities, collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducible
set.seed(20210202)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20210202)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:8)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "ln_price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)

skim(data_train$ln_days_since)

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)



# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(0.3, 0.6), breaks = seq(0.3,0.6, 0.1)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels
save_fig("ch14-figure-7-airbnb-model-result-levels", output, "small")



#################################
#           LASSO               #
#################################

# take model 8 (and find observations where there is no missing data)may
vars_model_7 <- c("price", basic_lev,reviews,poly_lev,X1,X2,amenities)
vars_model_8 <- c("price", basic_lev,reviews,poly_lev,X1,X2,amenities,X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("ln_price ~ ", paste(setdiff(vars_model_8, "ln_price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])

# Note: re book
# The textbook contains a somewhat different table and graph for train and test RMSE. 
# The ordering is the same but the numbers are not. This is because of some minor change in cleaing file. Sory. 


########################################
# PART III.
########################################



###################################################
# Diagnsotics #
###################################################
model4_level <- model_results_cv[["modellev4"]][["model_work_data"]]
model7_level <- model_results_cv[["modellev7"]][["model_work_data"]]


# look at holdout RMSE
model7_level_work_rmse <- mse_lev(predict(model7_level, newdata = data_work), data_work[,"ln_price"] %>% pull)**(1/2)
model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"ln_price"] %>% pull)**(1/2)
model7_level_holdout_rmse

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["ln_price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("ln_price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
(d$elev <- d$ylev - d$predlev)

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 10, yend =10), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(2,7), ylim = c(2, 7)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0,10), breaks=seq(0, 10, by= 1)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0,10), breaks=seq(0, 10, by=1)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred
save_fig("LA-level-vs-pred", output, "small")


# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("ln_price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
                   conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev7_holdout_summary.tex"))


LA_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (US dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
LA_CI_n_accomodate
save_fig("LA-8b-ci-n-accomodate", output, "small")

# #NOT USED
# # Density chart (not in book)
# g3 <- ggplot(data = datau, aes(x=price)) +
#   geom_density(aes(color=f_room_type, fill=f_room_type),  na.rm =TRUE, alpha= 0.3) +
#   labs(x="Price (US dollars)", y="Density", color = "") +
#   scale_color_manual(name="",
#                      values=c(color[2],color[1], color[3]),
#                      labels=c("Entire home/apt","Private room", "Shared room")) +
#   scale_fill_manual(name="",
#                     values=c(color[2],color[1], color[3]),
#                     labels=c("Entire home/apt","Private room", "Shared room")) +
#   theme_bg() 
# theme(legend.position = c(0.7,0.7),
#       legend.direction = "horizontal",
#       legend.background = element_blank(),
#       legend.box.background = element_rect(color = "white"))
# g3
# 
# 
# # Barchart  (not in book)
# plot4 <- ggplot(data = datau, aes(x = factor(n_accommodates), color = f_room_type, fill = f_room_type)) +
#   geom_bar(alpha=0.8, na.rm=T, width = 0.8) +
#   scale_color_manual(name="",
#                      values=c(color[2],color[1], color[3])) +
#   scale_fill_manual(name="",
#                     values=c(color[2],color[1],  color[3])) +
#   labs(x = "Accomodates (Persons)",y = "Frequency")+
#   theme_bg() 
# theme(legend.position = "bottom")
# plot4
# 
# 
# 
# 

# Random Forest -----------------------------------------------------------
#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 16
# CH16A Predicting apartment prices with random forest
# using the airbnb dataset
# version 0.9 2020-09-09
#########################################################################################



# ------------------------------------------------------------------------------------------------------

library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)



# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

#-----------------------------------------------------------------------------------------

#########################################################################################
#
# PART I
# Loading and preparing data ----------------------------------------------
#
#########################################################################################

data <- read_csv(read_csv(paste0(data_out, "airbnb_LA_workfile_adj.csv"))) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))


count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}

count_missing_values(data)

# Sample definition and preparation ---------------------------------------

# We focus on normal apartments, n<8
# data <- data %>% filter(n_accommodates < 8)
table(data$n_accommodates)


# copy a variable - purpose later, see at variable importance
data <- data %>% mutate(n_accommodates_copy = n_accommodates)

# basic descr stat -------------------------------------------
skimr::skim(data)
summary(data$price)
Hmisc::describe(data$price)
describe(data$f_room_type)
describe(data$f_property_type)
table(data$f_number_of_reviews)

# create train and holdout samples -------------------------------------------
# train is where we do it all, incl CV

set.seed(2802)

# First pick a smaller than usual training set so that models run faster and check if works
# If works, start anew without these two lines

# try <- createDataPartition(data$price, p = 0.2, list = FALSE)
#data <- data[try, ]



train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

# Define models: simpler, extended -----------------------------------------------------------

# Basic Variables inc neighnourhood
basic_vars <- c("n_accommodates", "n_beds", "n_days_since", "f_property_type","f_room_type", 
                "f_neighbourhood_cleansed")

# reviews
reviews <- c("n_number_of_reviews", "f_number_of_reviews" ,"n_review_scores_rating", "flag_review_scores_rating")

# reviews %in% colnames(data)

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
# from ch14
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",  "f_room_type*d_instant_bookable",
         "d_wifi_agg*f_property_type", "d_bread_maker*f_property_type", "d_trashcompactor*f_property_type")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)


#########################################################################################
#
# PART II
# RANDOM FORESTS -------------------------------------------------------
#
#########################################################################################



# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("ln_price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("ln_price ~ ", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action=na.exclude
  )
})

rf_model_2

# auto tuning first
# set.seed(1234)
# system.time({
#   rf_model_2auto <- train(
#     formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#     data = data_train,
#     method = "ranger",
#     trControl = train_control,
#     importance = "impurity"
#   )
# })
# rf_model_2auto 
rf_model_2auto <- rf_model_2





# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2,
    model_2b = rf_model_2auto
    
  )
)
summary(results)

# Save outputs -------------------------------------------------------

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(output,"rf_tuning_modelB.tex"))


# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_2auto$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size,
  rf_model_2auto$finalModel$min.node.size
  
),
nrow=3, ncol=2,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_turning_choices.tex"))

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`),
                     mean(results$values$`model_2b~RMSE`)
),
nrow=3, ncol=1,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_rmse.tex"))


#########################################################################################
#
# PART III
# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))

cutoff = 0.002
rf_model_2_var_imp_plot <-  ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp > cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot
#save_fig("rf_varimp1",output, "large")
save_fig("LA-rf-varimp-base",output, "large")

##############################
# 2) full varimp plot, top 10 only
##############################


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b
#save_fig("rf_varimp1_b",output, "small")
save_fig("LA-2b-rf-varimp-top10",output, "small")


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_grouped_plot
#save_fig("rf_varimp_grouped1",output, "small")
save_fig("ch16-figure-2a-rf-varimp-group",output, "small")



#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

# TODO
# : somehow adding scale screws up. ideadlly both graphs y beween 70 and 130,
# n:accom should be 1,7 by=1

# FIXME
# should be on holdout, right? pred.grid = distinct_(data_train, "), --> pred.grid = distinct_(data_holdout, )

pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  geom_line(color=color[1], size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bg()
pdp_n_acc_plot
#save_fig("rf_pdp_n_accom", output, "small")
save_fig("ch16-figure-3a-rf-pdp-n-accom", output, "small")

# describe(data_holdout_w_prediction$n_accommodates)


pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  ylab("Predicted ln_price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(0, 10), breaks=seq(0,10, by=1)) +
  theme_bg()
pdp_n_roomtype_plot
#save_fig("rf_pdp_roomtype", output, "small")
save_fig("ch16-figure-3b-rf-pdp-roomtype", output, "small")

# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))



######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("Westminster", "Camden", "Kensington and Chelsea", "Tower Hamlets", "Hackney", "Newham")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Apartment", "House")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Borough", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)

##########################################



#########################################################################################
#
# PART IV
# HORSERACE: compare with other models -----------------------------------------------
#
#########################################################################################



# OLS with dummies for area
# using model B

set.seed(1234)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

# * LASSO
# using extended model w interactions

set.seed(1234)
system.time({
  lasso_model <- train(
    formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
    trControl = train_control
  )
})

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # the column has a name "1", to be renamed

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)
regression_coeffs %>%
  write.csv(file = paste0(output, "regression_coeffs.csv"))

# CART
set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")

# GBM  -------------------------------------------------------
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model


# much more tuning

#  faster, for testing
#gbm_grid2 <-  expand.grid(interaction.depth = c( 5, 7, 9, 11), # complexity of the tree
#                          n.trees = (1:10)*50, # number of iterations, i.e. trees
#                          shrinkage = c(0.05, 0.1), # learning rate: how quickly the algorithm adapts
#                          n.minobsinnode = c(10,20) # the minimum number of training set samples in a node to commence splitting
#)


# the next will be in final model, loads of tuning
gbm_grid2 <-  expand.grid(interaction.depth = c(1, 3, 5, 7, 9, 11), # complexity of the tree
                          n.trees = (1:10)*50, # number of iterations, i.e. trees
                          shrinkage = c(0.02, 0.05, 0.1, 0.15, 0.2), # learning rate: how quickly the algorithm adapts
                          n.minobsinnode = c(5,10,20,30) # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model2 <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                      data = data_train,
                      method = "gbm",
                      trControl = train_control,
                      verbose = FALSE,
                      tuneGrid = gbm_grid2)
})
gbm_model2


# and get prediction rmse and add to next summary table

# ---- compare these models

final_models <-
  list("OLS" = ols_model,
       "LASSO (model w/ interactions)" = lasso_model,
       "CART" = cart_model,
       "Random forest (smaller model)" = rf_model_1,
       "Random forest" = rf_model_2,
       "Random forest (auto tuned)" = rf_model_2auto,
       "GBM (basic tuning)"  = gbm_model,
       "GBM (broad tuning)" = gbm_model2)

results <- resamples(final_models) %>% summary()


# Save output --------------------------------------------------------
# Model selection is carried out on this CV RMSE

result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"horse_race_of_models_cv_rmse.tex"))




# evaluate preferred model on the holdout set -----------------------------

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

kable(x = result_5, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"horse_race_of_models_houldout_rmse.tex"))

#ch16-table-1-rf-models-turning-choices
#ch16-table-2-performance-across-subsamples
#ch16-table-3-horse-race-of-models-cv-rmse


