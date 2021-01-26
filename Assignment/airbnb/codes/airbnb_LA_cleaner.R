# Cleaning London airbnb file
# v.1.2. 2021-01-04 paths changed


# IN data from web
# out: airbnb_london_cleaned.csv

#setting working directory
rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
# setwd("")
setwd("/Users/wodediannao/desktop/airbnb/")
dir<-"/Users/wodediannao/desktop/"

#location folders
data_in  <- paste0(dir,"airbnb/raw/")
data_out <- paste0(dir,"airbnb/clean/")

library(tidyverse)

# zero step
# not necessary
data<-read.csv(paste0(data_in, "LA_listings.csv"))

glimpse(data)

drops <- c("host_thumbnail_url","host_picture_url","listing_url", "picture_url","host_url",
           "last_scraped","description", 
            "neighborhood_overview", 
            "host_about", "host_response_time", "name", "host_location")

# drops %in% colnames(data)

data <- data[ , !(names(data) %in% drops)]

write.csv(data,file=paste0(data_in,"airbnb_LA_listing.csv"))


#####################################

# opening dataset
df <- read.csv(paste0(data_in,"airbnb_LA_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)
              
#drop broken lines - where id is not a character of numbers
df$junk <- grepl("[[:alpha:]]", df$id)

df <- subset(df, df$junk==FALSE)
df <- df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified",
                 "instant_bookable", "has_availability")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df <- cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities")
df<-df[ , !(names(df) %in% drops)]

# MINOR STUFF
# data changed marginally, to make it compatible with textbook, we'll drop 27 rows. 

# not_in_book <- read.csv(paste0(data_in,"not_in_book.csv"), header=TRUE, row.names = 1)
# df<-df %>%
#   left_join(not_in_book, by="id")%>%
#   filter(is.na(not_in_book))%>%
#   select(-not_in_book)
  
#write csv
write.csv(df,file=paste0(data_out,"airbnb_LA_cleaned.csv"))
