a <- c("amenities","translation missing: en.hosting_amenity_49",
       "translation missing: en.hosting_amenity_50")

a %in% colnames(df)

b <- data %>% group_by(property_type) %>% summarise(n=n()) %>% arrange(-n)


data$property_type %>% str_detect("Entire")

