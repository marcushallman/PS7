library(dplyr)
library(ggplot2)

# Problem 1
setwd("~/Documents/PS7")
crime_data <- read.csv("March2018.csv")


# Problem 2
crime_data <- mutate(crime_data, cleaned = gsub("-.*$", "", crime_data$Description))
daily_rates <- crime_data %>%
  group_by(cleaned) %>%
  summarise(count = n()/31) %>%    #Divides total by days in a month
  arrange(desc(count))   #sort by count
daily_rates

#Larceny is tbe most common crime

# Provlem 3
daily_neighborhood <- crime_data %>%
  group_by(Neighborhood) %>%
  summarise(count = n()/31) %>%  #Divides total by days in a month
  arrange(desc(count))   #Sort by count
daily_neighborhood

#Neighborhood 35 has the monst daily crimes
# problem 4
robbery_district <- crime_data %>%
  group_by(District) %>%
  mutate(robbery = ifelse(cleaned == "ROBBERY", 1, 0)) %>%
  summarise(total_count = n(),
        robbery_count = sum(robbery),
        percrime = robbery_count/total_count) %>%
  arrange(desc(percrime))  
robbery_district

# District 5 has the most robberies per crime

# problem 5

