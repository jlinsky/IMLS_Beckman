
# load tidyverse library with packages we'll use
library(tidyverse)

# read in data
data <- read.csv("./Desktop/work/Oak spp. with threat code and country.csv",
  stringsAsFactors = F)
# glance at data
str(data)

# remove NA rows
data <- data %>% filter(Country.distr. != "#N/A")
nrow(data) #682

# make sure there aren't any spaces in the country codes column
data$Country.distr. <- gsub(" ", "", data$Country.distr.)
sort(unique(data$Country.distr.))

# separate by country code column and create additional rows
data2 <- separate_rows(data, Country.distr.)
str(data2)

# write file
write.csv(data2, "./Desktop/work/Oak spp. with threat code and country - tidy.csv",
  row.names = F)
