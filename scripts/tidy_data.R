
# load tidyverse library with packages we'll use
library(tidyverse)
library(data.table)

##
### We want one row for each observation (rather than concatenated in a cell)
##

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

##
### We want to arrange the countries in one cell alphabetically
##

# read in data
data <- read.csv("./Desktop/work/Appendices - Quercus species - Alphabetically.csv",
  stringsAsFactors = F)
# glance at data
str(data)

# make sure there aren't any spaces in the country codes column
data$Country.distribution <- gsub(" ", "", data$Country.distribution)
sort(unique(data$Country.distribution))

# separate country distribution column by comma then arrange each element
#   alphabetically, then join back to data
ctry_arranged <- setDT(data)[,list(Country.distribution =
  toString(sort(unique(strsplit(Country.distribution,',')[[1]])))),
  by = Genus.species]
data <- data %>%
  dplyr::select(-Country.distribution) %>%
  full_join(ctry_arranged)
# remove spaces in the new column
data$Country.distribution <- gsub(" ", "", data$Country.distribution)
unique(data$Country.distribution)

# write file
write.csv(data, "./Desktop/work/Appendices - Quercus species - Alphabetically - tidy.csv",
  row.names = F)
