# Notes from DataCamp courses

#########################################
#### Introduction to Cleaning Data in R
#########################################

# Understanding the structure of your data
class()
dim()
names()
str()
dplyr::glimpse()
summary() # can also see counts of NAs

# Looking at your data
head() # n = num rows to show
tail() # n = num rows to show

# Visualizing your data
hist()
plot()
boxplot()

# CLEAN DATA: Observations in rows, variables in columns, each table represents one type of observational unit

# Introduction to tidyr
gather(data,key,value...) # wide df to long
  #bmi_long <- gather(bmi, year, bmi_val, -Country)
  #weather2 <- gather(weather,day,value,X1:X31,na.rm=T) # the 'X' colu are condensed into the 'day' and 'value' col
spread(data,key,value...) # long df to wide
separate(data,col,into)
unite(data,col_to_create,col1,col2,sep="")

# Working with dates
library(lubridate) # converts a varitety of date formats to one single format

# Type conversions
as.character()
as.numeric()
as.integer()
as.factor()
as.logical()

# String manipulation
library(stringr)
  # all functions take: 1)string of interest, 2)pattern of interest, 3)string to replace with
str_trim() # remove leading/trailing whitespace
str_pad() # can add padding zeros back in front!!
str_detect()
str_replace()
  # rearrange columns
df <- select(df, col1, col2, col3:col10)
  # apply function (as.numeric in example below) to a set of columns
df <- mutate_at(df,vars(col1:col10),funs(as.numeric))
  # rename columns
names(df) <- c(new_colname1,new_colname2,new_colname3)

# Missing and special data
any(is.na(df))
sum(is.na(df))
which(is.na(df$col))
complete.cases(df) # T/F for each row, if NAs are present
  df[complete.cases(df),] # keep only complete cases
na.omit(df) # remove rows with NAs
df$col[dff$col == ""] <- NA # replace all empty strings in col with NA

#########################################
####
#########################################
