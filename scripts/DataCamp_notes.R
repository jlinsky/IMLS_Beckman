################################################################################

### Author: Emily Beckman  ###  Date: 2020
# Notes from DataCamp courses

################################################################################
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

# CLEAN DATA: Observations in rows, variables in columns, each table represents
#             one type of observational unit

# Introduction to tidyr
gather(data,key,value...) # wide df to long
  #bmi_long <- gather(bmi, year, bmi_val, -Country)
  #weather2 <- gather(weather,day,value,X1:X31,na.rm=T) # the 'X' col are
  # condensed into the 'day' and 'value' col
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
  # all functions take: 1)string of interest, 2)pattern of interest,
  #                     3)string to replace with
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

################################################################################
#######################################
#### Visualizing Geospatial Data in R
#######################################

# Basic mapping with ggplot2 and ggmap
library(ggplot2)
library(ggmap)
  # NYC example
nyc <- c(lon = -74.0059, lat = 40.7128)
nyc_map <- get_map(location = nyc, zoom = 10) # 'zoom' takes 3 to 21
ggmap(nyc_map)
  # add points to map, can have color or size gradient based on variables
ggmap(myc) +
  geom_point(aes(lon, lat, color = year_built, size = bedrooms), data = sales)
  # more visual options
corvallis <- c(lon = -123.2620, lat = 44.5646)
corvallis_map_bw <- get_map(corvallis, zoom = 13)
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, color = year_built), data = sales)
  # simple black and white background
corvallis_map_bw <- get_map(corvallis, zoom = 13,source = "stamen",
  maptype = "toner")
ggmap(corvallis_map_bw) +
  geom_point(aes(lon, lat, color = year_built), data = sales)
  # create base_layer to give more flexibility with geom_point
ggmap(corvallis_map_bw,
  base_layer = ggplot(sales, aes(lon, lat))) +
  geom_point(aes(color = year_built))
  # gives you a bunch of little maps, one for each class !
ggmap(corvallis_map_bw, base_layer = ggplot(sales, aes(lon, lat))) +
  geom_point(aes(color = class)) +
  facet_wrap(~ class)
  # another way to get the little maps, with less code
qmplot(lon, lat, data = sales, geom = "point", color = bedrooms) +
  facet_wrap(~ month)

# Point and polygon data
library(sp)
  # take a look at the data
summary(polygons)
plot(polygons);
str(polygons,max.level=2)
  # access slots
x@slot_name # or...
slot(x, "slot_name")
  # look at specific item in slot
t <- x@polygons[[169]]
  # select specific element (Spatial___DataFrame) in overall object
  #    (e.g. countries of the world)
usa <- countries_spdf[169,]
  # two ways to show list of elements in column of data slot
str(countries_spdf@data) # look at data slot
countries_spdf$name # 1) pull out the name column using $
countries_spdf[["subregion"]] # 2) pull out subregion column using [[]]
    # example
in_asia <- countries_spdf$region == "Asia"
countries_spdf[in_asia, ]
  # tmap package: like ggplot2 but expects spatial object input
library(tmap)
  # basic example
t <- tm_shape(countries_spdf, projection = "robin") +
     tm_grid(n.x = 11, n.y = 11) +
     tm_fill(col = "population", style = "quantile")  +
     tm_borders(col = "burlywood4")
  # save your plot
tmap_save(t,"population.png") # static
tmap_save(t,"population.html") # interactive

# Raster data and color
library(raster)
  # look at structure
print(x)
str(x,max.level=2)
plot(x)
  # You can think of RasterLayer like a matrix, but RasterStack and
  #   RasterBrick objects are more like three dimensional arrays
pop_by_age[["under_1"]] # select one layer from RasterStack
  # plot raster
tm_shape(pop_by_age) +
  tm_raster(col="under_1")
  # another way to visualize rasters !
library(rasterVis)
levelplot(pop)
  # generating colors
library(RColorBrewer)
display.brewer.all()
library(viridisLite)
viridis(n = 9)
  # add colors from RColorBrewer package
ggplot(preds) +
  geom_tile(aes(lon, lat, fill = predicted_price), alpha = 0.8) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu"))
  # add colors from viridisLite package
ggplot(preds) +
  geom_tile(aes(lon, lat, fill = predicted_price), alpha = 0.8)  +
  scale_fill_gradientn(colors=viridis(9)) #colors=magma(9)
  # example using tmap package instead of ggplot2; "rev" reverses color order
tm_shape(prop_by_age) +
  tm_raster("age_18_24",palette=rev(mag)) +
  tm_legend(position = c("right", "bottom"))
# ggplot2 maps to CONTINUOUS GRADIENT while tmpa maps to DISCRETE of colors
library(classInt) # shortcuts for binning a variable
  # example
classIntervals(values,n=5,style="equal") # "quantile" or "pretty" or "fixed"
  # create histogram of proportions
hist(values(prop_by_age[["age_18_24"]]))
  # example maps
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = mag, style = "quantile") +
  tm_legend(position = c("right", "bottom"))
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = mag,
    style = "fixed", breaks = c(0.025, 0.05, 0.1, 0.2, 0.25, 0.3, 1))
tm_shape(migration) +
  tm_raster(palette=red_gray,style="fixed",
    breaks=c(-5e6, -5e3, -5e2, -5e1, 5e1, 5e2, 5e3, 5e6)) +
  tm_legend(outside = TRUE, outside.position = c("bottom"))
  # set sequence of intuative colors
levels(land_cover)
intuitive_cols <- c(
  "darkgreen",
  "darkolivegreen4",
  "goldenrod2",
  "seagreen",
  "wheat",
  "slategrey",
  "white",
  "lightskyblue1"
)
tm_shape(land_cover) +
  tm_raster(palette=intuitive_cols) +
  tm_legend(position = c("left","bottom"))

# Data import and projections
  # read in shapefile with readOGR()
neighborhoods <- readOGR("nynta_16c","nynta") # 1st arg =folder; 2nd arg =file
  # read in raster
income_grid <- raster("nyc_grid_data/m5602ahhi00.tif")
  # source of high resolution world maps: coastlines, states, populated places
library(rnaturalearth)
  # way to easily download and import shapefiles based on US Census geographies
library(tigris)
tracts(); states(); counties(); places()
nyc_tracts <- tracts(state="NY",county="New York",cb=T) # cb=lower res
  # change projection of one object to match another
neighborhoods <- rgdal::spTransform(neighborhoods,proj4string(nyc_tracts))
  # check for duplicates
any(duplicated(nyc_income$tract))
  # check that all elements of one are in another
all(nyc_tracts$TRACTCE %in% nyc_income$tract)
  # merging spatial polygon objects
nyc_tracts_merge <- sp::merge(nyc_tracts,nyc_income,
  by.x="TRACTCE",by.y="tract")
  # quick plot
tm_shape(nyc_tracts_merge) + tm_fill(col="estimate")
  # layering polygons on map
tm_shape(nyc_tracts_merge) +
  tm_fill(col = "estimate") +
  tm_shape(water) +
  tm_fill(col = "grey90") +
  tm_shape(neighborhoods) +
  tm_borders() +
  tm_text(text = "name",size=0.5)
  # subset polygon object to remove areas outside your focus

  # complete map example
m <- tm_shape(nyc_tracts_merge) +
  tm_fill(col = "estimate",
          title = "Median Income",
          palette = "Greens") +
  tm_borders(col = "grey60",
            lwd = 0.5) +
  tm_shape(water) +
  tm_fill(col = "grey90") +
  tm_shape(manhat_hoods) +
  tm_borders(col = "grey40",
            lwd = 2) +
  tm_text(text = "name", size = 0.5) +
  tm_credits("Source: ACS 2014 5-year Estimates, \n accessed via acs package",
            position = c("right", "bottom"))
tmap_save(m,"nyc_income_map.png",width = 4,height = 7)

################################################################################
########################################
#### Working with Data in the Tidyverse
########################################

# Explore your data
  # filter with dplyr
df %>% filter(is.na(df$col))
  # see all columns in a tibble
glimpse(df)
  # another way to summarize data; breaks down by variable type
library(skimr)
skim(df)
  # combining glimpse with other functions to see helpful values
df %>%
  arrange(col) %>%
  glimpse()
  # combining skim with other functions to summarize more
df %>%
  filter(!is.na(col)) %>%
  group_by(col) %>%
  skim()
  # distinct and count
  # count totals by piping twice
df %>%
  count(col1,col2) %>%
  count(col1,sort=T) # can use sort within the count() function!
  # make a bunch of bar charts, one for each facet
ggplot(df, aes(col)) +
    geom_bar() +
    facet_wrap(~col2)

# Tame your data
filter() # rows
select() # columns
mutate()
  # parsing and casting a column during read-in
df <- read_csv("test.csv",
        na = c("","NA","N/A"),
        col_types = cols(
          col1 = col_date(format = "%d %m %y"),
          col2 = col_number(),
          col3 = col_factor(levels=NULL)
        ))
  # easily recode variables!
df <- df %>%
  mutate(col1 = recode(col1,"text" = "text2",
                       "text3" = NA_character_)) # NA for character col
  # another recode example with .default and numeric values
df <- df %>%
  mutate(col_new = recode(col_old,'1' = 1,
                          .default = 0))
  # compare counts
df %>% count(col1 == "test", col2)
  # select also reorders columns; everything() keeps all cols
  # there are other "helper" functions that go inside select, such as
  #   ends_with, contains, etc...
  # can just add a "-" in front of specific columns you want to remove
df %>% select(col,everything(),-ends_with("test"))
  # format messy column names
library(janitor)
df <- df %>% clean_names("snake") # lots of options in clean_names
  # batch rename columns with select
df <- df %>% select(col, prefix, ends_with("end"))

# Tidy your data
  ## tidy data is usually LONG rather than WIDE; each variable is one column
  # reshape data using gather
gather(key="new_key_column",value="new_variable_column",col:col)
  # gather and plot example
tidy_ratings <- ratings %>%
	gather(key = "episode", value = "viewers_7day", -series,
           factor_key = TRUE, na.rm = TRUE) %>%
    arrange(series, episode) %>%
    mutate(episode_count = row_number())
ggplot(tidy_ratings, aes(x = episode_count,
                y = viewers_7day,
                fill = series)) + geom_col()

################################################################################
#########################
#### Introduction to Git
#########################



################################################################################
###########################################
#### String Manipulation with Stringr in R
###########################################

# RESOURCES TO LOOK AT FURTHER:
#   www.rdocumentation.org/packages/stringr
#   stringi
#   www.regular-expressions.info
#   Mastering Regular Expressions by Jeffrey Friedl
#   http://r4ds.had.co.nz/strings.html#matching-patterns-with- regular-expressions

## String basics

writeLines(list,sep="\n") # prints a list, with each element on new row
writeLines("\U1F30D") #prints a globe emoji!

format(x, digits=NUM,...) # format number as string; lots of "pretty" options
formatC() # more predictable version of format

# can use paste(), format(), and writeLines() to create a table of sorts
income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")
income <- c(72,1030,10292,1189192)
pretty_income <- format(income,digits=2,big.mark=",")
dollar_income <- paste("$",pretty_income,sep="")
formatted_names <- format(income_names,justify="right")
rows <- paste(formatted_names,dollar_income,sep="   ")
writeLines(rows)

# example with collapse and paste using vectors
my_toppings <- c("mushrooms","onions","tomatoes")
  # Paste "and " to last element only
my_toppings_and <- paste(c("","","and "),my_toppings,sep="")
  # Collapse with comma
these_toppings <- paste(my_toppings_and,collapse=", ")
  # Add rest of sentence: my_order
my_order <- paste("I want to order a pizza with ",these_toppings,".",sep="")
  # Order pizza with writeLines()
writeLines(my_order)

## Introduction to stringr

library(stringr)

str_c() # similar to paste0 but everything NA if one NA
str_replace_na() # replaces missing values with value of your choosing

# str_length() example
library(babynames)
library(dplyr)
babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
head(boy_names)
boy_length <- str_length(boy_names)
head(boy_length)
head(factor(boy_names))

# str_sub() example
  # Extract first letter from boy_names
boy_first_letter <- str_sub(boy_names,1,1)
table(boy_first_letter)
  # Extract the last letter in boy_names, then tabulate
boy_last_letter <- str_sub(boy_names,-1,-1)
table(boy_last_letter)

# str_detect() example
contains_zz <- str_detect(boy_names,"zz") # returns T/F
str(contains_zz)
sum(contains_zz)
boy_names[contains_zz]

# str_subset() example
starts_U <- str_subset(girl_names,"U")
str_subset(starts_U,"z") # can combine multiple to create complicated queries

# str_count() example
  # count number of "a"s
number_as <- str_count(girl_names,"a")
  # girl_names with more than 4 a's
girl_names[total_as > 4]

# str_split() example
chars <- c("Tom & Jerry", "Alvin & Simon & Theodore")
  # n lets you decide how many elements you want to split into
  # simplify = T will return a matrix instead of a list (this means all
  #   rows will have same number of elements also...)
str_split(chars, pattern = " & ", n = 2, simplify = T)
# another example
words <- "This is a sentence we are going to split and count."
words <- str_split(lines," ")
  # Number of words per line
lapply(words,length)
  # Number of characters in each word
word_lengths <- lapply(words,str_length)
  # Average word length per line
lapply(word_lengths,mean)

# str_replace() example
phone_numbers <- c("510-555-0123", "541-555-0167")
  # str_replace() only replaces first instance
str_replace(phone_numbers,"-"," ")
  # str_replace_all() replaces all instances
str_replace_all(phone_numbers,fixed("-")," ")

## Regular expressions

library(rebus)

# str_view() example, where you can see matches to pattern
# rebus package is used for the "START %R%" language for matching
x <- c("cat", "coat", "scotland", "tic toc")
str_view(x, pattern = START %R% "c")
  # another example
str_view(c("cat", "coat", "scotland", "tic toc"),
  pattern = "c" %R% ANY_CHAR %R% "t") # can add "match = T" to view only matches

# SUMMARY OF SOME ABOVE stringr FUNCTIONS
x <- c("cat", "coat", "scotland", "tic toc")
pattern <- "c" %R% ANY_CHAR %R% "t"
str_detect(x, pattern)
str_subset(x, pattern)
str_count(x, pattern)

# str_extract() example
pattern <- "q" %R% ANY_CHAR
part_with_q <- str_extract(boy_names,pattern) # returns pattern matches

# more rebus matching examples
  # Match with alternate endings
by_parts <- or("Je", "Geo") %R% "ff" %R% or("ry", "ery", "rey", "erey")
str_view(boy_names, pattern = by_parts, match = TRUE)
  # Match names that start with Cath or Kath
ckath <- START %R% or("C","K") %R% "ath"
str_view(girl_names, pattern = ckath, match = TRUE)

# using char_class() to match a variety of characters
  # match "a gr followed by, either an a or e, followed by a y"
x <- c("grey sky", "gray elephant")
str_view_all(x, pattern = "gr" %R% char_class("ae") %R% "y")

# using one_or_more(), zero_or_more(), optional(), and negated_char_class()
vowels <- char_class("aeiouAEIOU")
x <- c("ow", "ooh", "yeeeah!", "shh", "ya")
  # See words with only vowels
str_view(x,
  pattern = exactly(one_or_more(vowels)),
  match = TRUE)
  # See words with no vowels
not_vowels <- negated_char_class("aeiouAEIOU")
str_view(x,
  pattern = exactly(one_or_more(not_vowels)),
  match = TRUE)

# Example for pulling phone numbers from text!
contact <- c("Call me at 555-555-0191","123 Main St","(555) 555 0191",
  "Phone: 555.555.0191 Mobile: 555.555.0192")
  # Create pattern
three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")
phone_pattern <- optional(OPEN_PAREN) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  four_digits
  # Extract phone numbers
str_extract(contact,phone_pattern)
  # Extract ALL phone numbers
str_extract_all(contact,phone_pattern)
  # Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R%
           capture(three_digits) %R% zero_or_more(separator) %R%
           capture(four_digits)
  # Pull out the parts with str_match()
phone_numbers <- str_match(contact,pattern=phone_pattern)
  # Put them back together
str_c(
  "(",
  phone_numbers[,2],
  ") ",
  phone_numbers[,3],
  "-",
  phone_numbers[,4])
  # If you wanted to extract beyond the first phone number, e.g. The second
  #   phone number in the last string, you could use str_match_all(). But,
  #   like str_split() it will return a list with one component for each input
  #   string, and you'll need to use lapply() to handle the result.

# Example for pulling mult. patterns from text description
  # Create pattern
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")
gender <- optional(SPC) %R% or("M", "F")
  # Extract age, unit, gender from string such as:
    # "19YOM-SHOULDER STRAIN-WAS TACKLED" or "53 YO F TRIPPED ON CARPET"
str_extract(narratives,pattern=age %R% unit %R% gender)
  # Numeric ages, from previous step
ages_numeric <- as.numeric(str_extract(age_gender, age))
  # Extract units
time_units <- str_extract(age_gender,pattern=unit)
  # Extract first word character
time_units_clean <- str_extract(time_units,pattern = WRD)
  # Turn ages in months to years
ifelse(time_units_clean == "Y", ages_numeric, ages_numeric/12)

# "capture" specific parts of matched strings
hero_contacts <- c("(wolverine@xmen.com)","wonderwoman@justiceleague.org",
  "thor@avengers.com")
  # Pattern matching
email <- capture(one_or_more(WRD)) %R%
  "@" %R% capture(one_or_more(WRD)) %R%
  DOT %R% capture(one_or_more(WRD))
  # Pull out match and captures
email_parts <- str_match(hero_contacts,pattern=email)
email_parts
  # Save just one part of captured text
host <- email_parts[,3]
host

# Backreferencing
  # Names with a pair that reverses
pair_that_reverses <- capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1
  # Four letter palindrome names
four_letter_palindrome <- exactly(capture(LOWER) %R% capture(LOWER) %R% REF2
  %R% REF1)

# Replacing with regular expressions
  # Replace digits with "X"
str_replace(contact, DGT, "X")
  # Replace all digits with "X"
str_replace_all(contact, DGT, "X")
  # Replace all digits with different symbol
str_replace_all(contact, DGT, c("X", ".", "*", "_"))

# Replacing with backreferences
  # Build pattern to match words ending in "ING"
pattern <- one_or_more(WRD) %R% "ING"
str_view(narratives, pattern)
  # Test replacement
str_replace(narratives, capture(pattern),
  str_c("CARELESSLY", REF1, sep = " "))
  # Select one adverb per narrative
adverbs_10 <- sample(adverbs, 10)
  # Replace "***ing" with "adverb ***ly"
str_replace(narratives,
  capture(pattern),
  str_c(adverbs_10, REF1, sep = " "))

# get unicode value
as.hexcode(utf8ToInt("×"))
# The stringi package that stringr is built on contains functions for
#   converting between the two forms. stri_trans_nfc() composes characters
#   with combining accents into a single character. stri_trans_nfd()
#   decomposes character with accents into separate letter and accent
#   characters. You can see how the characters differ by looking at the
#   hexadecimal codes. For example:
as.hexmode(utf8ToInt(stri_trans_nfd("\u00e8")))
as.hexmode(utf8ToInt(stri_trans_nfc("\u0065\u0300")))
# In Unicode, an accent is known as a diacritic Unicode Property, and you can
#   match it using the rebus value UP_DIACRITIC

# Unicode example
  # Names with builtin accents
(tay_son_builtin <- c(
  "Nguy\u1ec5n Nh\u1ea1c",
  "Nguy\u1ec5n Hu\u1ec7",
  "Nguy\u1ec5n Quang To\u1ea3n"
))
  # Convert to separate accents
tay_son_separate <- stri_trans_nfd(tay_son_builtin)
  # Verify that the string prints the same
tay_son_separate
  # Match all accents
str_view_all(tay_son_separate,pattern=UP_DIACRITIC)
 # View all the characters in tay_son_separate
str_view_all(tay_son_separate,ANY_CHAR)
  # View all the graphemes in tay_son_separate
str_view_all(tay_son_separate,GRAPHEME)
  # Combine the diacritics with their letters
tay_son_builtin <- stri_trans_nfc(tay_son_separate)
tay_son_builtin

# CASE STUDY
# text file: http://s3.amazonaws.com/assets.datacamp.com/production/course_2922/datasets/importance-of-being-earnest.txt
  # Read play in using stri_read_lines()
earnest <- stri_read_lines(earnest_file)
  # Detect start and end lines
start <- str_which(earnest, fixed("START OF THE PROJECT"))
end <- str_which(earnest, fixed("END OF THE PROJECT"))
  # Get rid of gutenberg intro text
earnest_sub  <- earnest[(start + 1):(end - 1)]
  # Detect first act
lines_start <- str_which(earnest_sub, fixed("FIRST ACT"))
  # Set up index
intro_line_index <- 1:(lines_start - 1)
  # Split play into intro and play
intro_text <- earnest_sub[intro_line_index]
play_text <- earnest_sub[-intro_line_index]
  # Take a look at the first 20 lines
writeLines(play_text[1:20])
  # Get rid of empty strings
empty <- stri_isempty(play_text)
play_lines <- play_text[!empty
  # Pattern to attempt pulling character names
pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT
  # Get subset of lines that match
lines <- str_subset(play_lines,pattern_2)
  # Extract match from lines
who <- str_extract(lines,pattern_2)
  # Let's see what we have
unique(who)
  # Create vector of characters
characters <- c("Algernon", "Jack", "Lane", "Cecily", "Gwendolen", "Chasuble",
  "Merriman", "Lady Bracknell", "Miss Prism")
  # Match start, then character name, then .
pattern_3 <- START %R% or1(characters) %R% DOT
  # View matches of pattern_3
str_view(play_lines,pattern_3,match=T)
    # View non-matches of pattern_3
str_view(play_lines,pattern_3,match=F)
  # Pull out matches
lines <- str_subset(play_lines,pattern_3)
  # Extract match from lines
who <- str_extract(lines,pattern_3)
# Let's see what we have
unique(who)
  # Count lines per character
table(who)

# Rebus function called whole_word(). The argument to whole_word() will only
#   match if it occurs as a word on its own, for example whole_word("cat")
#   will match cat in "The cat " and "cat." but not in "caterpillar"

# case sensitivity: can either make everything one case or ignore case
str_to_upper()
str_to_lower()
str_to_title()
stri_trans_totitle() # type = "word" (default) or "sentence"
  # Construct case insensitive pattern
trip_pattern <- regex("TRIP", ignore_case = TRUE)
  # View case insensitive matches to "TRIP"
str_view(catcidents, pattern = trip_pattern, match = TRUE)
  # Get subset of matches
trip <- str_subset(catcidents,trip_pattern)
  # Extract matches
str_extract(trip,trip_pattern)

################################################################################
#################################
#### Working with Web Data in R
#################################

## Downloading Files and Using API Clients

# Download the file from web with download.file()
download.file(url = csv_url, destfile = "feed_data.csv")
# Read it in with read.csv()
csv_data <- read.csv("feed_data.csv")

# Save R objects in an R-specific file format, with the data structure intact
# Save it to disk with saveRDS()
saveRDS(object = csv_data, file = "modified_feed_data.RDS")
# Read it back in with readRDS()
modified_feed_data <- readRDS(file = "modified_feed_data.RDS")

# Websites let you and I interpret and interface with a server,
#   APIs allow computers to do the same.
# Search CRAN [dataset name] to see if an R package exists to work with API

## Using httr to interact with APIs directly

# Load the httr package
library(httr)
# Make a GET request to http://httpbin.org/get
get_result <- GET("http://httpbin.org/get")
# Make a POST request to http://httpbin.org/post with the body "this is a test"
post_result <- POST(url = "http://httpbin.org/post",body = "this is a test")

# Make a GET request to url and save the results
pageview_response <- GET(url)
# Call content() to retrieve the data the server sent back
pageview_data <- content(pageview_response)

# Make the GET request
request_result <- GET(fake_url)
# Check request_result
if(http_error(request_result)){
	warning("The request failed")
} else {
	content(request_result)
}

# Construct a directory-based API URL to `http://swapi.co/api`,
# looking for person `1` in `people`
directory_url <- paste("http://swapi.co/api", "people", "1", sep = "/")

# Create list with nationality and country elements
query_params <- list(nationality = "americans",
    country = "antigua")
# Make parameter-based call to httpbin, with query_params
parameter_response <- GET("https://httpbin.org/get", query = query_params)

# Informative user-agents are a good way of being respectful of the developers
#  running the API. Try to include your email and URL for the project the code is a part of
GET("http://url.goes.here/", user_agent("somefakeemail@domain.com http://project.website"))

# The next stage of respectful API usage is rate-limiting: making sure you only make a certain number of requests to the server in a given time period.
# Construct a vector of 2 URLs
urls <- c("http://httpbin.org/status/404","http://httpbin.org/status/301")
for(url in urls){
    # Send a GET request to url
    result <- GET(url)
    # Delay for 5 seconds between requests
    Sys.sleep(5)
}

# PUT IT ALL TOGETHER
get_pageviews <- function(article_title){
  url <- paste(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents",
    article_title,
    "daily/2015100100/2015103100",
    sep = "/"
  )
  response <- GET(url, user_agent("my@email.com this is a test"))
  # Is there an HTTP error?
  if(http_error(response)){
    # Throw an R error
    stop("the request failed")
  }
  # Return the response's content
  content(response)
}
