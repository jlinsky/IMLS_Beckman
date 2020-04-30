### Author: Emily Beckman  ###  Date: 2020                                     |
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
corvallis_map_bw <- get_map(corvallis, zoom = 13,source = "stamen", maptype = "toner")
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
  #
