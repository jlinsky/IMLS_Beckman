### Author: Emily Beckman  ###  Date: 1/7/2020                                |

### DESCRIPTION:
  #

### INPUTS:
  #

### OUTPUTS:
  #


#################
### LIBRARIES ###
#################

library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(data.table)

##############
### SCRIPT ###
##############

setwd("./Desktop")

# read in CSV of distribution points
df <- read.csv("all_occ_compiled_unq_all_exsitu_AJOENSIS.csv")

# take a look at options for color schemes, if needed
#display.brewer.all()

# create color scheme
  # this one is for categorical (factor) variables
palette_fix <- colorFactor(palette = c("#2cb42c","purple"),
                       levels = c("exsitu","insitu"))
  # this one is for continuous (numeric) variables
#palette_cont <- colorNumeric(palette = "RdBu",
#                             domain = c(1:20))

# take a look at options for basemaps ("provider tiles")
#names(providers)

# create a map
map <- df %>%
  leaflet() %>%
  # you can add as many separate provider tiles as you wish:
  addProviderTiles(
    "CartoDB.PositronNoLabels",
    options = providerTileOptions(maxZoom = 9),
    group = "Base map") %>%
  # the "popup" is the text that appears when you click a point
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    popup = ~paste0("<b>","<i>",species,"</b>","</i>","<br/>",dataset),
    color = ~palette_fix(dataset), #"#2cb42c"
    radius = 3,
    group = "Q. ajoensis"
  ) %>%
  # this creates a little box where you can click layers on and off
  addLayersControl(
    baseGroups = c("Base map"),
    overlayGroups = c("Q. ajoensis"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  # something isn't quite right here
  addLegend(
    labels = c("Ex situ","In situ"),
    colors = ~palette_fix(dataset),
    title = "Legend",
    position = "topright",
    opacity = 0.75)
# view your map!
map

### Map with toggles for species ###

# load data
df <- read.csv("all_occ_compiled_unq_all_exsitu_short.csv",as.is=T)
  str(df)

# order data so ex situ points are at the top
df <- setorder(df, -dataset)
  str(df)

# create list of unique species names (grouping to use for toggle)
sp <- unique(df$species)

# create title text
title <- "In situ distribution and ex situ source localities for native U.S. oaks of conservation concern"

# Example for creating a grouping variable for number intervals
#quakes <- quakes %>%
#  mutate(groups = case_when(
#    stations < 30 ~ 1,
#    stations < 50 ~ 2,
#    TRUE ~ 3
#  ))

# function to plot a map with layer selection
map_layers <- function(){
  # base map
  map <- leaflet() %>%
    addProviderTiles(
      "CartoDB.PositronNoLabels",
      options = providerTileOptions(maxZoom = 10))
  # loop through all groups and add a layer one at a time
  for (i in 1:length(sp)) {
    map <- map %>%
      addCircleMarkers(
        data = df %>%
          filter(species == sp[i]),
          group = sp[i],
          lng = ~longitude, lat = ~latitude,
          radius = 4,
          color = ~palette_fix(dataset),
          fillOpacity = 1,
          stroke = F
      )
  }
  # create layer control
  map %>%
    # add title
    addControl(title, position = "topright") %>%
    # add legend
    addLegend(
      labels = c("Ex situ","In situ"),
      colors = c("#2cb42c","purple"),
      title = "Legend",
      position = "topright",
      opacity = 0.75) %>%
    # add layer control panel
    addLayersControl(
      overlayGroups = sp,
      options = layersControlOptions(collapsed = F)) %>%
    hideGroup(sp[2:length(sp)]) # hide all groups except the 1st one
}

# view map
map_layers()



### EXAMPLE ###

# load data
data("quakes")

# create a grouping variable
quakes <- quakes %>%
  mutate(groups = case_when(
    stations < 30 ~ 1,
    stations < 50 ~ 2,
    TRUE ~ 3
  ))

# function to plot a map with layer selection
map_layers <- function(){
  # number of groups
  k <- n_distinct(quakes$groups)
  # base map
  map <- leaflet() %>%
    addProviderTiles(
      "CartoDB.PositronNoLabels",
      options = providerTileOptions(maxZoom = 9))
  # loop through all groups and add a layer one at a time
  for (i in 1:k) {
    map <- map %>%
      addCircleMarkers(
        data = quakes %>% filter(groups == i),
          group = as.character(i),
          lng = ~long, lat = ~lat, radius = 1
      )
  }
  # create layer control
  map %>%
    addLayersControl(
      overlayGroups = c(1:k),
      options = layersControlOptions(collapsed = F)) %>%
    hideGroup(as.character(c(2:k))) # hide all groups except the 1st one
}

# view map
map_layers()


map2 <- map %>%
  clearBounds() %>%
  clearMarkers()

# <br/> = line break
# <i></i> = italics
