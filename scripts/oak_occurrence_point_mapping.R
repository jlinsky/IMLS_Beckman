short <- read.csv("./Desktop/work/all_occ_compiled_unq_all_exsitu_short.csv")
update <- read.csv("./Desktop/work/oak_dist_compiled_sep2018__11.20_update.csv")
dec3_2 <- read.csv("./Desktop/work/occurrence_compiled_refined_dec3_2.csv")
exsitu <- read.csv("./Desktop/work/all_occ_compiled_unq_all_exsitu.csv")

df <- short
df <- update
df <- dec3_2
df <- exsitu

# you can view the final product of the following code block hhere:
#   https://leopardshark.com/esb/oak_test_map.html

# download CSV of oak distribution points (you need permission) and place
#   the file in the working directory you specified above:
#   https://drive.google.com/open?id=1hVJIdO49eZg_ggJnsXeLFkGGnxMWl-Oy

# read in CSV of distribution points
#df <- read.csv("all_occ_compiled_unq_all_exsitu_short.csv",as.is=T)
  str(df) # take a look at the data structure

# reorder data so the ex situ points are at the top (easier to view in map)
df <- setorder(df, -dataset)

# create list of unique species names; this is the grouping used to toggle
sp <- unique(df$species)

# create color scheme
# take a look at options for color pre-made schemes, if needed, by running
#   "display.brewer.all()"
# this scheme is for categorical (factor) variables
palette_fix <- colorFactor(palette = c("#2cb42c","purple"),
                       levels = c("exsitu","insitu"))
# this scheme is for continuous (numeric) variables
#palette_cont <- colorNumeric(palette = "RdBu",
#                             domain = c(1:20))

# write text that will be map's title
title <- "In situ distribution and ex situ source localities for native U.S.
  oaks of conservation concern"

# create function to plot the map
map_layers <- function(){
  map <- leaflet() %>%
  # add base map; you can add as many separate provider tiles as you wish
  # see all provider tiles (base maps) by running "names(providers)"
  addProviderTiles(
    "CartoDB.PositronNoLabels",
    # you can change the "maxZoom" option depending on how far you want
    #   viewers to be able to zoom (addresses privacy issues)
    options = providerTileOptions(maxZoom = 10))
  # loop through all groups and add each toggle layer one at a time
  for (i in 1:length(sp)) {
    map <- map %>%
    addCircleMarkers(
      data = df %>%
        # change 'species' to the name of the column you groupped
        filter(species == sp[i]),
        group = sp[i],
        lng = ~decimalLongitude, lat = ~decimalLatitude,
        radius = 4,
        # change 'dataset' to the name of the column you want to define
        #   the color of the points
        color = ~palette_fix(dataset),
        fillOpacity = 1,
        stroke = F
    )
  }
  # add layer control
  map %>%
  # add title
  addControl(title, position = "topright") %>%
  # add legend
  addLegend(
    labels = c("Yes","No"),
    colors = c("#2cb42c","purple"),
    title = "Represented ex situ?",
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

# save map
map_final <- map_layers()
htmlwidgets::saveWidget(map_final, file = "oak_test_map.html")
