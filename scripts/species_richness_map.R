### Author: Emily Beckman  ###  Date: 05/14/2020

### DESCRIPTION:
  # Working script to create species richness maps based on geopolitical
	#		boundaries.

### INPUTS:
	#

### OUTPUTS:
	#

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal",#"knitr",
	"RColorBrewer","tidyverse")

#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

#################
### FUNCTIONS ###
#################


################################################################################
# A) Read in data
################################################################################

setwd("./Desktop")
#setwd("./../..")
#setwd("/Volumes/GoogleDrive/My Drive/Q_havardii_buffer_test")

# READ IN POLYGONS

# read in shapefiles
	# https://www.arcgis.com/home/item.html?id=ac9041c51b5c49c683fbfec61dc03ba8
mx_states_shp <- readOGR("mexstates/mexstates.shp")
	# https://www.arcgis.com/home/item.html?id=2ca75003ef9d477fb22db19832c9554f
world_country_shp <- readOGR("countries_shp/countries.shp")

# select only countries of interest
select_countries <- c("Mexico","Guatemala","Belize","El Salvador","Honduras",
	"Nicaragua","Costa Rica","Panama")
target_countries <- world_country_shp[world_country_shp@data$COUNTRY %in%
	select_countries,]
head(target_countries)

# READ IN DISTRIBUTION AND GARDEN LOCATION SPREADSHEETS

# read in country/state distribution information
dist <- read.csv("Quercus in Mesoamerica - heatmap_data.csv", as.is=T,
	na.strings=c("","NA"), colClasses="character")
	# remove accents
dist$MX_states <- stringi::stri_trans_general(
  dist$MX_states, "Latin-ASCII")

# read in garden location points
mx_gardens <- read.csv("Quercus in Mesoamerica - mexico_gardens.csv", as.is=T,
	na.strings=c("","NA"))

# CALCULATE RICHNESS

## Countries

# function to create richness table and join to polygon data
richness.poly.countries <- function(df,polygons){
	l <- sapply(df$RL_countries,function(x) str_count(x, pattern = ","))
	COUNTRY <- str_split_fixed(df$RL_countries, ", ", n = (max(l)+1))
	richness <- as.data.frame(table(COUNTRY))
	richness <- richness[-1,]
	print(richness)
	merged <- merge(polygons,richness)
	merged@data$Freq[which(is.na(merged@data$Freq))] <- 0
	return(merged)
}
# find country richness
map_countries <- richness.poly.countries(dist,target_countries)

## make map
# create color bins and labels
bins <- c(1,11,21,31,51,71,100,Inf)
labels <- c("1-10","11-20","21-30","31-50","51-70","71-99","100+")
# create color palette
palette_country <- colorBin(palette = "YlOrRd", bins = bins,
	domain = map_countries@data$Freq, reverse = F, na.color = "transparent")
# map
map <- leaflet() %>%
	addProviderTiles(
		"CartoDB.PositronNoLabels",
		options = providerTileOptions(maxZoom = 10)) %>%
	addPolygons(
		data = map_countries,
		label = ~COUNTRY,
		color = "grey",
		weight = 2,
		opacity = 0.5,
		fillColor = ~palette_country(map_countries@data$Freq),
		fillOpacity = 0.8) %>%
	#addControl(
	#	title,
	#	position = "topright") %>%
	addLegend(
		pal = palette_country,
		values = map_countries@data$Freq,
		opacity = 0.7,
		title = paste0("Number of native","<br/>","oak species"),
		labFormat = function(type, cuts, p) {paste0(labels)},
		position = "bottomright") %>%
	leafem::addStaticLabels(
		.,
		data = map_countries,
		label = map_countries@data$Freq,
		style = list("font-weight"="bold","font-size"="14px"))
map

## States

# function to create richness table and join to polygon data
richness.poly.states <- function(df,polygons){
	l <- sapply(df$MX_states,function(x) str_count(x, pattern = ","))
	ADMIN_NAME <- str_split_fixed(df$MX_states, ", ", n = (max(l)+1))
	richness <- as.data.frame(table(ADMIN_NAME))
	richness <- richness[-1,]
  richness <- richness %>%
	  mutate(ADMIN_NAME = recode(ADMIN_NAME,
			"Mexico Distrito Federal" = "Distrito Federal",
			"Mexico State" = "Mexico"))
	print(richness)
	merged <- merge(polygons,richness)
	merged@data$Freq[which(is.na(merged@data$Freq))] <- 0
	return(merged)
}
# find state richness for all species
state_dist <- dist %>% filter(!is.na(MX_states))
map_states <- richness.poly.states(state_dist,mx_states_shp)
# find state richness for THREATENED species
state_dist_th <- dist %>% filter(!is.na(MX_states) & Threat=="Threatened")
map_states_th <- richness.poly.states(state_dist_th,mx_states_shp)
# find state richness for ALL POSSIBLY THREATENED species(includes NT and DD)
state_dist_pth <- dist %>% filter(!is.na(MX_states) & (Threat=="Threatened" |
	Threat=="Possibly Threatened"))
map_states_pth <- richness.poly.states(state_dist_pth,mx_states_shp)

# map function
map.state.richness <- function(shapes,my_palette,labels,legend_txt1,
	legend_txt2){
	map <- leaflet() %>%
		addProviderTiles(
			"CartoDB.PositronNoLabels",
			options = providerTileOptions(maxZoom = 10)) %>%
		addPolygons(
			data = shapes,
			label = ~ADMIN_NAME,
			color = "grey",
			weight = 2,
			opacity = 0.5,
			fillColor = ~my_palette(shapes@data$Freq),
			fillOpacity = 0.8) %>%
		addCircleMarkers(
			data = mx_gardens,
			lat = ~lat, lng = ~long,
			label = ~garden,
			color = "black",
			fillColor = "#0fb909",
			radius = 3,
			weight = 2,
			stroke = T,
			fillOpacity = 1) %>%
		#addControl(
		#	title,
		#	position = "topright") %>%
  	addLegend(
    	labels = legend_txt2,
    	colors = "#0fb909; width: 10px; height: 10px; border-radius: 50%",
    	position = "bottomright",
    	opacity = 1) %>%
		addLegend(
			pal = my_palette,
			values = shapes@data$Freq,
			opacity = 0.7,
			title = legend_txt1,
			labFormat = function(type, cuts, p) {paste0(labels)},
			position = "bottomright")
		#	%>%
		#leafem::addStaticLabels(
		#	.,
		#	data = shapes,
		#	label = shapes@data$Freq,
		#	style = list("font-weight"="bold","font-size"="14px"))
}

# ALL SPECIES
#legend_txt1 <- "Number of native oak species"
#legend_txt2 <- "Botanic gardens"
legend_txt1 <- "Número de especies <br/> nativas de roble"
legend_txt2 <- "Jardines Botanicos"
bins <- c(0,1,11,16,21,26,31,36,41,46,50,Inf)
label_state <- c("0","1-10","11-15","16-20","21-25","26-30","31-35","36-40",
	"41-45","46-49","50+")
palette_state <- colorBin(palette = "RdYlBu", bins = bins,
	domain = map_states@data$Freq, reverse = T, na.color = "transparent")
map <- map.state.richness(map_states,palette_state,label_state,legend_txt1,
	legend_txt2); map

# THREATENED
legend_txt1 <- "Number of native <br/> threatened oak <br/> species"
legend_txt2 <- "Botanic gardens"
legend_txt1 <- "Número de especies <br/> de robles nativos <br/> amenazados"
legend_txt2 <- "Jardines Botanicos"
bins <- c(0,1,2,3,4,5,6,7,8,9,10)
label_state <- c("0","1","2","3","4","5","6","7","8","9")
palette_state <- colorBin(palette = "YlOrRd", bins = bins,
	domain = map_states_th@data$Freq, reverse = F, na.color = "transparent")
map <- map.state.richness(map_states_th,palette_state,label_state,legend_txt1,
	legend_txt2); map

# POSSIBLY THREATENED
legend_txt1 <- "Number of native <br/> likely threatened <br/> oak species"
bins <- c(0,1,3,6,9,12,15,19,21,Inf)
label_state <- c("0","1-2","3-5","6-8","9-11","12-14","15-17","18-20","21+")
palette_state <- colorBin(palette = "YlOrRd", bins = bins,
	domain = map_states_th@data$Freq, reverse = F, na.color = "transparent")
map <- map.state.richness(map_states_pth,palette_state,label_state,legend_txt1,
	legend_txt2); map
