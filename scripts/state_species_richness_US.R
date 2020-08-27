################################################################################

### Author: Emily Beckman  ### Date: 07/23/2020

### DESCRIPTION:
  # Working script to create species richness maps based on geopolitical
	#		boundaries.

### DATA IN:
	#

### DATA OUT:
	#


################################################################################
# Load libraries
################################################################################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal",#"knitr",
	"RColorBrewer","tidyverse","ggplot2","ggmap","tmap","maptools")
#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Read in data
################################################################################

setwd("./Desktop")

# READ IN POLYGONS

# read in shapefiles
	# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
states_shp <- readOGR("cb_2018_us_state_5m/cb_2018_us_state_5m.shp")
	# https://www.arcgis.com/home/item.html?id=2ca75003ef9d477fb22db19832c9554f
world_shp <- readOGR("countries_shp/countries.shp")
	# https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/pad-us-data-download?qt-science_center_objects=0#qt-science_center_objects
PA_d <- readOGR("PADUS2_0_Shapefiles/PADUS2_0Designation.shp")
PA_e <- readOGR("PADUS2_0_Shapefiles/PADUS2_0Easement.shp")
PA_f <- readOGR("PADUS2_0_Shapefiles/PADUS2_0Fee.shp")
#PA_m <- readOGR("PADUS2_0_Shapefiles/PADUS2_0Marine.shp")
#PA_p <- readOGR("PADUS2_0_Shapefiles/PADUS2_0Proclamation.shp")

#PA_data <- list(PA_d,PA_e,PA_f,PA_m,PA_p)
#PA_all <- Reduce(merge,PA_data)

#PA <- readOGR("WDPA_Jul2020-shapefile/WDPA_Jul2020-shapefile0/WDPA_Jul2020-shapefile-polygons.shp")

# READ IN SPECIES RICHNESS DATA

# read in distribution information
richness <- read.csv("US_state_richness_trees_shrubs.csv", as.is=T,
	na.strings=c("","NA"))
state_richness <- richness #%>%
	#filter(STUSPS != "PR" & STUSPS != "VI")
country_richness <- richness %>%
	filter(STUSPS == "PR" | STUSPS == "VI") %>%
	rename("ISO2" = "STUSPS")

# JOIN STATE/COUNTRY POLYGONS AND RICHNESS DATA

states_joined <- merge(states_shp,state_richness)
countries_joined <- merge(world_shp,country_richness)
# select only countries of interest
select_countries <- c("Virgin Islands")
countries_joined <- countries_joined[countries_joined@data$NAME %in%
	select_countries,]
head(countries_joined)

# CLIP PROTECTED AREA SHAPEFILE TO STATE/COUNTRY ONLY

#PA_clip <- gIntersection(states_joined,PA,byid = T,drop_lower_td = T)
#PA_simple <- gSimplify(PA, tol = 5, topologyPreserve=T)
#PA_clip_sp <- as(PA_clip, "SpatialPolygonsDataFrame")
#writeOGR(PA_clip_sp, dsn = '.', layer = 'US_CLIP_WDPA_Jul2020-shapefile0.shp',
#	driver = "ESRI Shapefile")

#coord <- coordinates(PA)
#ID <- cut(coord[,1], quantile(coord[,1]), include.lowest=TRUE)
#PA_diss <- unionSpatialPolygons(PA,ID)
proj <- states_joined@proj4string
poly_to_raster <- function(poly){
	poly_proj <- spTransform(poly,proj)
	blank_raster <- raster(nrow=800, ncol=3200, extent(states_joined))
	poly_raster <- rasterize(poly_proj, blank_raster, field="GAP_Sts")
	plot(poly_raster, legend=FALSE)
	projection(poly_raster) <- proj
	poly_raster_clip <- mask(poly_raster, states_joined)
	#leaflet() %>% addTiles() %>% addRasterImage(poly_raster_clip)
	return(poly_raster_clip)
}
#PA_raster_clip2 <- poly_to_raster(PA_all)
PA_d_raster <- poly_to_raster(PA_d)
PA_e_raster <- poly_to_raster(PA_e)
PA_f_raster <- poly_to_raster(PA_f)
#PA_raster <- rasterize(PA, blank_raster, field="GAP_Sts")
#PA_raster[!(is.na(PA_raster))] <- 1


################################################################################
# Map
################################################################################

# function to create color palettes
create_pal <- function(spdf_col,palette,rev){
	pal <- colorBin(palette = palette, bins = bins,
		domain = spdf_col, reverse = rev, na.color = "transparent")
	return(pal)
}
display.brewer.all()

# create color bins and labels, then color palettes
	# palette 1
bins <- c(1,50,100,150,200,250,300,350,400,450,500,Inf)
labels <- c("1-49","50-99","100-149","150-199","200-249","250-299","300-349",
	"350-399","400-449","450-499","500+")
pal_tree <- create_pal(states_joined@data$Num_tree_sp,"Spectral",T) #YlOrRd
pal_shrub <- create_pal(states_joined@data$Num_shrub_sp,"Spectral",T) #YlOrRd

#bins <- c(1,100,200,300,400,500,Inf)
#labels_tree <- c("1-99","100-199","200-299","300-399","400-499","500+")
#pal_tree <- create_pal(states_joined@data$Num_tree_sp,"YlOrRd")
	# palette 2
bins <- c(1,100,200,300,400,500,600,700,800,Inf)
labels_woody <- c("1-99","100-199","200-299","300-399","400-499","500-599","600-699",
	"700-799","800+")
pal_woody <- create_pal(states_joined@data$Num_woody_sp,"Spectral",T)

# function to create maps
map.richness <- function(boundary_poly,boundary_richness,my_palette,labels,
	legend_txt){
	map <- leaflet() %>%
		addProviderTiles("CartoDB.PositronNoLabels",
			options = providerTileOptions(maxZoom = 10)) %>%
		#addRasterImage(PA,
		#	opacity = 1,
		#	color = "#137d2f") %>%
		addPolygons(data = boundary_poly,
			label = ~NAME,
			color = "black",
			weight = 2,
			opacity = 0.6,
			fillColor = ~my_palette(boundary_richness),
			fillOpacity = 0.8) %>%
		#addPolygons(
		#	data = national,
		#	label = ~NAME,
		#	color = "black",
		#	weight = 2,
		#	opacity = 0.6,
		#	fillColor = ~my_palette(national_richness),
		#	fillOpacity = 0.7) %>%
		#addControl(
		#	title,
		#	position = "topright") %>%
		addLegend(pal = my_palette,
			values = boundary_richness,
			opacity = 0.8,
			title = legend_txt,
			labFormat = function(type, cuts, p) {paste0(labels)},
			position = "bottomright")
	return(map)
}

tree <- map.richness(states_joined,states_joined@data$Num_tree_sp,
	pal_tree,labels,"Number of native <br/> tree species")
htmlwidgets::saveWidget(tree, file = "US_tree_heatmap.html")
tree

shrub <- map.richness(states_joined,states_joined@data$Num_shrub_sp,
	pal_shrub,labels,"Number of native <br/> shrub species")
htmlwidgets::saveWidget(shrub, file = "US_shurb_heatmap.html")
shrub

woody <- map.richness(states_joined,states_joined@data$Num_woody_sp,
	pal_woody,labels_woody,"Number of native <br/> woody plant species")
htmlwidgets::saveWidget(woody, file = "US_woody_heatmap.html")
woody







# select only countries of interest
select_countries <- c("Mexico","Guatemala","Belize","El Salvador","Honduras",
	"Nicaragua","Costa Rica","Panama")
target_countries <- world_country_shp[world_country_shp@data$COUNTRY %in%
	select_countries,]
head(target_countries)

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
