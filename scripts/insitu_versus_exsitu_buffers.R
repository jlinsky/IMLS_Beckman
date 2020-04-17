

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr")

#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

#################
### FUNCTIONS ###
#################

# function to create buffers around spatial points, for viewing with leaflet
view.buffers <- function(latlong,df,radius){
	# turn occurrence point data into a SpatialPointsDataFrame
	wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
		+no_defs +towgs84=0,0,0")
	wgs_df <- SpatialPointsDataFrame(latlong, df, proj4string = wgs.proj)
	# place buffer around each point
	buffer_wgs <- buffer(wgs_df,width=radius,dissolve=T)
	# return buffer polygons
	return(buffer_wgs)
}

# function to create buffers around spatial points and calculate area
calc.buffer.area <- function(latlong,df,radius){
	# turn occurrence point data into a SpatialPointsDataFrame
	wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
		+no_defs +towgs84=0,0,0")
	wgs_df <- SpatialPointsDataFrame(latlong, df, proj4string = wgs.proj)
	# reproject SpatialPointsDataFrame to projection with meters as unit
	aea.proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
		+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
	aea_df <- spTransform(wgs_df,aea.proj)
	# place buffer around each point
	buffer_aea <- buffer(aea_df,width=radius,dissolve=T)
	# calculate buffer area
	buff_area <- buffer_aea@polygons[[1]]@area/1000000
	print(paste("Area covered by the buffers:", round(buff_area,2), "km²"))
	# return buffer polygons
	return(buff_area)
}

# create map to visualize buffer and point data
map.buffers <- function(insitu_coords,insitu,exsitu_coords,exsitu,title,radius){
	map <- leaflet() %>%
		addProviderTiles("CartoDB.PositronNoLabels",
  								   options = providerTileOptions(maxZoom = 10)) %>%
		addPolygons(data = view.buffers(insitu_coords,insitu,radius),
								smoothFactor = 0.5, weight = 2, color = "red") %>%
		addPolygons(data = view.buffers(exsitu_coords,exsitu,radius),
								smoothFactor = 0.5, weight = 2, color = "blue") %>%
  	addCircleMarkers(data = insitu,
									   lng = ~Longitude, lat = ~Latitude,
    					 		 	 popup = ~paste("In situ:", Pop),
      						 	 radius = 4, fillOpacity = 0.7, stroke = F,
								 	   color = "red") %>%
  	addCircleMarkers(data = exsitu,
									 	 lng = ~long, lat = ~lat,
									 	 popup = ~paste("Ex situ institution:",institution,"<br/>",
									 								  "Lat-long source:",gps_det,"<br/>",
																	  "Collection year:",aqu_year),
      						 	 radius = 4, fillOpacity = 0.7, stroke = F,
								 	 	 color = "blue") %>%
  	addControl(title, position = "topright") %>%
		addControl("Click on points to see more information",
							 position = "topleft") %>%
		addLegend(labels = c("In situ","Ex situ"),
    					colors = c("red","blue"),
    					title = "Key",
    					position = "topright",
    					opacity = 0.75)
	return(map)
}

################################################################################
# A) Read in data
################################################################################

setwd("Desktop")

# read in data
insitu <- read.csv("BeckHob_QHOccur_Vetted.csv", as.is=T, na.strings=c("","NA"))
	# round coordinates
	insitu$Longitude <- round(insitu$Longitude,5)
	insitu$Latitude <- round(insitu$Latitude,5)
exsitu <- read.csv("havardii_exsitu_2017_AllUSSPRecords.csv", as.is=T,
	na.strings=c("","NA"))
	exsitu$long <- round(exsitu$long,5)
	exsitu$lat <- round(exsitu$lat,5)

# be sure all exsitu points are included in the insitu data
exsitu_add <- distinct(cbind(exsitu[,c(8,7)]))
names(exsitu_add) <- c("Longitude","Latitude")
insitu <- full_join(insitu,exsitu_add)
insitu[which(is.na(insitu$Pop)),]$Pop <- "exsitu_survey_2017"

# recode gps determination column in exsitu data
exsitu <- exsitu %>%
  mutate(gps_det = recode(gps_det,
         "G" = "Provided by institution",
         "L" = "Locality description",
				 "C" = "County centroid"))

################################################################################
# B) Calculate buffer areas
################################################################################

# select coordinate columns in each dataset, long then lat
insitu_coords <- cbind(insitu[,1:2])
exsitu_coords <- cbind(exsitu[,c(8,7)])

# calculate area based on 50 kilometer buffers
insitu_buff_50 <- calc.buffer.area(insitu_coords,insitu,50000)
exsitu_buff_50 <- calc.buffer.area(exsitu_coords,exsitu,50000)
paste("Percent coverage with 50km radius: ",
	round((exsitu_buff_50/insitu_buff_50)*100,2), "%", sep = "")

# calculate area based on 10 kilometer buffers
insitu_buff_10 <- calc.buffer.area(insitu_coords,insitu,10000)
exsitu_buff_10 <- calc.buffer.area(exsitu_coords,exsitu,10000)
paste("Percent coverage with 10km radius: ",
	round((exsitu_buff_10/insitu_buff_10)*100,2), "%", sep = "")

################################################################################
# C) Map points and buffers
################################################################################

# create map for 50 km buffers
title <- paste("<b>","Quercus havardii in situ distribution and wild
	collection sites of ex situ accessions","<br/>","</b>",
	"Percent coverage of ex situ collections, based on 50 kilometer buffers: ",
	round((exsitu_buff_50/insitu_buff_50)*100,2),"%", sep = "")
map_50 <- map.buffers(insitu_coords,insitu,exsitu_coords,exsitu,title,50000)
# view map
map_50
# save map
htmlwidgets::saveWidget(map_50, file = "Quercus_havardii_buffer_map_50km.html")

# create map for 10 km buffers
title <- paste("<b>","Quercus havardii in situ distribution and wild
	collection sites of ex situ accessions","<br/>","</b>",
	"Percent coverage of ex situ collections, based on 10 kilometer buffers: ",
	round((exsitu_buff_10/insitu_buff_10)*100,2),"%", sep = "")
map_10 <- map.buffers(insitu_coords,insitu,exsitu_coords,exsitu,title,10000)
# view map
map_10
# save map
htmlwidgets::saveWidget(map_10, file = "Quercus_havardii_buffer_map_10km.html")
