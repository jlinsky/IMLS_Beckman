### Author: Emily Beckman  ###  Date: 04/17/2020

### DESCRIPTION:
  #

### INPUTS:
  #

### OUTPUTS:
  #

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal","knitr")

#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

#################
### FUNCTIONS ###
#################

# create buffers around spatial points and calculate area
calc.buffer.area <- function(df,radius){
	# select coordinate columns
	latlong <- df %>% select(longitude,latitude)
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
	#print(paste("Area covered by the buffers:", round(buff_area,2), "km²"))
	# return buffer polygons
	return(buff_area)
}

# create data frame with ecoregion data extracted for each spatial point
extract.ecoregions <- function(df){
	# turn occurrence points into spatial points
	latlong <- df %>% select(longitude,latitude)
	wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
		+no_defs +towgs84=0,0,0")
	pts_wgs <- SpatialPoints(latlong, proj4string = wgs.proj)
	# pull ecoregion data at each spatial point
	df_eco <- extract(ecoregions_wgs,pts_wgs)
	# join point data to extracted data
	df_eco <- cbind(df,df_eco)
	return(df_eco)
}

# create buffers around spatial points, for viewing with leaflet
view.buffers <- function(df,radius){
	# select coordinate columns
	latlong <- df %>% select(longitude,latitude)
	# turn occurrence point data into a SpatialPointsDataFrame
	wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
		+no_defs +towgs84=0,0,0")
	wgs_df <- SpatialPointsDataFrame(latlong, df, proj4string = wgs.proj)
	# place buffer around each point
	buffer_wgs <- buffer(wgs_df,width=radius,dissolve=T)
	# return buffer polygons
	return(buffer_wgs)
}

# create map to visualize buffer and point data
map.buffers <- function(insitu,exsitu,title,radius){
	map <- leaflet() %>%
		addProviderTiles(
			"CartoDB.PositronNoLabels",
			options = providerTileOptions(maxZoom = 10)) %>%
		addPolygons(
			data = view.buffers(insitu,radius), smoothFactor = 0.5,
			weight = 2, color = "red") %>%
		addPolygons(
			data = view.buffers(exsitu,radius), smoothFactor = 0.5,
			weight = 2, color = "blue") %>%
		addCircleMarkers(
			data = insitu, lng = ~longitude, lat = ~latitude,
			popup = ~paste("In situ:", Pop), radius = 4, fillOpacity = 0.7,
			stroke = F, color = "red") %>%
		addCircleMarkers(
			data = exsitu, lng = ~longitude, lat = ~latitude,
			popup = ~paste("Ex situ institution:",institution,"<br/>",
				"Lat-long source:",gps_det,"<br/>",
				"Collection year:",aqu_year),
			radius = 4, fillOpacity = 0.7, stroke = F, color = "blue") %>%
		addControl(
			title, position = "topright") %>%
		addControl(
			"Click on points to see more information", position = "topleft") %>%
		addLegend(
			labels = c("In situ","Ex situ"), colors = c("red","blue"), title = "Key",
			position = "topright", opacity = 0.75)
	return(map)
}

################################################################################
# A) Read in data
################################################################################

setwd("Desktop")

# read in point data
insitu <- read.csv("BeckHob_QHOccur_Vetted.csv", as.is=T, na.strings=c("","NA"))
	# round coordinates
	insitu$Longitude <- round(insitu$Longitude,5)
	insitu$Latitude <- round(insitu$Latitude,5)
exsitu <- read.csv("havardii_exsitu_2017_AllUSSPRecords.csv", as.is=T,
	na.strings=c("","NA"))
	exsitu$long <- round(exsitu$long,5)
	exsitu$lat <- round(exsitu$lat,5)

# be sure all exsitu points are included in the insitu data
exsitu_add <- exsitu %>% select(latitude,longitude,east_west) %>% distinct()
insitu <- full_join(insitu,exsitu_add)
insitu[which(is.na(insitu$Pop)),]$Pop <- "exsitu_survey_2017"
insitu

# recode gps determination column in exsitu data
exsitu <- exsitu %>%
  mutate(gps_det = recode(gps_det,
         "G" = "Provided by institution",
         "L" = "Locality description",
				 "C" = "County centroid"))

# create subsets for eastern population and western population
insitu_e <- insitu %>% filter(east_west == "E")
insitu_w <- insitu %>% filter(east_west == "W")
exsitu_e <- exsitu %>% filter(east_west == "E")
exsitu_w <- exsitu %>% filter(east_west == "W")

# read in shapefile of ecoregions and state boundaries
ecoregions <- readOGR("us_eco_l4_state_boundaries/us_eco_l4.shp")
# reproject ecoregions to match spatial point projection
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
	+no_defs +towgs84=0,0,0")
ecoregions_wgs <- spTransform(ecoregions, wgs.proj)

################################################################################
# B) Calculate geographic coverage (buffer areas)
################################################################################

### OVERALL

# calculate area based on 50 kilometer buffers
insitu_buff_50 <- calc.buffer.area(insitu,50000)
exsitu_buff_50 <- calc.buffer.area(exsitu,50000)
g_coverage_50 <- round((exsitu_buff_50/insitu_buff_50)*100,2)
	paste("Percent coverage using 50km radius: ", g_coverage_50, "%", sep = "")

# calculate area based on 10 kilometer buffers
insitu_buff_10 <- calc.buffer.area(insitu,10000)
exsitu_buff_10 <- calc.buffer.area(exsitu,10000)
g_coverage_10 <- round((exsitu_buff_10/insitu_buff_10)*100,2)
	paste("Percent coverage using 10km radius: ", g_coverage_10, "%", sep = "")

### EAST ONLY

# calculate area based on 50 kilometer buffers
insitu_buff_50_e <- calc.buffer.area(insitu_e,50000)
exsitu_buff_50_e <- calc.buffer.area(exsitu_e,50000)
g_coverage_50_east <- round((exsitu_buff_50_e/insitu_buff_50_e)*100,2)
	paste("Percent coverage of EASTERN population, using 50km radius: ",
	g_coverage_50_east, "%", sep = "")

# calculate area based on 10 kilometer buffers
insitu_buff_10_e <- calc.buffer.area(insitu_e,10000)
exsitu_buff_10_e <- calc.buffer.area(exsitu_e,10000)
g_coverage_10_east <- round((exsitu_buff_10_e/insitu_buff_10_e)*100,2)
	paste("Percent coverage of EASTERN population, using 10km radius: ",
	g_coverage_10_east, "%", sep = "")

### WEST ONLY

# calculate area based on 50 kilometer buffers
insitu_buff_50_w <- calc.buffer.area(insitu_w,50000)
exsitu_buff_50_w <- calc.buffer.area(exsitu_w,50000)
g_coverage_50_west <- round((exsitu_buff_50_w/insitu_buff_50_w)*100,2)
	paste("Percent coverage of WESTERN population, using 50km radius: ",
	g_coverage_50_west, "%", sep = "")

# calculate area based on 10 kilometer buffers
insitu_buff_10_w <- calc.buffer.area(insitu_w,10000)
exsitu_buff_10_w <- calc.buffer.area(exsitu_w,10000)
g_coverage_10_west <- round((exsitu_buff_10_w/insitu_buff_10_w)*100,2)
	paste("Percent coverage of WESTERN population, using 10km radius: ",
	g_coverage_10_west, "%", sep = "")

################################################################################
# C) Calculate ecological coverage (ecoregion counts)
################################################################################

### OVERALL

# extract ecoregion data for each spatial point
insitu_eco <- extract.ecoregions(insitu)
exsitu_eco <- extract.ecoregions(exsitu)

# count number Level 4 ecoregions with occurrence points and calculate coverage
insitu_eco_4 <- insitu_eco %>% count(US_L4CODE) %>% count()
exsitu_eco_4 <- exsitu_eco %>% count(US_L4CODE) %>% count()
e_coverage_4 <- round((exsitu_eco_4[[1]]/insitu_eco_4[[1]])*100,2)
	paste("Percent coverage using Level IV ecoregions: ",
	e_coverage_4, "%", sep = "")

# count number Level 3 ecoregions with occurrence points and calculate coverage
insitu_eco_3 <- insitu_eco %>% count(US_L3CODE) %>% count()
exsitu_eco_3 <- exsitu_eco %>% count(US_L3CODE) %>% count()
e_coverage_3 <- round((exsitu_eco_3[[1]]/insitu_eco_3[[1]])*100,2)
	paste("Percent coverage using Level III ecoregions: ",
	e_coverage_3, "%", sep = "")

### EAST ONLY

# select data for eastern population
insitu_eco_e <- insitu_eco %>% filter(east_west == "E")
exsitu_eco_e <- exsitu_eco %>% filter(east_west == "E")

# count number Level 4 ecoregions with occurrence points and calculate coverage
insitu_eco_4_e <- insitu_eco_e %>% count(US_L4CODE) %>% count()
exsitu_eco_4_e <- exsitu_eco_e %>% count(US_L4CODE) %>% count()
e_coverage_4_east <- round((exsitu_eco_4_e[[1]]/insitu_eco_4_e[[1]])*100,2)
	paste("Percent coverage of EASTERN population, using Level IV ecoregions: ",
	e_coverage_4_east, "%", sep = "")

# count number Level 3 ecoregions with occurrence points and calculate coverage
insitu_eco_3_e <- insitu_eco_e %>% count(US_L3CODE) %>% count()
exsitu_eco_3_e <- exsitu_eco_e %>% count(US_L3CODE) %>% count()
e_coverage_3_east <- round((exsitu_eco_3_e[[1]]/insitu_eco_3_e[[1]])*100,2)
	paste("Percent coverage of EASTERN population, using Level III ecoregions: ",
	e_coverage_3_east, "%", sep = "")

### WEST ONLY

# select data for western population
insitu_eco_w <- insitu_eco %>% filter(east_west == "W")
exsitu_eco_w <- exsitu_eco %>% filter(east_west == "W")

# count number Level 4 ecoregions with occurrence points and calculate coverage
insitu_eco_4_w <- insitu_eco_w %>% count(US_L4CODE) %>% count()
exsitu_eco_4_w <- exsitu_eco_w %>% count(US_L4CODE) %>% count()
e_coverage_4_west <- round((exsitu_eco_4_w[[1]]/insitu_eco_4_w[[1]])*100,2)
	paste("Percent coverage of WESTERN population, using Level IV ecoregions: ",
	e_coverage_4_west, "%", sep = "")

# count number Level 3 ecoregions with occurrence points and calculate coverage
insitu_eco_3_w <- insitu_eco_w %>% count(US_L3CODE) %>% count()
exsitu_eco_3_w <- exsitu_eco_w %>% count(US_L3CODE) %>% count()
e_coverage_3_west <- round((exsitu_eco_3_w[[1]]/insitu_eco_3_w[[1]])*100,2)
	paste("Percent coverage of WESTERN population, using Level III ecoregions: ",
	e_coverage_3_west, "%", sep = "")

################################################################################
# D) View summary results tables
################################################################################

### GEOGRAPHIC COVERAGE

summary_tbl_geo <- matrix(c(g_coverage_50,g_coverage_50_east,g_coverage_50_west,
													g_coverage_10,g_coverage_10_east,g_coverage_10_west),
													ncol=3,byrow=TRUE)
colnames(summary_tbl_geo) <- c("All","East","West")
rownames(summary_tbl_geo) <- c("50 km Buffer","10 km Buffer")
summary_tbl_geo <- as.table(summary_tbl_geo)

### ECOLOGICAL COVERAGE

summary_tbl_eco <- matrix(c(e_coverage_3,e_coverage_3_east,e_coverage_3_west,
														e_coverage_4,e_coverage_4_east,e_coverage_4_west),
														ncol=3,byrow=TRUE)
colnames(summary_tbl_eco) <- c("All","East","West")
rownames(summary_tbl_eco) <- c("Level III Ecoregions","Level IV Ecoregions")
summary_tbl_eco <- as.table(summary_tbl_eco)

### VIEW RESULTS

kable(summary_tbl_geo, format = "pandoc", align = "c",
	caption = "Geographic Coverage (%)")
kable(summary_tbl_eco, format = "pandoc", align = "c",
	caption = "Ecological Coverage (%)")

################################################################################
# E) Map points and buffers
################################################################################

# create map for 50 km buffers
title <- paste("<b>","Quercus havardii in situ distribution and wild
	collection sites of ex situ accessions","<br/>","</b>",
	"Percent coverage of ex situ collections, based on 50 kilometer buffers: ",
	round((exsitu_buff_50/insitu_buff_50)*100,2),"%", sep = "")
map_50 <- map.buffers(insitu,exsitu,title,50000)
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
