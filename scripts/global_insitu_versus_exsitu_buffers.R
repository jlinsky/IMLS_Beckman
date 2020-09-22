### Author: Emily Beckman  ###  Date: 04/28/2020

### DESCRIPTION:
  # This script creates circular buffers around in situ and ex situ points and
	#		calculates the percent coverage based on area of buffers and number of
	#		EPA Level IV Ecoregions within the buffers
	# Right now the script is running for Quercus havardii, based on ex situ
	#		surveys in 2017 and 2019 and in situ points curated by Sean Hoban lab

### INPUTS:
	# all points, with ex situ marked ("BeckHob_QHOccur_Vetted_plusExSitu.csv")
	# shapefile of EPA Level IV Ecoregions
	#		("us_eco_l4_state_boundaries/us_eco_l4.shp")
	# 	("us_eco_l4/us_eco_l4_no_st.shp") # this one is uesd for mapping

	##OLD# in situ points, with at least "latitude" and "longitude" columns
	##		("BeckHob_QHOccur_Vetted.csv")
	##OLD# ex situ points, with at least "latitude" and "longitude" columns
	##		("havardii_exsitu_2017and2019_AllUSSpRecords.csv")

### OUTPUTS:
  # Table: Geographic Coverage (%)
		#                  All     East     West
		# -------------  -------  -------  -------
		# 50 km Buffer    24.23    20.87    32.94
		# 10 km Buffer    13.19    11.31    21.39
	# Table: Ecological Coverage (%)
		#                  All     East     West
		# -------------  -------  -------  -------
		# 50 km Buffer    45.12    34.15    53.49
		# 10 km Buffer    35.29    29.03    42.86
	# leaflet map with 50 km buffers
	# leaflet map with 10 km buffers

### NOTES:
	# Could make more flexible by creating list of populations then cycling
	# 	through all functions to get data for each popuatlion, versus hard
	#		coding 'east' and 'west'

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal","knitr",
	"RColorBrewer","Polychrome","rnaturalearth")
#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

select <- dplyr::select

#################
### FUNCTIONS ###
#################

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj){
	# select coordinate columns
	latlong <- df %>% select(longitude,latitude)
	# turn occurrence point data into a SpatialPointsDataFrame
	sp_df <- SpatialPointsDataFrame(latlong, df, proj4string = pt_proj)
	# reproject SpatialPointsDataFrame to specified projection
	proj_df <- spTransform(sp_df,buff_proj)
	# place buffer around each point
	buffers <- buffer(proj_df,width=radius,dissolve=T)
	# return buffer polygons
	return(buffers)
}

# create buffers around in situ and ex situ spatial points, calculate areas,
#		then compare to calculate percent coverage
compare.buff.area <- function(insitu,exsitu,radius,pt_proj,buff_proj){
	# create buffers
	buffer_insitu <- create.buffers(insitu,radius,pt_proj,buff_proj)
	buffer_exsitu <- create.buffers(exsitu,radius,pt_proj,buff_proj)
	# calculate buffer area
	area_insitu <- buffer_insitu@polygons[[1]]@area/1000000
	#print(paste("Area covered by in situ buffers:", round(area_insitu,2),"km²"))
	area_exsitu <- buffer_exsitu@polygons[[1]]@area/1000000
	#print(paste("Area covered by ex situ buffers:", round(area_exsitu,2),"km²"))
	# calculate difference between in situ and ex situ buffer areas (% coverage)
	area_diff_percent <- (area_exsitu/area_insitu)*100
	return(area_diff_percent)
}

# create data frame with ecoregion data extracted for area covered by buffers
intersect.eco.buff <- function(df,radius,pt_proj,buff_proj,eco){
	# create buffers
	buffers <- create.buffers(df,radius,pt_proj,buff_proj)
	# make sure ecoregions are in same projection as buffers
	eco_proj <- spTransform(eco,buff_proj)
	# intersect buffers with ecoregions
	buff_join_eco <- raster::intersect(buffers,eco_proj)
	return(buff_join_eco)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions
compare.eco.count.us <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	count_insitu <- eco_insitu@data %>% count(US_L4CODE) %>% count()
	count_exsitu <- eco_exsitu@data %>% count(US_L4CODE) %>% count()
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	return(eco_diff_percent[1,1])
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions
compare.eco.count.tnc <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	count_insitu <- eco_insitu@data %>% count(ECO_CODE) %>% count()
	count_exsitu <- eco_exsitu@data %>% count(ECO_CODE) %>% count()
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	return(eco_diff_percent[1,1])
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions
compare.eco.count.wwf <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	count_insitu <- eco_insitu@data %>% count(ECO_ID) %>% count()
	count_exsitu <- eco_exsitu@data %>% count(ECO_ID) %>% count()
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	return(eco_diff_percent[1,1])
}

# create map to visualize buffer and point data
map.buffers <- function(insitu,exsitu,title,radius,eco,pal){
	map <- leaflet() %>%
		## background
		addProviderTiles("Esri.WorldTerrain",
			options = providerTileOptions(maxZoom = 10)) %>%
		addPolygons(data = countries, stroke = F, color = "white", opacity = 1,
			fillOpacity = 1, weight = 2, smoothFactor = 0.5) %>%
		## country outlines
		addPolygons(data = countries, stroke = T, color = "black", opacity = 1,
			fillOpacity = 0, weight = 1.5, smoothFactor = 0.5, label = ~name,
			labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
		## ecoregions
		addPolygons(data = eco, color = ~pal, fillOpacity = 0.5, weight = 1,
			opacity = 0.8) %>%
		## insitu buffers
		addPolygons(data = create.buffers(insitu,radius,wgs.proj,wgs.proj),
			stroke = T, color = "#e3e3e3", smoothFactor = 0.5,	weight = 2, opacity = 0.8,
			fillColor = "#e3e3e3", fillOpacity = 0.4) %>%
		## insitu points
		addCircleMarkers(data = insitu, lng = ~longitude, lat = ~latitude,
			popup = ~paste("In situ:", Pop),
			radius = 4, fillOpacity = 1, stroke = F, color = "white") %>%
		## exsitu buffers
		addPolygons(data = create.buffers(exsitu,radius,wgs.proj,wgs.proj),
			smoothFactor = 0.5, weight = 2, color = "black",fillOpacity = 0.4) %>%
		## exsitu points
		addCircleMarkers(data = exsitu, lng = ~longitude, lat = ~latitude,
			#popup = ~paste("Ex situ institution:",institution,"<br/>",
			#	"Lat-long source:",gps_det,"<br/>","Collection year:",aqu_year,"<br/>",
			#	"Accession number:",acc_no),
			popup = ~paste("Ex situ:", Pop),
			radius = 4, fillOpacity = 1, stroke = F, color = "black") %>%
		## title
		addControl(title, position = "topright") %>%
		## note
		addControl("Click on points to see more information",
			position = "topleft") %>%
		## legend
		addLegend(labels = c("In situ","Ex situ"), colors = c("#e3e3e3","black"),
			title = "Key", position = "topright", opacity = 0.4)
	return(map)
}

################################################################################
# A) Read in data
################################################################################

setwd("./../..")
setwd("/Volumes/GoogleDrive/My Drive/Q_havardii_buffer_test")

# define projection of points (usually WGS 84)
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
	+no_defs +towgs84=0,0,0")
# define projection for calculations (meters must be the unit)
aea.proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
	+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

### POINT DATA

pts <- read.csv("BeckHob_QHOccur_Vetted_plusExSitu.csv", as.is=T,
	na.strings=c("","NA"))
insitu <- pts
exsitu <- pts[which(!is.na(pts$ex_situ)),]

# OLD: read in point data
#insitu <- read.csv("BeckHob_QHOccur_Vetted.csv", as.is=T, na.strings=c("","NA"))
#exsitu <- read.csv("havardii_exsitu_2017and2019_AllUSSpRecords.csv", as.is=T,
#	na.strings=c("","NA"))

# OLD: be sure all exsitu points are included in the insitu data
	# round coordinates before joining
#insitu$longitude <- round(insitu$longitude,5)
#insitu$latitude <- round(insitu$latitude,5)
#exsitu$longitude <- round(exsitu$longitude,5)
#exsitu$latitude <- round(exsitu$latitude,5)
	# select columns to join
#exsitu_add <- exsitu %>% select(latitude,longitude,east_west) %>% distinct()
	# join unique ex situ points to in situ points
#insitu <- full_join(insitu,exsitu_add)
#insitu[which(is.na(insitu$Pop)),]$Pop <- "exsitu_survey"
# recode gps determination column in exsitu data
#exsitu <- exsitu %>%
#  mutate(gps_det = recode(gps_det,
#         "G" = "Provided by institution",
#         "L" = "Locality description",
#				 "C" = "County centroid"))

# create subsets for eastern population and western population
#insitu_e <- insitu %>% filter(east_west == "E")
#insitu_w <- insitu %>% filter(east_west == "W")
#exsitu_e <- exsitu %>% filter(east_west == "E")
#exsitu_w <- exsitu %>% filter(east_west == "W")

### POLYGONS

# read in shapefile of U.S. ecoregions and state boundaries
#ecoregions <- readOGR("us_eco_l4_state_boundaries/us_eco_l4.shp")
# read in shapefile of just U.S. ecoregions
#ecoregions_nobound <- readOGR("us_eco_l4/us_eco_l4_no_st.shp")
# read in shapefile of TNC global ecoregions
ecoregions_tnc <- readOGR("terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
#length(unique(ecoregions_tnc@data$ECO_CODE)) #814
# read in shapefile of WWF global ecoregions
ecoregions_wwf <- readOGR("terr-ecoregions-WWF/wwf_terr_ecos.shp")
#length(unique(ecoregions_wwf@data$ECO_ID)) #827
# read in shapefile of Resolve 2017 global ecoregions
#ecoregions_2017 <- readOGR("Ecoregions2017_Resolve/Ecoregions2017.shp")
#length(unique(ecoregions_2017@data$ECO_ID)) #847

# get shapefile of global country boundaries
countries <- ne_countries(type = "countries", scale = "medium")

################################################################################
# B) Calculate geographic coverage (buffer areas)
################################################################################

### OVERALL

# calculate area based on 50 kilometer buffers
geo_coverage_50 <- compare.buff.area(insitu,exsitu,50000,wgs.proj,aea.proj)
paste("Percent coverage using 50km radius: ", round(geo_coverage_50,2),
	"%", sep = "")
# calculate area based on 10 kilometer buffers
#geo_coverage_10 <- compare.buff.area(insitu,exsitu,10000,wgs.proj,aea.proj)
#paste("Percent coverage using 10km radius: ", round(geo_coverage_10,2),
#	"%", sep = "")

### EAST ONLY

# calculate area based on 50 kilometer buffers
#geo_coverage_50e <- compare.buff.area(insitu_e,exsitu_e,50000,wgs.proj,aea.proj)
#paste("Percent coverage of EASTERN population, using 50km radius: ",
#	round(geo_coverage_50e,2), "%", sep = "")
# calculate area based on 10 kilometer buffers
#geo_coverage_10e <- compare.buff.area(insitu_e,exsitu_e,10000,wgs.proj,aea.proj)
#paste("Percent coverage of EASTERN population, using 10km radius: ",
#	round(geo_coverage_10e,2), "%", sep = "")

### WEST ONLY

# calculate area based on 50 kilometer buffers
#geo_coverage_50w <- compare.buff.area(insitu_w,exsitu_w,50000,wgs.proj,aea.proj)
#paste("Percent coverage of WESTERN population, using 50km radius: ",
#	round(geo_coverage_50w,2), "%", sep = "")
# calculate area based on 10 kilometer buffers
#geo_coverage_10w <- compare.buff.area(insitu_w,exsitu_w,10000,wgs.proj,aea.proj)
#paste("Percent coverage of WESTERN population, using 10km radius: ",
#	round(geo_coverage_10w,2), "%", sep = "")

################################################################################
# C) Calculate ecological coverage using buffers (ecoregion counts)
################################################################################

### OVERALL

# count ecoregions under 50 km buffers
	# U.S.
#eco_coverage_50 <- compare.eco.count.us(insitu,exsitu,50000,wgs.proj,aea.proj,
#	ecoregions)
#paste("Percent coverage using 50 km radius and Level IV ecoregions: ",
#	round(eco_coverage_50,2), "%", sep = "")
	# global TNC
eco_coverage_50 <- compare.eco.count.tnc(insitu,exsitu,50000,wgs.proj,aea.proj,
	ecoregions_tnc)
paste("Percent coverage using 50 km radius and TNC ecoregions: ",
	round(eco_coverage_50,2), "%", sep = "")
	# global WWF
#eco_coverage_50 <- compare.eco.count.wwf(insitu,exsitu,50000,wgs.proj,aea.proj,
#	ecoregions_wwf)
#paste("Percent coverage using 50 km radius and WWF ecoregions: ",
#	round(eco_coverage_50,2), "%", sep = "")

# count ecoregions under 10 km buffers
	# U.S.
#eco_coverage_10 <- compare.eco.count(insitu,exsitu,10000,wgs.proj,aea.proj,
#	ecoregions)
#paste("Percent coverage using 10 km radius and Level IV ecoregions: ",
#	round(eco_coverage_10,2), "%", sep = "")

### EAST ONLY

# count ecoregions under 50 km buffers
#eco_coverage_50e <- compare.eco.count(insitu_e,exsitu_e,50000,wgs.proj,aea.proj,
#	ecoregions)
#paste("Percent coverage of EASTERN population, using 50 km radius and Level IV ecoregions: ",
#	round(eco_coverage_50e,2), "%", sep = "")
# count ecoregions under 10 km buffers
#eco_coverage_10e <- compare.eco.count(insitu_e,exsitu_e,10000,wgs.proj,aea.proj,
#	ecoregions)
#paste("Percent coverage of EASTERN population, using 10 km radius and Level IV ecoregions: ",
#	round(eco_coverage_10e,2), "%", sep = "")

### WEST ONLY

# count ecoregions under 50 km buffers
#eco_coverage_50w <- compare.eco.count(insitu_w,exsitu_w,50000,wgs.proj,aea.proj,
#	ecoregions)
#paste("Percent coverage of WESTERN population, using 50 km radius and Level IV ecoregions: ",
#	round(eco_coverage_50w,2), "%", sep = "")
# count ecoregions under 10 km buffers
#eco_coverage_10w <- compare.eco.count(insitu_w,exsitu_w,10000,wgs.proj,aea.proj,
#	ecoregions)
#paste("Percent coverage of WESTERN population, using 10 km radius and Level IV ecoregions: ",
#	round(eco_coverage_10w,2), "%", sep = "")

################################################################################
# D) View summary results tables
################################################################################

### GEOGRAPHIC COVERAGE

#summary_tbl_geo <- matrix(c(geo_coverage_50,geo_coverage_50e,geo_coverage_50w,
#													geo_coverage_10,geo_coverage_10e,geo_coverage_10w),
#													ncol=3,byrow=TRUE)
#colnames(summary_tbl_geo) <- c("All","East","West")
#rownames(summary_tbl_geo) <- c("50 km Buffer","10 km Buffer")
#summary_tbl_geo <- as.table(summary_tbl_geo)

### ECOLOGICAL COVERAGE

#summary_tbl_eco <- matrix(c(eco_coverage_50,eco_coverage_50e,eco_coverage_50w,
#														eco_coverage_10,eco_coverage_10e,eco_coverage_10w),
#														ncol=3,byrow=TRUE)
#colnames(summary_tbl_eco) <- c("All","East","West")
#rownames(summary_tbl_eco) <- c("50 km Buffer","10 km Buffer")
#summary_tbl_eco <- as.table(summary_tbl_eco)

### VIEW RESULTS

#kable(summary_tbl_geo, format = "pandoc", align = "c", digits = 2,
#	caption = "Geographic Coverage (%)")
#kable(summary_tbl_eco, format = "pandoc", align = "c", digits = 2,
#	caption = "Ecological Coverage (%)")

################################################################################
# E) Map points and buffers
################################################################################

# select only ecoregions that are within the buffers; otherwise there are too
#		many and it takes a long time to load in browser
#inter <- intersect.eco.buff(insitu,50000,wgs.proj,wgs.proj,ecoregions_tnc)
eco_wgs_tnc <- spTransform(ecoregions_tnc,wgs.proj)
#codes <- unique(inter@data$ECO_CODE)
#eco_inter <- eco_wgs[eco_wgs@data$ECO_CODE %in% codes,]
#eco_wgs <- spTransform(ecoregions_nobound,wgs.proj)
#codes <- unique(inter@data$US_L4CODE)
#eco_inter <- eco_wgs[eco_wgs@data$US_L4CODE %in% codes,]

# create color palette
#pal <- hcl.colors(length(unique(eco_inter@data$ECO_CODE)))
#pal <- hcl.colors(length(unique(eco_inter@data$US_L4CODE)))
#pal <- rainbow(length(unique(eco_inter@data$US_L4CODE)))
#pal <- heat.colors(length(unique(eco_inter@data$US_L4CODE)))
#pal <- terrain.colors(length(unique(eco_inter@data$ECO_CODE)))
#pal <- topo.colors(length(unique(eco_inter@data$US_L4CODE)))
#pal <- cm.colors(length(unique(eco_inter@data$US_L4CODE)))

# create palette for ecoregions
#pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome,
#  stepped, tol, watlington,
#  show.names=FALSE)
pal <- createPalette(length(unique(ecoregions_tnc@data$ECO_CODE)),
	seedcolors = c("#bf2b21","#d69b54","#c7ae0e","#3064bf","#7470c4","#660ba3",
	"#b05fab"),range = c(5,50), target = "normal", M=50000)
pal <- as.vector(pal)
length(pal)

# create map for 50 km buffers
title <- paste(
	"<b>","Quercus havardii in situ distribution and wild
		collection sites of ex situ accessions","<br/>","</b>",
	"Geographic coverage of ex situ collections, based on 50 kilometer
		buffers: ", round(geo_coverage_50,2), "%" ,"<br/>",
	"Ecological coverage of ex situ collections, based on TNC
		Ecoregions within the buffers: ", round(eco_coverage_50,2), "%",
	sep = "")
map_50 <- map.buffers(insitu,exsitu,title,50000,eco_wgs_tnc,pal)
# view map
map_50
# save map
#htmlwidgets::saveWidget(map_50, file = "Quercus_havardii_buffer_map_50km.html")

# create map for 10 km buffers
title <- paste(
	"<b>","Quercus havardii in situ distribution and wild
		collection sites of ex situ accessions","<br/>","</b>",
	"Geographic coverage of ex situ collections, based on 10 kilometer
		buffers: ", round(geo_coverage_10,2), "%" ,"<br/>",
	"Ecological coverage of ex situ collections, based on EPA Level IV
		Ecoregions within the buffers: ", round(eco_coverage_10,2), "%",
	sep = "")
map_10 <- map.buffers(insitu,exsitu,title,10000,eco_inter)
# view map
map_10
# save map
#htmlwidgets::saveWidget(map_10, file = "Quercus_havardii_buffer_map_10km.html")
















######
# OLD -- count ecoregions by points not buffers
######

# create data frame with ecoregion data extracted for each spatial point
extract.ecoregions.pts <- function(df){
	# turn occurrence points into SpatialPoints
	latlong <- df %>% select(longitude,latitude)
	pts_wgs <- SpatialPoints(latlong, proj4string = wgs.proj)
	# pull ecoregion data at each spatial point
	df_eco <- extract(ecoregions_wgs,pts_wgs)
	# join point data to extracted data
	df_eco <- cbind(df,df_eco)
	return(df_eco)
}

### OVERALL

# extract ecoregion data for each spatial point
insitu_eco <- extract.ecoregions.pts(insitu)
exsitu_eco <- extract.ecoregions.pts(exsitu)

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