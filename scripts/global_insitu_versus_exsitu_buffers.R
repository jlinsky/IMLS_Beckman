### Author: Emily Beckman  ###  Date: 10/26/2020

### DESCRIPTION:

### INPUTS:
	# in situ and ex situ points
	# ecoregions

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

################################################################################

#################
### LIBRARIES ###
#################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal","knitr",
	"RColorBrewer","Polychrome","rnaturalearth","cleangeo","smoothr")
#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)

select <- dplyr::select

#########################
### WORKING DIRECTORY ###
#########################

# set up working directories
main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Gap Analysis"
local_dir <- "./Desktop/work"

################################################################################
################################################################################
# Read in data
################################################################################

# define initial projection of points (usually WGS 84)
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
	+no_defs +towgs84=0,0,0")
# define projection for calculations (meters must be the unit)
aea.proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
	+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

### POLYGON DATA

## Ecoregions
	# U.S. only
	# https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
	# shapefiles of U.S. ecoregions and state boundaries
#coregions_l4 <- readOGR(file.path(local_dir,"us_eco_l4_state_boundaries/us_eco_l4.shp"))
#ecoregions_l4.wgs <- spTransform(ecoregions_l4,wgs.proj)
#ecoregions_l3 <- readOGR(file.path(local_dir,"us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"))
#ecoregions_l3.wgs <- spTransform(ecoregions_l3,wgs.proj)
#ecoregions_l3_clean.wgs <- clgeo_Clean(ecoregions_l3.wgs)
#us_boundary <- aggregate(ecoregions_l3_clean.wgs,dissolve = TRUE)
	# shapefiles of just U.S. ecoregions
ecoregions_l4 <- readOGR(file.path(local_dir,"us_eco_l4/us_eco_l4_no_st.shp"))
ecoregions_l4.wgs <- spTransform(ecoregions_l4,wgs.proj)
ecoregions_l3 <- readOGR(file.path(local_dir,"us_eco_l3/us_eco_l3.shp"))
ecoregions_l3.wgs <- spTransform(ecoregions_l3,wgs.proj)
ecoregions_l3_clean.wgs <- clgeo_Clean(ecoregions_l3.wgs)
#us_boundary <- aggregate(ecoregions_l3_clean.wgs,dissolve = TRUE)
	# Global
	# shapefile of TNC global ecoregions
	# http://maps.tnc.org/gis_data.html
#ecoregions_tnc <- readOGR("terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
#length(unique(ecoregions_tnc@data$ECO_CODE)) #814
	# shapefile of WWF global ecoregions
	# https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
#ecoregions_wwf <- readOGR("terr-ecoregions-WWF/wwf_terr_ecos.shp")
#length(unique(ecoregions_wwf@data$ECO_ID)) #827
	# shapefile of Resolve 2017 global ecoregions
	# https://ecoregions2017.appspot.com
#ecoregions_2017 <- readOGR("Ecoregions2017_Resolve/Ecoregions2017.shp")
#length(unique(ecoregions_2017@data$ECO_ID)) #847

## States
	# U.S. states
	# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us_states <- readOGR(file.path(local_dir,"cb_2018_us_state_5m/cb_2018_us_state_5m.shp"))
us_states.wgs <- spTransform(us_states,wgs.proj)
us_boundary <- aggregate(us_states.wgs,dissolve = TRUE)

## Countries
	# get shapefile of global country boundaries
#countries <- ne_countries(type = "countries", scale = "large")
	# reproject to WGS84
#countries.wgs <- spTransform(countries, wgs.proj)
#us <- countries.wgs[countries.wgs$name == "United States of America", ]

# Elbert L. Little, Jr. digitized maps
	# https://web.archive.org/web/20170127093428/https://gec.cr.usgs.gov/data/little/
#sp <- "fagugran"
#download.file(
#	"https://web.archive.org/web/20170127095632/https://gec.cr.usgs.gov/data/little/fagugran.zip",
#  destfile = file.path(local_dir,paste0(sp,".zip")))
#unzip(zipfile = file.path(local_dir,paste0(sp,".zip")),
#  exdir = file.path(local_dir,sp))
#  # delete unzipped folder
#unlink(paste0(file.path(local_dir,paste0(sp,".zip"))))
  # read in file
#little <- readOGR(file.path(local_dir,sp,paste0(sp,".shp")))
	# assign projection based on metadata online (was NA)
#proj4string(little) <- CRS("+init=epsg:4267")
	# reproject
#little.wgs <- spTransform(little,wgs.proj)
	# clip to target area only
#little.wgs <- gIntersection(us_bound, little.wgs, byid = TRUE, drop_lower_td = TRUE)
#little_smooth.wgs <- smooth(little.wgs, method = "spline")

### COLOR PALETTES

## Ecoregions polygons
eco_pal <- createPalette(length(unique(ecoregions_l3_clean.wgs@data$NA_L3CODE)),
	seedcolors = c("#bf2b21","#d69b54","#c7ae0e","#3064bf","#7470c4","#660ba3",
	"#b05fab"),range = c(5,50), target = "normal", M=50000)
eco_pal <- as.vector(eco_pal)
length(eco_pal)

eco_pal <- colorNumeric("Greys", ecoregions_l3_clean.wgs@data$Shape_Area,
	na.color = "white")

## Ex situ point data
	# small
triangle_sm <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
 	iconWidth = 8, iconHeight = 8)
triangle_sm2 <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/pink-triangle-png-7.png",
 	iconWidth = 12, iconHeight = 12)
	#medium
triangle_md <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
 	iconWidth = 18, iconHeight = 18)
triangle_md2 <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/pink-triangle-png-7.png",
 	iconWidth = 22, iconHeight = 22)
	# large
triangle_lg <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
	iconWidth = 28, iconHeight = 28)
triangle_lg2 <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/pink-triangle-png-7.png",
 	iconWidth = 32, iconHeight = 32)
	# legend
#html_legend <- "Source locality and number of wild provenance<br/>
#individuals present in ex situ collections<br/>
#<img src='https://www.freeiconspng.com/uploads/triangle-png-28.png'>1-10<br/>
#<img src='http://leafletjs.com/examples/custom-icons/leaf-red.png'>red"

####
##### SPECIES BY SPECIES
####

target_sp <- c("Fagus_grandifolia","Juglans_nigra","Juglans_cinerea",
	"Juglans_major","Juglans_californica","Juglans_microcarpa","Juglans_hindsii")
target_sp_codes <- c("531","602","601","606",NA,"605",NA)


for(sp in 1:length(target_sp)){

# POINT DATA

# in situ and ex situ points (output from 3-1_refine_occurrence_points.R)
pts <- read.csv(file.path(main_dir,
	paste0("occurrence_points/outputs/spp_edited_points/",target_sp[sp],".csv")),
	na.strings=c("","NA"), stringsAsFactors = F)
nrow(pts)
# set database as factor and order appropriately
pts$database <- factor(pts$database,
  levels = c("FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN","Ex_situ"))
# arrange by database and filter out some flagged points
pts <- pts %>% arrange(desc(database)) %>%
	filter(.cen & .inst & .con & .outl & .yr1980 &
		basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
			basisOfRecord != "H?" &
		establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
			establishmentMeans != "INVASIVE")
nrow(pts)
# clip points by ecoregions so only in target area
	# select coordinate columns
latlong <- pts %>% select(decimalLongitude,decimalLatitude)
	# turn occurrence point data into a SpatialPointsDataFrame
spatial_pts <- SpatialPointsDataFrame(latlong, pts, proj4string = wgs.proj)
	# clip
spatial_pts <- spatial_pts[us_boundary, ]
nrow(spatial_pts)
insitu <- spatial_pts

# ex situ points only
exsitu <- insitu[insitu$database == "Ex_situ", ]@data
exsitu$establishmentMeans <- as.numeric(exsitu$establishmentMeans)
exsitu1 <- exsitu %>% arrange(establishmentMeans) %>%
	filter(establishmentMeans <= 10)
exsitu2 <- exsitu %>% arrange(establishmentMeans) %>%
	filter(establishmentMeans > 10 & establishmentMeans < 50)
exsitu3 <- exsitu %>% arrange(establishmentMeans) %>%
	filter(establishmentMeans >= 50)

### RASTER DATA

## Species native distribution
	# https://www.fs.usda.gov/rds/archive/catalog/RDS-2013-0013
# raster of modeled species distribution from Wilson et al. 2013
distribution <- raster(file.path(local_dir,
	paste0("Wilson_2013_rasters/s",target_sp_codes[sp],".img")))
	# aggregate so its not as big and detailed
distribution_agg <- aggregate(distribution, fact=20, fun=mean, dissolve = T)
	# remove lowest values (they cover the whole modeled area)
#distribution_agg[distribution_agg < 0.01] <- NA
	# reproject
distribution_agg <- projectRaster(distribution_agg, crs = wgs.proj)
	# crop
distribution_agg_clip <- crop(distribution_agg, us_boundary)
	# mask the cells that do not overlap a polygon
distribution_agg_clip <- rasterize(us_boundary, distribution_agg_clip,
	mask=TRUE)

## Species native distribution raster
dist_pal <- colorNumeric(
	#c("transparent","#999999","#7d7d7d","#595959","#454545"),
	c("transparent","#bddb95","#a4db7f","#80db67","#4cc246","#2aa33b","#138a3b","#077036"),#,"#02522f"
	#c("transparent","#ffe770","#faca5a","#fcaf3a","#f59425","#fa6f19","#de4310","#b82807"),
	values(distribution_agg_clip),na.color = "transparent")

### MAP

  # create map
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## Base layer
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #CartoDB.PositronNoLabels
		## Species name label
		addControl(paste0("<b>",target_sp[sp]), position = "topright") %>%
		## Raster distribution
		#addRasterImage(distribution_agg_clip, opacity = 1, colors = dist_pal,
		#	group = "Modeled live basal area (Wilson et al., 2013)") %>%
    ## In situ
    addCircleMarkers(data = insitu,
      color = "#508757", radius = 4, fillOpacity = 0.5, stroke = F,
			group = "In situ occurrence points") %>%
		## EPA Level III ecoregions
		addPolygons(data = ecoregions_l3_clean.wgs,
			fillColor = "grey",
			#fillColor = ~eco_pal(ecoregions_l3_clean.wgs@data$Shape_Area),
			fillOpacity = 0.1, weight = 1, opacity = 0.4, color = "black",
			group = "EPA Level III Ecoregions") %>%
		## U.S. states outline
		addPolygons(data = us_states, fillColor = "transparent",
			weight = 1.5, opacity = 0.4, color = "black",
			group = "U.S. state outlines") %>%
		## Ex situ
    addMarkers(data = exsitu1,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_sm2) %>%
    addMarkers(data = exsitu1,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_sm) %>%
    addMarkers(data = exsitu2,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_md2) %>%
    addMarkers(data = exsitu2,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_md) %>%
    addMarkers(data = exsitu3,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_lg2) %>%
    addMarkers(data = exsitu3,
			lng = ~decimalLongitude, lat = ~decimalLatitude,
			icon = triangle_lg) %>%
			#maxZoom = 10
			#radius = 6, fill = T, fillOpacity = 1,
			#fillColor = ~pal_exsitu(exsitu1@data$establishmentMeans),
			#stroke = T, color = "black", weight = 1, opacity = 1)
		## Layers control
    addLayersControl(
      overlayGroups = c(#"Modeled live basal area (Wilson et al., 2013)",
												"In situ occurrence points",
												"U.S. state outlines",
												"EPA Level III Ecoregions"),
      options = layersControlOptions(collapsed = FALSE))# %>%
    #hideGroup("In situ occurrence points") %>%
		#addLegend(pal = dist_pal, values = values(distribution_agg_clip),
    #	opacity = 1, title = "Live basal area", position = "bottomright")
		#addControl(html = html_legend, position = "bottomright")
  map

}

  # save map
  htmlwidgets::saveWidget(map, file.path(path.figs,
    paste0(spp.all[i], "_leaflet_map.html")))

  cat("\tEnding ", spp.all[i], ", ", i, " of ", length(spp.all), ".\n\n", sep="")
}




    #addProviderTiles(providers$CartoDB.PositronNoLabels, #Esri.WorldGrayCanvas,
    #  group = "CartoDB.PositronNoLabels") %>%
		## Labels
    #addControl(paste0("<b>",spp.all[i]), position = "topright") %>%
		#      popup = ~paste0(
		 #       "<b>Source database:</b> ",database,"<br/>",
		  #      "<b>Year:</b> ",year,"<br/>",
		   #     "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
		    #    "<b>Dataset name:</b> ",datasetName,"<br/>"),
      #popup = ~paste0(
      #  "<b>Institution:</b> ",datasetName,"<br/>",
      #  "<b>Year:</b> ",year,"<br/>",
      #  "<b>Provenance type:</b> ",basisOfRecord,"<br/>",
      #  "<b>Number of individuals:</b> ",establishmentMeans,"<br/>"),

    #addControl(
    #  "Toggle the checkboxes below on/off to view flagged points (colored red) in each category.</br>
    #  If no points turn red when box is checked, there are no points flagged in that category.</br>
    #  Click each point for more information about the record.",
    #  position = "topright") %>%
    ## Overlay groups (can toggle)
			## Little polygon
		#addPolygons(data = little_smooth.wgs, color = grey, opacity = 0.6,
		#	group = "Approximate native range (Little, 1971-1978)") %>%

		  #baseGroups = c("CartoDB.PositronNoLabels",
      #               "CartoDB.Positron",
      #               "Esri.WorldTopoMap",
      #               "Stamen.Watercolor"),

    #addControl(
    #  "See https://github.com/MortonArb-CollectionsValue/OccurrencePoints
    #  for information about data sources and flagging methodology.",
    #  position = "bottomleft")






pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(distribution_agg),
  na.color = "transparent")

	leaflet() %>%
		## background
		addProviderTiles("Esri.WorldGrayCanvas",
			options = providerTileOptions(maxZoom = 10)) %>%
		#addPolygons(data = ecoregions_l3.wgs)
		## insitu points
		addCircleMarkers(data = spatial_pts,
			radius = 3, fillOpacity = 1, stroke = F, color = "black")
		## exsitu points
		addCircleMarkers(data = exsitu, lng = ~decimalLongitude, lat = ~decimalLatitude,
			radius = 3, fillOpacity = 1, stroke = F, color = "white") %>%




################################################################################
# B) Calculate geographic coverage (buffer areas)
################################################################################




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
