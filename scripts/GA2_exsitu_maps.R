
################################################################################

### Author: Emily Beckman ### Date: 07/22/20

### DESCRIPTION:

### DATA IN:

### DATA OUT:


################################################################################
# Load libraries
################################################################################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal","knitr",
	"RColorBrewer","pals","rcartocolor","colorspace","Polychrome")
#install.packages(my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Gap Analysis/Ex situ Survey"

# or use 0-1_set_workingdirectory.R script:
#source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")

################################################################################
# 1. Read in data
################################################################################

### TARGET SPECIES

target_sp <- c(
	"Carya floridana","Carya myristiciformis",
	"Fagus grandifolia",
	"Gymnocladus dioicus",
	"Juglans californica","Juglans cinerea","Juglans hindsii","Juglans major",
		"Juglans microcarpa","Juglans nigra",
	"Lindera benzoin","Persea borbonia","Persea humilis","Persea palustris",
		"Sassafras albidum",
	"Pinus albicaulis","Pinus balfouriana","Pinus coulteri","Pinus flexilis",
		"Pinus lambertiana","Pinus monticola","Pinus muricata","Pinus palustris",
		"Pinus ponderosa","Pinus radiata","Pinus strobiformis","Pinus torreyana",
	"Taxus brevifolia","Taxus canadensis","Taxus floridana")

### POINTS

exsitu_20 <- read.csv(file.path(main_dir,
	"exsitu_compiled_standardized_dupsRemoved_7_15_20_TargetSpecies.csv"),
	as.is=T, na.strings=c("","NA"))
nrow(exsitu_20) #1094

exsitu_19 <- read.csv(file.path(main_dir,
	"GA2_exsitu_compiled_GEOLOCATED.csv"),
	as.is=T, na.strings=c("","NA"))
exsitu_19 <- exsitu_19 %>%
	filter(sp_full_name_acc %in% target_sp) %>%
	filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
nrow(exsitu_19) #4731

### POLYGONS

# shapefile of ecoregions and state boundaries
ecoregions <- readOGR(file.path(main_dir,
	"us_eco_l3_state_boundaries","us_eco_l3_state_boundaries.shp"))
# read in shapefile of just ecoregions
ecoregions_nobound <- readOGR(file.path(main_dir,
	"us_eco_l3","us_eco_l3.shp"))
# read in shapefile of species native distribution
species_dist <- readOGR(file.path(main_dir,
	"Fagus_Gymnocladus_Juglans_merge_distribution.shp"))

# PROJECTIONS

# define projection of points (usually WGS 84)
wgs.proj <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84
	+no_defs +towgs84=0,0,0")
# define projection for calculations (meters must be the unit)
aea.proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110
	+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")


# IUCN RL
library(rredlist)
countries <- data.frame()
for(i in 1:length(target_sp)){
	#print(target_sp[[i]])
	dist <- rl_occ_country(target_sp[[i]])
	name <- dist$name
	dist <- as.data.frame(dist$result)
	if(nrow(dist>0)){
		dist$genus_species <- rep(name)
		countries <- rbind(countries,dist)
	} else {
		print(paste(target_sp[[i]],"not found"))
	}
}


################################################################################
# 1. Visualize data
################################################################################

# create species-specific subsets
	# polygon
dist <- subset(species_dist,species == "F_grandifolia")
dist_proj <- spTransform(dist,wgs.proj)
eco_proj <- spTransform(ecoregions_nobound,wgs.proj)
	# points
exsitu_sub <- exsitu_20 %>%
	filter(taxon_name_acc == "Fagus grandifolia")
		# select coordinate columns
latlong <- exsitu_sub %>% select(long_round,lat_round)
		# turn occurrence point data into a SpatialPointsDataFrame
exsitu_proj <- SpatialPointsDataFrame(latlong, exsitu_sub,
	proj4string = wgs.proj)
		# reproject SpatialPointsDataFrame to specified projection
#exsitu_proj <- spTransform(exsitu_sp,aea.proj)

# create palette for ecoregions
#pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome,
#  stepped, tol, watlington,
#  show.names=FALSE)
pal <- createPalette(length(unique(eco_proj@data$US_L3CODE)),
	seedcolors = c("#bf2b21","#d69b54","#c7ae0e","#3064bf","#7470c4","#660ba3","#b05fab"),
	range = c(5,50), target = "normal", M=50000)
pal <- as.vector(pal)
length(pal)
pal2 <- hcl.colors(length(unique(eco_proj@data$US_L3CODE)))

# mapping function
map.leaflet <- function(exsitu,dist,eco){
	map <- leaflet() %>%
		addProviderTiles("CartoDB.PositronNoLabels",
			options = providerTileOptions(maxZoom = 10)) %>%
		addPolygons(data = eco, color = ~pal, fillOpacity = 0.5, weight = 1,
			smoothFactor = 0.5, opacity = 0.3, popup = ~US_L3NAME) %>%
		addPolygons(data = dist, fillOpacity = 0.2,
			smoothFactor = 0.5,	weight = 3, color = "black") %>%
		addCircleMarkers(data = exsitu, lng = ~long_round, lat = ~lat_round,
			#popup = ~paste("Ex situ institution:",institution,"<br/>",
			#	"Lat-long source:",gps_det,"<br/>","Collection year:",aqu_year,"<br/>",
			#	"Accession number:",acc_no),
			#popup = ~paste("Ex situ:", Pop),
			radius = 4, fillOpacity = 0.7, stroke = F, color = "black")# %>%
		#addControl(title, position = "topright") %>%
		#addControl("Click on points to see more information",position = "topleft") %>%
		#addLegend(labels = c("In situ","Ex situ"), colors = c("red","black"),
		#	title = "Key", position = "topright", opacity = 0.5)
	return(map)
}

map.leaflet(exsitu_proj,dist_proj,eco_proj)
