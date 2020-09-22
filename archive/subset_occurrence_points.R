setwd("./Desktop/GA2")

### LOAD PACKAGES ###
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(rgdal)
library(geosphere)
library(sp)
library(rgeos)
library(maptools)


occur_all <- read.csv("occurrence_compiled/occurrence_compiled_refined_8_27_ArcGIS.csv", as.is=T, na.strings=c("","NA"))
nrow(occur_all) #318464

#occur_all <- occur_all[,c(1,5:47,49:52,54:66,71:73,75,88:91)]

# make a copy of exsitu records to add back in after next set of removals
exsitu_add <- occur_all[which(occur_all$dataset == "exsitu"),]
  nrow(exsitu_add) #12230
#write.csv(exsitu_add,"final_exsitu_data.csv")

# remove points with fewer than 2 digits after the decimal for lat and/or long
occur_dec2 <- occur_all[grep("\\.[0-9][1-9]",occur_all$decimalLatitude),]
  nrow(occur_dec2) #755966
occur_dec2 <- occur_dec2[grep("\\.[0-9][1-9]",occur_dec2$decimalLongitude),]
  nrow(occur_dec2) #685508

# round lat and long to specific # of digits after decimal
occur_dec2$lat_round3 <- round(occur_dec2$decimalLatitude, 3)
occur_dec2$long_round3 <- round(occur_dec2$decimalLongitude, 3)
occur_dec2$lat_round2 <- round(occur_dec2$decimalLatitude, 2)
occur_dec2$long_round2 <- round(occur_dec2$decimalLongitude, 2)
occur_dec2$lat_round1 <- round(occur_dec2$decimalLatitude, 1)
occur_dec2$long_round1 <- round(occur_dec2$decimalLongitude, 1)

# count number of duplicates per unique occurrence
freq <- plyr::count(occur_dec2,vars = c("species_name_acc","lat_round2","long_round2"))
occur_dec2 <- join(freq,occur_dec2,by=c("species_name_acc","lat_round2","long_round2"),type="right")
# remove spatial duplicates based on species key and lat/long rounded to 2 digits after the decimal
occur_unq1 <- occur_dec2 %>% dplyr::distinct(species_name_acc,lat_round2,long_round2,.keep_all=TRUE)
  nrow(occur_unq1) #330684
  unique(occur_unq1$gps_determ) # "G" "C" "L"

write.csv(occur_unq1, "occurrence_compiled/occurrence_compiled_dec2_unique_8_28.csv", row.names = F)

##########################################
### 3. Remove Records Outside State Range
##########################################

# spatially join occurrence pt df and counties shapefile to assign all points a county & state
  # load shapefile of US county boundaries
  counties_map <- readOGR('tl_2018_us_county/tl_2018_us_county.shp')
  # project to WGS84
  wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  counties_wgs <- spTransform(counties_map, wgs84)
  cent <- data.frame(gCentroid(counties_wgs,byid=T)); str(cent)
    setnames(cent,
           old=c("x","y"),
           new=c("centroid_long","centroid_lat"))
  counties_wgs <- spCbind(counties_wgs,cent)
  # turn occurrence point data into a SpatialPointsDataFrame
  #occur_unq1 <- occur_unq1[,c(1:43,47:73)]
  occur_unq_short <- occur_unq1[,1:3]
  coordinates(occur_unq_short) <- c("long_round2", "lat_round2")
  proj4string(occur_unq_short) <- wgs84
  # spatial join of occurrence points to counties shapefile
  pt_poly <- sp::over(occur_unq_short, counties_wgs)
  # add centroids
  # cbind
  occur_unq <- cbind(occur_unq1,pt_poly); nrow(occur_unq) #330684; 355191

  # remove points that do not have a centroid (out of bounds)
  occur_clean <- occur_unq[which(!is.na(occur_unq$centroid_lat)),]; nrow(occur_clean)
  occur_clean <- occur_clean[which(!is.na(occur_clean$centroid_long)),]; nrow(occur_clean)

# replace abbreviations with number identifier
#to_this <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27",
#            "28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54",
#            "55","56")
#change_it <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN",
#              "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
#              "WV","WI","WY")
#replace <- c("(P1)","(P1?)","(P1P2)","(P1P3)","(P2)","(P2?)","(P2P3)","(P2P4)","(P3)",
#             "(P3?)","(P3P4)","(P3P5)","(P4)","(P4?)","(P4P5)","(P5)","(P5?)","(PH)",
#             "(PNA)","(PNR)","(PU)","(S1)","(S1?)","(S1S2)","(S1S3)","(S2)","(S2?)",
#             "(S2S3)","(S2S4)","(S3)","(S3?)","(S3S4)","(S3S5)","(S4)","(S4?)","(S4S5)",
#             "(S5)","(S5?)","(SH)","(SNA)","(SNR)","(SU)","(S1.1)")
# this loop finds all cases of the characters in the change_it vector and replaces
# them with the characters in the to_this vector
#occur_unq$state_dist <- occur_unq$US_STATES_AND_S_RANKS
#  for (i in 1:length(to_this)){
#    occur_unq$state_dist <- gsub(pattern = change_it[i], x = occur_unq$state_dist, replacement = to_this[i])
#  }

# mark records that are inside known state range
#occur_unq$within_range <- "NA"
#occur_unq$STATEFP <- as.character(occur_unq$STATEFP)
#occur_unq <- occur_unq[which(!is.na(occur_unq$STATEFP)),]; nrow(occur_unq) #319823
#occur_unq$STATEFP <- paste(occur_unq$STATEFP,"(",sep="")
#  unique(occur_unq$STATEFP)
#occur_marked <- occur_unq
#for (i in 1:nrow(occur_marked)){
#  if (length(grep(pattern=paste(occur_marked$STATEFP[i],"\\","(",sep=""), x=occur_marked$state_dist[i])) > 0){
#    occur_marked$within_range[i] <- "Y"
#  }
#}
#table(occur_marked$within_range)
# NA   Y
# 1359 318464

# remove marked Records
#occur_clean <- occur_marked[which(occur_marked$within_range == "Y"),]

write.csv(occur_clean, "occurrence_compiled/occurrence_compiled_dec2_unique_8_28.csv", row.names = F)
#write.csv(occur_marked, file=paste0(compiled, "/occurrence_compiled_dec2_unique_ctyDupMarked_statesMarked.csv"), row.names = F)

###########################################
### 4. Remove Spatial Duplicates by County
###########################################

# mark records that may be centroids based on matching coordinates to centroid
#occur_clean$centroid_lat[is.na(occur_clean$centroid_lat)] <- 0

for(i in 1:nrow(occur_clean)){
    if((round(occur_clean$centroid_lat[i], 1) == occur_clean$lat_round1[i]) &
    (round(occur_clean$centroid_long[i], 1) == occur_clean$long_round1[i]) &
    occur_clean$gps_determ[i] == "G"){
      occur_clean$gps_determ[i] <- "SC"
    }
  }
table(occur_clean$gps_determ)
# C     G      L    SC
# 21697 277606 449  18712

# create df of unique records
unique_rec <- occur_clean %>% distinct(species_name_acc,COUNTYNS,.keep_all=TRUE); nrow(unique_rec) #29218
# create df of records we will keep whether they are county-level duplicates or not
keep <- occur_clean[which(occur_clean$gps_determ != "C" & occur_clean$gps_determ != "SC"),]; nrow(keep) #278055
# join unique values with ones we want to keep
occur_clean <- join(unique_rec, keep, type="full"); nrow(occur_clean) #287116

# add exsitu data back in
occur_clean2 <- join(occur_clean, exsitu_add, type="full"); nrow(occur_clean2) #299346

# And write another file without the county duplicates
write.csv(occur_clean2, "occurrence_compiled/occurrence_compiled_dec2_unique_dupRemoved_8_28.csv", row.names = F)

# make smaller dataset for testing
torreyana <- occur_clean2[which(occur_clean2$species_name_acc == "Pinus torreyana"),]; nrow(torreyana) #211
write.csv(torreyana, "occurrence_compiled/occurrence_compiled_torreyana_test3.csv", row.names = F)
