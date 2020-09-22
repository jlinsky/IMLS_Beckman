############ 1.3
########### 5.30.18 Elizabeth Tokarz & 5.10.19 Emily Beckman
############# refine locality information provided for each occurrence

## Be sure to run "set_workingdirectory.R" before running this script

############### INPUT: all_occurrence_compiled.csv
############### OUTPUT:

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
library(anchors)


## Match up column headers, keeping ALL columns instead of just matching ones (fills added columns with NAs)
# SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}

###################
## 1. Read in Data
###################

all_data <- read.csv("occurrence_compiled/all_data_raw_8_26.csv", as.is=T, na.strings=c("","NA"),colClasses="character",fileEncoding="latin1") # replace any empty or "NA" cells with NA so the rest of the script works smoothly
  nrow(all_data) #865872
  table(all_data$gps_determ)
  # C    G       L     NT    T
  # 252  687189  1014  3416  454

# select desired columns and drop the rest
df <- subset(all_data, select = c(sp_full_name,species_name_acc,dataset,family,genus,species,infra_rank,infra_name,genus_species,
                                  acceptedScientificName,speciesKey,taxonomicStatus,
                                  orig_list,fia_code,ref,
                                  nativeDatabaseID,basisOfRecord,catalogNumber,collectionCode,institutionCode,datasetName,recordNumber,recordedBy,year,FIPS,
                                  coordinatePrecision,coordinateUncertaintyInMeters,gps_determ,georeferenceRemarks,hasGeospatialIssues,extirpated,
                                  country,countryCode,stateProvince,higherGeography,county,decimalLatitude,decimalLongitude,fieldNotes,verbatimLocality,
                                  habitat,locality,locationRemarks,municipality,occurrenceRemarks,individualCount,occurrenceStatus))

# create data frame of points with lat-long already
orig <- df[which(!is.na(df$decimalLatitude) & !is.na(df$decimalLongitude)),]; nrow(orig) #688207
#write.csv(orig,"occurrence_compiled/df_DC_orig.csv")

# set names for columns used in rest of script
df <- setnames(df,
         old=c("decimalLatitude","decimalLongitude","stateProvince"),
         new=c("lat","long","state"))

# replace non-alphanumeric characters and other misspellings/formatting errors
sort(unique(df$state))
search_list <- c("Californie","Caliornia","Caroline Du Nord","Claifornie","Colordao",
               "Conneticut","Georgie","Illilois","Illinoi","Illlinois","Kalifornia",
               "Louisana","Louisiane","Lousiana","Massachusets","Massachussets","Michgan",
               "Miss.","Missoui","Pensilvania","Pensylvania","South c","Tenn.",
               "Uath","W. Virginie","Washington D.C.","Washington DC","Washington, D.C.",
               "Mississippissippi","Mississippiuri","missoui","N.C.","Wiconsin","Nouveau Mexique",
               "Arizonica","Carolina del norte","Carolina do Norte","Colorad","Illinoiss","Miane",
               "New nexico","Tennesseesse","Tennesseessee")
replace_list <- c("California","California","North Carolina","California","Colorado",
               "Connecticut","Georgia","Illinois","Illinois","Illinois","California",
               "Louisiana","Louisiana","Louisiana","Massachusetts","Massachusetts","Michigan",
               "Mississippi","Missouri","Pennsylvania","Pennsylvania","South Carolina","Tennessee",
               "Utah","West Virginia","District of Columbia","District of Columbia","District of Columbia",
               "Mississippi","Mississippi","Missouri","NC","Wisconsin","New Mexico",
               "Arizona","North Carolina","North Carolina","Colorado","Illinois","Maine",
               "New Mexico","Tennessee","Tennessee")
  for (i in 1:length(search_list)){
    df$state <- str_replace_all(df$state,search_list[i],replace_list[i])
  }

df$state <- str_replace_all(df$state, "[^[:alnum:]]", " "); sort(unique(df$state))
df$county <- str_replace_all(df$county, "[^[:alnum:]]", " ")

change_it <- c("Du Page","DoÃ a Ana","Chesapeake","Virginia Beach","James",
                 "Newport News","Queen Anne","Saint Marys","Saint Mary s","O Brien","Prince George",
                 "Saint","St  ","St ","John s","Mary s","Marys","Johns","Santa BÃ rbara",
                 "San Bernadino","Petersburg","Yakutat","Sainte","Batan","Baton",
                 "Francois","Claire","Ogelthorpe","Oglethorp","John'son","  ")
to_this <- c("DuPage","Dona Ana","Chesapeake city","Virginia Beach city","James City",
                  "Newport News city","Queen Anne's","St. Mary's","St. Mary's","O'Brien","Prince George's",
                  "St.","St. ","St. ","John's","Mary's","Mary's","John's","Santa Barbara",
                  "San Bernardino","Wrangell-Petersburg Census Area","Yakutat Borough","St.","West Baton Rouge","West Baton Rouge",
                  "Francis","Clair","Oglethorpe","Oglethorpe","Johnson"," ")
for (i in 1:length(to_this)){
  df$county <- gsub(pattern = change_it[i], x = df$county, replacement = to_this[i])
}; sort(unique(df$county))

#write.csv(df,"occurrence_compiled/df_DC_raw.csv", row.names = F)

###########################
## 2. Fill Locality Column
###########################

# see how many rows are missing locality data
sum(is.na(df$locality)) #714746
# concatenate other columns potentially containing locality information to a new locality column
df$locality_orig <- df$locality
df$locality[!is.na(df$verbatimLocality)] <- paste(df$locality[!is.na(df$verbatimLocality)], df$verbatimLocality[!is.na(df$verbatimLocality)], sep = "; ")
df$locality[!is.na(df$occurrenceRemarks)] <- paste(df$locality[!is.na(df$occurrenceRemarks)], df$occurrenceRemarks[!is.na(df$occurrenceRemarks)], sep = "; ")
df$locality[!is.na(df$locationRemarks)] <- paste(df$locality[!is.na(df$locationRemarks)], df$locationRemarks[!is.na(df$locationRemarks)], sep = "; ")
df$locality[!is.na(df$habitat)] <- paste(df$locality[!is.na(df$habitat)], df$habitat[!is.na(df$habitat)], sep = "; ")
df$locality[!is.na(df$municipality)] <- paste(df$locality[!is.na(df$municipality)], df$municipality[!is.na(df$municipality)], sep = "; ")
df$locality[!is.na(df$higherGeography)] <- paste(df$locality[!is.na(df$higherGeography)], df$higherGeography[!is.na(df$higherGeography)], sep = "; ")
df$locality[!is.na(df$fieldNotes)] <- paste(df$locality[!is.na(df$fieldNotes)], df$fieldNotes[!is.na(df$fieldNotes)], sep = "; ")
df$locality[!is.na(df$state)] <- paste(df$locality[!is.na(df$state)], df$state[!is.na(df$state)], sep = "; ")
df$locality[!is.na(df$county)] <- paste(df$locality[!is.na(df$county)], df$county[!is.na(df$county)], sep = "; ")

#######################
## 3. Fill State Column
#######################

# carry on only with rows without lat-long points
df2 <- df[which(is.na(df$lat) | is.na(df$long)),]; nrow(df2) #177670
merge_at_end <- anti_join(df,df2); nrow(merge_at_end)

# make a new column to preserve original state column
df2$state_new <- NA

# function to write in the appropriate state for each row in a new state column, by searching within locality column
extract_state <- function(d.f, loc, repl, case, d.f_col){
  df_s_na <- which(is.na(d.f$state_new))
  rows <- grep(pattern = loc, x = d.f_col, ignore.case = case)#, fixed=T)
  overlap <- intersect(df_s_na, rows)
  d.f$state_new[overlap] <- repl
  return(d.f$state_new)
}

# create lists of all states and abbreviations
# switch Virginia and West Virginia, otherwise Virginia will always replace West Virginia
state_names <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
                "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
                "Vermont","West Virginia","Washington","Virginia","Wisconsin","Wyoming","District of Columbia")
state_abb <- c("^AL$","^AK$","^AZ$","^AR$","^CA$","^CO$","^CT$","^DE$","^FL$","^GA$","^HI$","^ID$","^IL$","^IN$","^IA$","^KS$","^KY$","^LA$","^ME$","^MD$","^MA$","^MI$","^MN$","^MS$",
              "^MO$","^MT$","^NE$","^NV$","^NH$","^NJ$","^NM$","^NY$","^NC$","^ND$","^OH$","^OK$","^OR$","^PA$","^RI$","^SC$","^SD$","^TN$","^TX$","^UT$","^VT$","^WV$","^WA$",
              "^VA$","^WI$","^WY$","^DC$")
state_abb2 <- c(" AL "," AK "," AZ "," AR "," CA "," CO "," CT "," DE "," FL "," GA "," HI "," ID "," IL "," IN "," IA "," KS "," KY "," LA "," ME "," MD "," MA "," MI "," MN "," MS ",
              " MO "," MT "," NE "," NV "," NH "," NJ "," NM "," NY "," NC "," ND "," OH "," OK "," OR "," PA "," RI "," SC "," SD "," TN "," TX "," UT "," VT "," WV "," WA ",
              " VA "," WI "," WY "," DC ")

# see how many rows are missing state data
sum(is.na(df2$state_new)) #209345
# find and fill in the state names
for (i in 1:length(state_names)){
    df2$state_new <- extract_state(df2, state_names[i], state_names[i], T, df2$state)
  }
  sum(is.na(df2$state_new)) #58931 rows are still blank
  #unique(sort(paste(df2$state,df2$state_new,sep=" ; ")))
# look for state abbreviations
for (i in 1:length(state_names)){
  df2$state_new <- extract_state(df2, state_abb[i], state_names[i], T, df2$state)
}
  sum(is.na(df2$state_new)) #33580 rows are still blank
# find and fill in the state names ( DOES ignore.case )
for (i in 1:length(state_names)){
    df2$state_new <- extract_state(df2, state_names[i], state_names[i], T, df2$locality)
  }
  sum(is.na(df2$state_new)) #32403 rows are still blank
# look for state abbreviations ( does NOT ignore.case )
for (i in 1:length(state_names)){
  df2$state_new <- extract_state(df2, state_abb2[i], state_names[i], F, df2$locality)
}
  sum(is.na(df2$state_new)) #32286 rows are still blank

# check rows still missing state_new values
sort(unique(df2[is.na(df2$state_new), "state"]))

########################
## 4. Fill County Column
########################

# make a new column to preserve original county column
df2$county_new <- NA
# read in county and state vectors from the FIA county reference file from the FIA datamart
fia_cou <- read.csv('fia_county_raw.csv', as.is=T)
distinct_cou <- read.csv('fia_distinct_counties.csv', as.is=T)
# these two vectors contain all the counties in the US
cou_state_names <- fia_cou$STATENM
cou_county_names <- fia_cou$COUNTYNM
    #cou_county_names <- str_replace_all(fia_cou$COUNTYNM, "[^[:alnum:]]", " ")
# these two have all the distinct counties
cou_state_distinct <- distinct_cou$state
cou_county_distinct <- distinct_cou$county
    #cou_county_distinct <- str_replace_all(distinct_cou$county, "[^[:alnum:]]", " ")

# make a function for distinct counties (only in one state)
extract_county_distinct <- function(d.f, state_loc, loc, retur){
  df_c_look <- which(is.na(d.f$county_new))
  rows <- grep(pattern = loc, x = d.f$county, ignore.case = T)
  overlap <- intersect(df_c_look, rows)
  d.f$county_new[overlap]  <- loc
  d.f$state_new[overlap] <- state_loc
    df_c_look <- which(is.na(d.f$county_new))
    rows <- grep(pattern = loc, x = d.f$state, ignore.case = T)
    overlap <- intersect(df_c_look, rows)
    d.f$county_new[overlap]  <- loc
    d.f$state_new[overlap] <- state_loc
      df_c_look <- which(is.na(d.f$county_new))
      rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)
      overlap <- intersect(df_c_look, rows)
      d.f$county_new[overlap]  <- loc
      d.f$state_new[overlap] <- state_loc
  if(retur == 1){
    return(d.f$state_new)
  } else {
    return(d.f$county_new)
  }
}

for (i in 1:length(cou_state_distinct)){
  df2$county_new <- extract_county_distinct(df2, cou_state_distinct[i], cou_county_distinct[i], 0)
  df2$state_new <- extract_county_distinct(df2, cou_state_distinct[i], cou_county_distinct[i], 1)
  print(cou_county_distinct[i])
}

# make a function to search through the state, county and locality data to look for matches
# in this function, we consider the state and county as a pair
# the county and correlating state will always be entered as two of the arguments
extract_county <- function(d.f, state_loc, loc){
  # first to make sure that the county matches the state, we will only consider occurrences in the state half of the pair
  df_c_look <- which(d.f$state_new == state_loc)
  # often the county was already mentioned in the county column, so first we check the county column for the county name in the current pair
  rows <- grep(pattern = loc, x = d.f$county, ignore.case = T)#, fixed=T)
  # next we see which row numbers are also in the state listed in the argument
  overlap <- intersect(df_c_look, rows)
  # and then only for the overlapping rows do we write the county name into the new county column
  d.f$county_new[overlap] <- loc
    # look in state column
    df_c_look <- which(d.f$state_new[is.na(d.f$county_new)] == state_loc)
    rows <- grep(pattern = loc, x = d.f$state, ignore.case = T)#, fixed=T)
    overlap <- intersect(df_c_look, rows)
    d.f$county_new[overlap] <- loc
      # look in locality column
      df_c_look <- which(d.f$state_new[is.na(d.f$county_new)] == state_loc)
      rows <- grep(pattern = loc, x = d.f$locality, ignore.case = T)#, fixed=T)
      overlap <- intersect(df_c_look, rows)
      d.f$county_new[overlap] <- loc
  return(d.f$county_new)
}

# to save time, only look through rows that have a state_new value
df3 <- df2[which(!is.na(df2$state_new)),]; nrow(df3)
merge_at_end_2 <- anti_join(df3,df2)

# fill in counties with this loop; it might take 5-10 minutes to run
for (i in 1:length(cou_state_names)){
  df3$county_new <- extract_county(df3, cou_state_names[i], cou_county_names[i])
  print(cou_county_names[i])
}
# compare how many counties were NA within the original column and the new column
sum(is.na(df3$county)) #81714
sum(is.na(df3$county_new)) #105738

write.csv(df3, "occurrence_compiled/df_DC_working_8_26.csv", row.names = F)

#df2 <- read.csv("occurrence_compiled/df_DC_working.csv", as.is=T, na.strings=c("","NA"),colClasses="character",fileEncoding="latin1") # replace any empty or "NA" cells with NA so the rest of the script works smoothly

# take a look at the remaining rows with NAs in the new county column
unique(df3[which(is.na(df3$county_new) & !is.na(df3$county) & !is.na(df3$state_new)), c("county","state_new")])

#df3$state_new[which(is.na(df3$county_new) & df3$state_new == "Mississippi")] <- "Missouri"

# because of typos in the FIA document or the df2 entries, we can fill in some blanks:
# the below vectors can be adjusted based on the above results
#county_counties <- c("DeSoto", "De Kalb", "Saint Clair", "DE BACA", "De Baca", "De Soto", "Cockran",
#                      "Oglethorp Co.", "Saint Johns", "Saint Lucie", "Saint Louis", "San Bernadino","Oglethorp County","Santa Catarina","Santa Rosa")
#county_states <- c("Louisiana", "Georgia", "Alabama", "New Mexico", "New Mexico", "Florida", "Texas",
#                    "Georgia", "Florida", "Florida", "Missouri", "California","Georgia","California","California")
#county_replacements <- c("DeSoto", "DeKalb", "St. Clair", "De Baca", "De Baca", "DeSoto",
#                          "Cochran", "Oglethorpe", "St. Johns", "St. Lucie", "St. Louis", "San Bernardino","Oglethorpe","Los Angeles","Santa Barbara")
# this loop simply writes the county_replacement data to the new county column
# for rows matching both the county_counties and county_states data in the original state and county columns
#for (i in 1:length(county_counties)){
#df2$county_new[which(df2$county==county_counties[i] & df2$state==county_states[i])] <- county_replacements[i]
#}
#sum(is.na(df2$county_new)) #96518

# join everything back together
df_all <- rbind.all.columns(df3,merge_at_end)
df_all <- rbind(df_all,merge_at_end_2)

# add observation number
df_all$obs_no <- seq(1, length(df_all$basis), 1)
# rename to Darwin Core
setnames(df_all,
         old=c("lat","long","state"),
         new=c("decimalLatitude","decimalLongitude","stateProvince"))
# save this file
write.csv(df_all, "occurrence_compiled/df_DC_cleaned_8_27.csv", row.names = F)
#write.csv(df, file='all_occ_counties_filled.csv', row.names = F)

###########################
## 5. Fill County Centroids
###########################

df <- read.csv("occurrence_compiled/df_DC_cleaned_8_27.csv", as.is=T, na.strings=c("","NA")) # replace any empty or "NA" cells with NA so the rest of the script works smoothly
  #df <- df[,2:56]

# Make dataframe of county centroids
  # load shapefile of US county boundaries
  counties_map <- readOGR('tl_2018_us_county/tl_2018_us_county.shp')
  # project to WGS84
  wgs84 <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  counties_wgs <- spTransform(counties_map, wgs84)
  # create dataframe of county centroids (lat and long)
  centroids <- as.data.frame(centroid(counties_wgs))
  colnames(centroids) <- c("centroid_long", "centroid_lat")
  # round centroid lat and long to 2 digits after decimal
  centroids$centroid_long <- round(centroids$centroid_long, 2)
  centroids$centroid_lat <- round(centroids$centroid_lat, 2)
  # add county names to data frame
    # remove the 0s from the county FP codes by making the factors characters, then numeric, then factors again.
    centroids$STATECD <- as.factor(as.numeric(as.character(counties_wgs$STATEFP)))
    centroids$COUNTYCD <- as.factor(as.numeric(as.character(counties_wgs$COUNTYFP)))
  fia_cou$STATECD <- as.factor(fia_cou$STATECD)
  fia_cou$COUNTYCD <- as.factor(fia_cou$COUNTYCD)
  # Luckily these state and county codes align with the fia_county file from above
  # So let's tack on the names to these coordinates with fia_cou
  centroids <- join(centroids, fia_cou, by = c("STATECD", "COUNTYCD"), type = "left")
  # It looks like some numbers are still not quite aligning, there may be errors in states with a lot of counties--VA and AK and FL (Dade)
  centroids <- subset(centroids, select = c(centroid_lat,centroid_long,STATENM,COUNTYNM))
    str(centroids)

# Join county centroid lat/long to main df of occurrences
  colnames(df)[colnames(df)=="county_new"] <- "COUNTYNM"
  colnames(df)[colnames(df)=="state_new"] <- "STATENM"
    str(df); nrow(df); table(df$gps_determ)
  df_join <- join(df, centroids, by = c("STATENM", "COUNTYNM"), type = "left", match = "first")
    str(df_join); nrow(df_join); table(df_join$gps_determ)
    # C      G      L     NP      T
    # 228 687433   1029   1308    824
# Write in county centroid coordinates and label them with a C
df_join$gps_determ <- gsub("T",NA,df_join$gps_determ)
replace_latlong <- df_join[which(!is.na(df_join$centroid_lat) & is.na(df_join$gps_determ)),]; nrow(replace_latlong) #151046
no_replace <- df_join[which(!is.na(df_join$gps_determ)),]; nrow(no_replace) #689998
    replace_latlong$decimalLatitude <- replace_latlong$centroid_lat
    replace_latlong$decimalLongitude <- replace_latlong$centroid_long
    replace_latlong$gps_determ <- "C"
df_join2 <- join(replace_latlong,no_replace,type="full"); nrow(df_join2) #841044
    table(df_join2$gps_determ)
      # C      G        L       NP
      # 151274 687433   1029   1308

# Join centroids dataframe to points with orig lat-long
#orig <- read.csv("occurrence_compiled/df_DC_orig.csv", as.is=T, na.strings=c("","NA")) # replace any empty or "NA" cells with NA so the rest of the script works smoothly
#  nrow(orig); table(orig$gps_determ) #688207
  # G        L
  # 687188   1019
#df_join2 <- join(df_join,orig,type="full"); nrow(df_join2) #865872
#  table(df_join2$gps_determ)
  # C      G      L     NT    T
  # 146768 687188 1019  919   246

write.csv(df_join2, "occurrence_compiled/df_DC_cleaned_joined_8_27.csv", row.names = F)

#####################
## 6. Finishing Touches
#####################

occur_all_orig <- read.csv("occurrence_compiled/df_DC_cleaned_joined_8_27.csv", as.is=T, na.strings=c("","NA")) # replace any empty or "NA" cells with NA so the rest of the script works smoothly
occur_all <- occur_all_orig

# lat/long as numeric
occur_all$decimalLatitude<-as.numeric(occur_all$decimalLatitude)
occur_all$decimalLongitude<-as.numeric(occur_all$decimalLongitude)

# make some changes across this dataset to prevent future errors
occur_all$year[is.na(occur_all$year)]<-"1111"
replace <- c("UNKNOWN","\\<0\\>","N/A","NA","^$","is.na(i)")
for (i in replace){
  occur_all$year <- gsub(i,"1111",occur_all$year)
}
occur_all$year<-as.numeric(occur_all$year)
occur_all$locality<-gsub(",",".",occur_all$locality)

# fix year column
occur_all <- occur_all[order(occur_all$year, na.last = TRUE, decreasing = T),]
unique(occur_all$year)
  occur_all$year <- gsub(pattern = "ca.", x = occur_all$year, replacement = ""); unique(occur_all$year)
  occur_all$year <- gsub(pattern = "original", x = occur_all$year, replacement = "1111"); unique(occur_all$year)
  occur_all$year[is.na(occur_all$year)] <- "1111"; unique(occur_all$year)
    for(i in 1:nrow(occur_all)){
      if(nchar(occur_all$year[i]) == 8){
        occur_all$year[i] <- substr(occur_all$year, 4, 8)
      }
      else if(nchar(occur_all$year[i]) == 5){
        occur_all$year[i] <- substr(occur_all$year, 4, 5)
      }
    }; unique(occur_all$year)
  occur_all$year <- as.numeric(occur_all$year)
    for(i in 1:nrow(occur_all)){
      if(occur_all$year[i] < 10){
        occur_all$year[i] <- occur_all$year[i] + 2000
      }
      else if(occur_all$year[i] < 100){
        occur_all$year[i] <- occur_all$year[i] + 1900
      }
    }; unique(occur_all$year)

occur_all$year <- gsub(pattern = "^62016$", x = occur_all$year, replacement = "2016")
occur_all$year <- gsub(pattern = "^198$", x = occur_all$year, replacement = "1998")
occur_all$year <- gsub(pattern = "^164$", x = occur_all$year, replacement = "1964")
occur_all$year <- gsub(pattern = "^200$", x = occur_all$year, replacement = "2000")
occur_all$year <- gsub(pattern = "^9999$", x = occur_all$year, replacement = "1111")
occur_all$year <- gsub(pattern = "^2028$", x = occur_all$year, replacement = "1111")
occur_all$year <- gsub(pattern = "^1000$", x = occur_all$year, replacement = "1111")
sort(unique(occur_all$year))

# create dataset of ex situ records only
exsitu <- occur_all[which(occur_all$dataset == "exsitu"),]; nrow(exsitu) #17455
write.csv(exsitu, "exsitu_working/GA2_exsitu_compiled_GEOLOCATED_8_27.csv")

# remove rows with no lat and long still
occur_all <- df_join2[which(!(is.na(df_join2$decimalLatitude)) & !is.na(df_join2$decimalLongitude)),]; nrow(occur_all) #834966
  unique(occur_all$gps_determ)

# switch lat and long values if lat is greater than 50; make sure coordinates have correct +/- sign
for (row in 1:nrow(occur_all)){
  temp <- occur_all$decimalLatitude[row]
  if (temp > 50 || temp < -50){
    occur_all$decimalLatitude[row] <- occur_all$decimalLongitude[row]
    occur_all$decimalLongitude[row] <- temp
    print("switch")
  }
  if(occur_all$decimalLongitude[row] > 0){
    occur_all$decimalLongitude[row] <- occur_all$decimalLongitude[row]*(-1)
    print("pos")
  }
  if(occur_all$decimalLatitude[row] < 0){
    occur_all$decimalLatitude[row] <- abs(occur_all$decimalLatitude[row])
    print("neg")
  }
  if(occur_all$decimalLatitude[row] < -90 || occur_all$decimalLatitude[row] > 90 ||
  occur_all$decimalLongitude[row] < -180 || occur_all$decimalLongitude[row] > 180){
    occur_all$decimalLatitude[row] <- NA
    occur_all$decimalLongitude[row] <- NA
    print("out of bounds")
  }
}

# remove lat-long points which were out of bounds
nrow(occur_all) #834966
occur_all <- occur_all[which(!is.na(occur_all$decimalLatitude) & !is.na(occur_all$decimalLongitude)),]; nrow(occur_all) #833901

# reorder dataset before subsetting in next script to place higher quality datasets and most recent records first
occur_all <- setorder(occur_all,"decimalLatitude","decimalLongitude")
unique(occur_all$dataset)
occur_all <- occur_all[order(factor(occur_all$dataset,levels=c("fia","idigbio","consortium","gbif","exsitu",
                                                               "geolocate_comm","usda_plants","bonap",NA))),]
# join to new species list
sp_list <- read.csv("species_list_joined_8_23_2.csv", as.is=T)
occur_all <- occur_all[,c(1:3,5,18:53)]
occur_all2 <- plyr::join(occur_all,sp_list,type="left"); nrow(occur_all) #838430

# print percent of rows which are filled within each column
  # remove columns if they are entirely empty
for(i in 1:ncol(occur_all2)){
  t <- nrow(occur_all2[which(!is.na(occur_all2[,i])),])
  print(paste(colnames(occur_all2[i]),round(t/nrow(occur_all2)*100,2)))
}

# write file
write.csv(occur_all2, "occurrence_compiled/occurrence_compiled_refined_8_27.csv")

# make shorter version for ArcGIS
occur_all3 <- occur_all2[,c(1:4,6,10,13:14,17,19:20,26:27,31,35,38,42:43)]

# write file
write.csv(occur_all3, "occurrence_compiled/occurrence_compiled_refined_8_27_ArcGIS.csv")






### NOT USED RIGHT NOW ###
# create subset to use for GeoLocate
geoloc <- occur_all[which(occur_all$dataset=="gbif" | occur_all$dataset=="consortium"),]
nrow(geoloc) #17263
write.csv(geoloc, file=paste0(compiled, "/occurrence_compiled_refined_for_geolocate.csv"))
gbif_geo <- occur_all[which(occur_all$dataset=="gbif"),]
nrow(gbif_geo) #12195
#write.csv(gbif_geo, file=paste0(compiled, "/occurrence_compiled_for_geolocate.csv"))
diff <- setdiff(occur_all$obs_no,geoloc$obs_no)
the_rest <- occur_all[diff,]
nrow(the_rest) #50974
write.csv(the_rest, file=paste0(compiled, "/occurrence_compiled_refined_no_geolocate.csv"))

test <- the_rest[which(is.na(the_rest$gps_determ) & !is.na(the_rest$decimalLatitude)),]


### THIS PART DOES NOT APPLY CURRENTLY BECAUSE ALL POINTS ARE NOT RUN THROUGH GEOLOCATE ###
# Now let's see how many of the coordinates we had before have changed.
# sum(geo_loc$latitude==post_geo$latitude) #NA  Wow--everything has changed...This
# means that some of our pre-existing coordinates that were uploaded into GeoLocate were changed.
# We may need to change some back, but we also should be aware that some of those were
# associated with certain issues that could have been coordinate-related.

# find which coordinates existed in the input document
# pre_filled <- which(!is.na(geo_loc$latitude))
# Recognize that pre_existing coordinates were probably more precise than the
# geolocated coordinates, unless an issue was designated to it. We will want to
# retain the pre-existing coordinates that had no issues
# Remove from this above vector the coordinates that had issues.
# problematic issues here would be "COORDINATE_ROUNDED" and "COORDINATE_MISMATCH")
#### for some reason there are no issues, even in the original df file imported at very beginning of script
#rounded <- grep(pattern = "COORDINATE_ROUNDED", pre_geo$issue)
#mismatch <- grep(pattern = "COORDINATE_MISMATCH", post_geo_succ$issue)
#problems <- union(rounded, mismatch)
# We can make a vector of the so-called issueless rows, in which we trust the
# pre-filled coordinates more than the GeoLocate output. From our vector of the
# numbers of the pre-filled rows, remove all row numbers that had the issues described above.
#issueless <- post_geo_succ[-problems]
#nrow(issueless) # 3660
# Now replace the "issueless rows with their pre-existing coordinates
# post_geo$latitude[issueless] <- geo_loc$latitude[issueless]
# post_geo$longitude[issueless] <- geo_loc$longitude[issueless]
# Now label these replaced values with a G for given in "gps_determ"
# post_geo$gps_determ[issueless] <- "G"

# join post geolocate and occurrences with a given lat-long
#post_geo_all <- rbind(pre_geo,post_geo_succ)
#nrow(post_geo_all) #16295

# At this point, some of the NA coordinates may correspond to occurrences with only state and county locality.
# Label these points as C for county in "gps_determ"
# check_sc <- post_geo[which(is.na(post_geo$gps_determ)), c("state", "county")]
#length(check_sc$state)
#sum(!is.na(check_sc))
# a <- which(!is.na(check_sc$state))
# b <- which(!is.na(check_sc$county))
# keep_these <- intersect(a,b)
## now rewrite check_sc to feature numbers
# check_sc <- which(is.na(post_geo$gps_determ))
# and write in the C for county
# post_geo[check_sc[keep_these], "gps_determ"] <- "C"

# now remove all occurrences that still have NA in gps_determ
# keep_these <- which(!is.na(post_geo$gps_determ))
# post_geo <- post_geo[keep_these, ]

# tack on an observation number here
#post_geo_all$obs_no <- seq(1, length(post_geo_all), 1)
# rename columns to match df
# post_geo <- setnames(post_geo, old=c("locality_string","latitude","longitude"), new=c("locality","lat","long"))
# Now using the updated revised_post_geo dataset and the observation numbers that
# correlate with the df rows, tack on the other necessary DarwinCore columns

#df <- read.csv(file='df_DC_cleaned.csv', as.is=T)

#post_geo_full <- merge(all_occ, df, by = "obs_no")
#nrow(post_geo_full) #12195
#str(post_geo_full)
#post_geo_full <- post_geo_full[,c(1:18,20,25,27:29,32:36)]
#str(post_geo_full)

#post_geo$county <- post_geo$county.remove
#post_geo$locality <- post_geo$locality_string
#post_geo$stateProvince <- post_geo$state
#post_geo$decimalLatitude <- post_geo$latitude
#post_geo$decimalLongitude <- post_geo$longitude
