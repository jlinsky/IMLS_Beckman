### Author: Emily Beckman  ###  Date: 10/8/2019

### DESCRIPTION:
  # This script takes a folder of CSV files representing accessions data from different institutions,
  # combines them into one dataset, and standardizes some important fields.

### INPUTS:
    # 1. Folder (standard_column_names) of CSV files whose column names have already be standardized
    #     by hand using the "Standardizing Acessions Data Fields" template in Google Drive
    # 2. Target species list (sp_list_joined.csv), created through 1_compile_species_list.R script

### OUTPUTS:
    # 1. exsitu_compiled_raw.csv
    # 2. exsitu_compiled_namesAdded.csv
    # 3. exsitu_compiled_speciesFiltered.csv
    # 4. exsitu_compiled_standardized.csv
    ## 5. exsitu_compiled_noDuplicates.csv


#################
### LIBRARIES ###
#################

library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(data.table)
library(anchors)
library(measurements)
library(textclean)
library(plyr); library(dplyr)


#################
### FUNCTIONS ###
#################

# Matches up column headers, keeping all columns, not just matching ones [stacking]
# (fills added columns with NAs)
    # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- ""
    y[, c(as.character(x.diff))] <- ""
    return(rbind(x, y))
}

# count the number of spaces in a string; takes a vector (e.g., column of a data frame)
  # and returns all rows in dataframe with spaces in strings in target column
count.spaces <- function(x) { sapply(gregexpr(" ", x), function(y) { sum(y>=0) } ) }


##############
### SCRIPT ###
##############

setwd("./Desktop/exsitu")

###############################
# 1. Stack all accessions data
###############################

# read in ex situ accessions CSV files from folder and create data frame for each
file_list <- list.files(path="./standard_column_names",pattern=".csv",full.names=TRUE); str(file_list)
file_dfs <- sapply(file_list,read.csv,header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character")
  length(file_dfs) #5
  sapply(file_dfs, nrow)

# inst_short_added (nickname I've created for each institution) column added, based on file name
for(file in seq_along(file_dfs)){
  file_dfs[[file]]$inst_short_added <- rep(file_list[file], nrow(file_dfs[[file]]))
} ### way to do this more cleanly with apply family?

# stack all datasets; 'Reduce' iterates through list of dataframes and merges with previous one in list
all_data_raw <- Reduce(rbind.all.columns, file_dfs)
  nrow(all_data_raw) #3563
  ncol(all_data_raw) #31

# check out column names
sort(colnames(all_data_raw))
all_data <- all_data_raw
# IF NEEDED: remove extra columns (sometimes created through Excel to CSV issues)
  #all_data <- all_data_raw[, -grep("^X", names(all_data_raw))]
  #str(all_data); ncol(all_data) # check schema to see if problems still exist; 96
# IF NEEDED: merge similar columns (you may not need to do this if no schema mistakes were made)
  #all_data <- tidyr::unite(all_data,"inst_short", c("inst_name","inst_short","isnt_name"),sep="",remove=T)
  #str(all_data2); ncol(all_data2) # check schema to see if problems still exist : 51
# IF NEEDED: remove unused columns / rename others
  #all_data <- all_data_raw[ , -which(names(all_data_raw) %in% c("donor"))]
  #colnames(all_data)[colnames(all_data)=="elevation"] <- "altitude"

# update inst_short_added column to contain only garden name, not filepath
all_data$inst_short_added <- sub("./standard_column_names/","",all_data$inst_short_added)
all_data$inst_short_added <- sub(".csv","",all_data$inst_short_added)
  sort(unique(all_data$inst_short_added))

# remove leading, trailing, and middle (e.g., double space) whitespace, to prevent future errors
all_data <- as.data.frame(lapply(all_data, function(x) str_squish(x)),stringsAsFactors=F) # trip whitespace

# write raw file
write.csv(all_data, "exsitu_working/exsitu_compiled_raw.csv")

######################################
# 2. Standardize species name columns
######################################

# standardize hybrid column
sort(unique(all_data$hybrid))
all_data2 <- replace.value(all_data,"hybrid",c("H","X"),"x") # make the current values all the same
  nrow(all_data2[which(all_data2$hybrid=="x"),]) #162
all_data3 <- all_data2
# look for hybrid symbols in sp_full_name column and, when found, mark in hybrid column
all_data3$hybrid[grep("_", all_data3$sp_full_name, fixed=T)] <- "x" ; nrow(all_data3[which(all_data3$hybrid=="x"),]) #316
all_data3$hybrid[grep(" x ", all_data3$sp_full_name, ignore.case=T)] <- "x" ; nrow(all_data3[which(all_data3$hybrid=="x"),]) #319
all_data3$hybrid[grep("hybrid", all_data3$sp_full_name, fixed=T)] <- "x" ; nrow(all_data3[which(all_data3$hybrid=="x"),]) #348
all_data3$hybrid[grep("×", all_data3$sp_full_name)] <- "x" ; nrow(all_data3[which(all_data3$hybrid=="x"),]) #348
  all_data3[which(all_data3$hybrid=="x"),]$sp_full_name
# look for hybrid symbols in species column and, when found, mark in hybrid column
all_data3$hybrid[grep(" x ", all_data3$species)] <- "x" ; nrow(all_data3[which(all_data3$hybrid=="x"),]) #356

# OPTIONAL: remove marked hybrids
  #all_data3 <- all_data3[which(is.na(all_data3$hybrid)),]; nrow(all_data3)

# preserve original species name
all_data3$sp_full_name_orig <- all_data3$sp_full_name
  sort(unique(all_data3$sp_full_name_orig))

# create concatenated sp_full_name col
all_data3$genus <- str_to_title(tolower(all_data3$genus)) # correct genus capitalization issues
all_data3 <- unite(all_data3, "sp_full_name_concat", c(genus,species,infra_rank,infra_name), sep = " ", remove = F)
all_data3$sp_full_name_concat <- mgsub(all_data3$sp_full_name_concat, c("NA "," NA"," NA"," NA"," NA"), "") # get rid of NAs
all_data3$sp_full_name_concat <- str_squish(all_data3$sp_full_name_concat) # trim whitespace
  unique(all_data3$sp_full_name_concat)

# when blank, fill sp_full_name column with concatenated species full name
all_data3$sp_full_name[all_data3$sp_full_name==""] <- all_data3$sp_full_name_concat[all_data3$sp_full_name==""]
  unique(all_data3$sp_full_name)

# NOT USING RIGHT NOW
# remove hybrid symbol in sp_full_name column
#all_data3$sp_full_name <- mgsub(all_data3$sp_full_name, c("_"," X ","hybrid","×"," x "), " ")
#all_data3$sp_full_name <- gsub("  ", " ",all_data3$sp_full_name) # replace double space with single space
#  all_data3[which(all_data3$hybrid=="x"),]$sp_full_name

# separate out species full name and trim whitespace
all_data3$sp_full_name <- mgsub(all_data3$sp_full_name, c("\'","(","\\","\"",")"), "") # replace unwanted characters
all_data3$sp_full_name <- mgsub(all_data3$sp_full_name, c("_"), "x") # replace other hybrid characters with "x"
all_data4 <- all_data3 %>% separate("sp_full_name", c("genus_new","species_new","extra1","extra2",
                                    "extra3","extra4","extra5","extra6"),sep=" ", fill="right", extra="warn")
all_data4 <- as.data.frame(lapply(all_data4, function(x) str_squish(x)),stringsAsFactors=F) # trip whitespace

# create matrix of all "extra" species name columns, to search for infraspecific key words
search.col <- matrix(cbind(all_data4$extra1,all_data4$extra2,all_data4$extra3,all_data4$extra4,all_data4$extra5,all_data4$extra6),nrow=nrow(all_data4))
# search the "extra" column matrix for matches to infraspecific key words
matches <- which(search.col=="variety"|search.col=="var"|search.col=="var."|search.col=="v"|search.col=="v."|search.col=="V"|
                 search.col=="subspecies"|search.col=="subsp"|search.col=="subsp."|search.col=="ssp"|search.col=="ssp."|
                 search.col=="forma"|search.col=="form"|search.col=="fma"|search.col=="fo"|search.col=="fo."|search.col=="f"|search.col=="f."|
                 search.col=="infra"|
                 search.col=="x",arr.ind=T)
colnames(all_data4) # look at columns to check where the "extra" ones are
# change the number added based on where the extra1 column is in your dataset
  # e.g., add 19 if extra1 is in the 20th column
matches[,2] <- matches[,2]+19
# create new infra_rank column and fill with "extra" contents that matched infraspecific key word
all_data4$infra_rank_new <- NA
all_data4$infra_rank_new[matches] <- all_data4[matches]
  unique(all_data4$infra_rank_new) # check results
# create new infra_name column and fill with next column over from "extra" contents that matched infraspecific key word
all_data4$infra_name_new <- NA
matches[,2] <- matches[,2]+1
all_data4$infra_name_new[matches] <- all_data4[matches]
sort(unique(all_data4$infra_name_new))

# standardize infraspecific rank names
all_data4$infra_rank_new  <- mgsub(all_data4$infra_rank_new, c("v."), "var.")
all_data4$infra_rank_new  <- mgsub(all_data4$infra_rank_new, c("ssp."), "subsp.")
all_data4$infra_rank_new  <- mgsub(all_data4$infra_rank_new, c("forma"), "f.")
  unique(all_data4$infra_rank_new)

# write file
write.csv(all_data4, "exsitu_working/exsitu_compiled_namesAdded.csv")

#####################################
# 3. Filter by target species names
#####################################

# read in target species list
species_list <- read.csv("IMLS_sp_list_joined_NO_cv_or_x.csv",fileEncoding="latin1",strip.white=T,colClasses="character",as.is=T,na.strings=c("","NA"))
  #species_list <- species_list[,2:length(species_list)]

# for comparison as we remove rows:
nrow(all_data4) #3563

# check genus names
sort(unique(all_data4$genus_new))
# IF NEEDED: fix misspellings
  #all_data4 <- replace.value(all_data4, "genus_new", "Carrya", "Carya")

# remove rows without genus name
all_data5 <- all_data4[which(!is.na(all_data4$genus_new)),]
  nrow(all_data5) #3563
# keep only rows from target genera
all_data5 <- all_data5 %>% filter(all_data5$genus_new %in% unique(species_list$genus))
  nrow(all_data5) #2396

# check species names
sort(unique(all_data4$species_new))

# remove rows without species name
all_data6 <- all_data5[which(!is.na(all_data5$species_new)),]
  nrow(all_data6) #2396
# keep only rows from target genus and species
all_data6$genus_species <- paste(all_data6$genus_new,all_data6$species_new); unique(all_data6$genus_species)
all_data6 <- all_data6 %>% filter(all_data6$genus_species %in% unique(species_list$genus_species))
  nrow(all_data6) #1500

# create new species full name column
all_data6$sp_full_name <- NA
yes_infra <- which(!is.na(all_data6$infra_name_new)) # rows with infraspecific name
all_data6$sp_full_name[yes_infra] <- paste(all_data6$genus_new[yes_infra],all_data6$species_new[yes_infra],all_data6$infra_rank_new[yes_infra],all_data6$infra_name_new[yes_infra],sep=" ")
  sort(unique(all_data6$sp_full_name))
all_data6$sp_full_name[-yes_infra] <- paste(all_data6$genus_new[-yes_infra],all_data6$species_new[-yes_infra],sep=" ")
  sort(unique(all_data6$sp_full_name)); nrow(all_data6) #1500

# keep only rows from target species (based on full names)
all_data7 <- all_data6 %>% filter(all_data6$sp_full_name %in% unique(species_list$sp_full_name))
  nrow(all_data7) #1374

## check to see if institutions get excluded, and manually check those files to see if issues
setdiff(unique(all_data2$inst_short_added),unique(all_data7$inst_short_added))

# join dataset to species list
all_data8 <- subset(all_data7, select = -c(extra1,extra2,extra3,extra4,extra5,extra6,genus,genus_species))
all_data8 <- plyr::join(all_data8, species_list, type = "left", match = "first")
  str(all_data8)

# write file
write.csv(all_data8, "exsitu_working/exsitu_compiled_speciesFiltered.csv")

###################################
# 4. Standardize important columns
###################################

## A) PROVENANCE TYPE

# look at column contents
sort(unique(all_data8$prov_type))
## IF NEEDED: transfer contents of one column to another column, if data needs to be preserved but is in wrong place
  #all_data8$notes[grep("Of known, direct wild origin - Florence County, SC.", all_data8$prov_type)] <- "Florence County, SC"

# standardize column by searching for keywords and replacing with standard value
  # wild (W)
all_data8$prov_type <- ifelse(grepl(paste(c("California","wild","wld","collect","^w$"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"W",all_data8$prov_type)
  # cultivated (H)
all_data8$prov_type <- ifelse(grepl(paste(c("cultiva","garden","nursery","^c$","^g$"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"H",all_data8$prov_type)
  # ex wild (Z)
all_data8$prov_type <- ifelse(grepl(paste(c("indirect","ex wild","^z$"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"Z",all_data8$prov_type)
  # native to site (N)
all_data8$prov_type <- ifelse(grepl("native",
  all_data8$prov_type, ignore.case=T),"N",all_data8$prov_type)
  # unknown (U)
all_data8$prov_type <- ifelse(all_data8$prov_type!= "W" & all_data8$prov_type != "Z" &
  all_data8$prov_type != "H" & all_data8$prov_type != "N","U",all_data8$prov_type) # everything else is unknown ("U")
  all_data8$prov_type[which(is.na(all_data8$prov_type))] <- "U"

# check results
unique(all_data8$prov_type)

## B) NUMBER OF PLANTS

# look at column contents
sort(unique(all_data8$num_plants))
## IF NEEDED: replace unwanted characters
  #all_data8$num_plants <- mgsub(all_data8$num_plants, c(" at VC","\\+","ca "," S","1inG7"," in nur"," shoot","deck","mass",
  #  "(4 in nur)","3-","\\?","\\(4\\)"," in pot"," \\(1\\)"," in pots"," RJ"," \\*KH","-10"," \\*","\\(4 B&B\\)"), "")
  #sort(unique(all_data8$num_plants))

# change type to numeric and replace NA with 1
all_data8$num_plants <- as.numeric(all_data8$num_plants)
all_data8$num_plants[which(is.na(all_data8$num_plants))] <- 1

# check results
unique(all_data8$num_plants)

## C) LATITUDE AND LONGITUDE

# preserve original lat and long columns
all_data8$lat_dd <- all_data8$orig_lat
all_data8$long_dd <- all_data8$orig_long

# replace comma with decimal (european notation)
all_data8$lat_dd <- mgsub(all_data8$lat_dd, c(",",";"," ."), ".")
all_data8$long_dd <- mgsub(all_data8$long_dd, c(",",";"), ".")

# replace non-ascii characters
all_data8$lat_dd <- replace_non_ascii(all_data8$lat_dd)
all_data8$long_dd <- replace_non_ascii(all_data8$long_dd)

# replace remaining unwanted characters and format for conversion (e.g., ## ## ## (DMS) OR ## ##.### (DM));  (d, m, and s must be in the same cell, with 1 space between each value)
  ## latitude
  sort(unique(all_data8$lat_dd))
all_data8$lat_dd <- mgsub(all_data8$lat_dd, c("N","\\","\"","\'","/")," ")
all_data8$lat_dd <- str_squish(all_data8$lat_dd)
  sort(unique(all_data8$lat_dd))
    # the next two lines search for latitudes that start with four digits in a row and then adds a space after the first two
need_space <- grep("^[0-9][0-9][0-9][0-9]+",all_data8$lat_dd); unique(all_data8$lat_dd[need_space])
all_data8$lat_dd[need_space] <- gsub("^(.{2})(.*)$", "\\1 \\2",all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd))
  ## longitude
  sort(unique(all_data8$long_dd))
all_data8$long_dd <- mgsub(all_data8$long_dd, c("E","\\","\"","\'","/")," ")
all_data8$long_dd <- str_squish(all_data8$long_dd)
  sort(unique(all_data8$long_dd))
    # move "W" to end if at beginning
all_data8$long_dd[grep("^W",all_data8$long_dd)] <- paste(all_data8$long_dd[grep("^W",all_data8$long_dd)],"W")
all_data8$long_dd[grep("^W",all_data8$long_dd)] <- gsub("W","",all_data8$long_dd[grep("^W",all_data8$long_dd)],"W")
    # the next few lines search for longitudes that start with five digits in a row and then adds a space after the first three
need_space <- grep("^[0-9][0-9][0-9][0-9][0-9]+",all_data8$long_dd)
need_space <- c(need_space,grep("^1[0-9][0-9][0-9]",all_data8$long_dd))
all_data8$long_dd[need_space] <- gsub("^(.{3})(.*)$", "\\1 \\2",all_data8$long_dd[need_space])
  sort(unique(all_data8$long_dd))
    # the next few lines search for longitudes that start with four digits in a row and then adds a space after the first two
need_space <- grep("^[0-9][0-9][0-9][0-9]+",all_data8$long_dd)
#need_space <- c(need_space,grep("^[0-9][0-9][0-9][0-9] +",all_data8$long_dd))
all_data8$long_dd[need_space] <- gsub("^(.{2})(.*)$", "\\1 \\2",all_data8$long_dd[need_space])
  sort(unique(all_data8$long_dd))
    # if first three numbers are greater than 180, split string after second number
all_data8$long_dd <- ifelse(as.numeric(substr(all_data8$long_dd,start=1,stop=3))>180,gsub("^(.{2})(.*)$", "\\1 \\2", all_data8$long_dd),all_data8$long_dd)
    # add negative sign where needed and remove "W"
all_data8$long_dd[grep("W",all_data8$long_dd)] <- paste("-",all_data8$long_dd[grep("W",all_data8$long_dd)],sep="")
all_data8$long_dd <- gsub("W","",all_data8$long_dd)
all_data8$long_dd <- str_squish(all_data8$long_dd)
  sort(unique(all_data8$long_dd))

# convert decimal-minutes-seconds (dms) to decimal degrees (dd)
  # mark rows that need to be converted
convert <- all_data8[which(grepl(" ",all_data8$lat_dd) | grepl(" ",all_data8$long_dd)),]; nrow(convert) #125
good <- subset(all_data8, !(convert %in% all_data8))
  # separate by dec_min_sec and deg_dec_min then convert to decimal degrees
dms <- convert[which(count.spaces(convert$lat_dd) == 2),]; nrow(dms) #60
ddm <- convert[which(count.spaces(convert$lat_dd) == 1),]; nrow(ddm) #63
extra <- convert[which(count.spaces(convert$lat_dd) == 0),]; nrow(extra) #1
  dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #124
dms <- convert[which(count.spaces(convert$long_dd) == 2),]; nrow(dms) #60
ddm <- convert[which(count.spaces(convert$long_dd) == 1),]; nrow(ddm) #64
extra <- convert[which(count.spaces(convert$long_dd) != 1 & count.spaces(convert$long_dd) != 2),]; nrow(extra) #0
  dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #124
  # join everything back together
all_data9 <- rbind(good,convert); nrow(all_data9) #1498

# clean up lat and long coordinates
all_data9$lat_dd <- as.numeric(all_data9$lat_dd); sort(unique(all_data9$lat_dd))
all_data9$long_dd <- as.numeric(all_data9$long_dd); sort(unique(all_data9$long_dd))

# add gps_det column
all_data9$gps_det <- NA
all_data9$gps_det[which(all_data9$prov_type == "H")] <- "H"
all_data9$gps_det[which(!is.na(all_data9$lat_dd) & !is.na(all_data9$long_dd))] <- "G"
all_data9$prov_type[which(all_data9$gps_det == "G" & all_data9$prov_type == "H")] <- "H?"
  table(all_data9$prov_type)

## D) LOCALITY AND COLLECTOR

# create all_locality and collector columns
  # replace non-ascii characters
all_data9$locality <- replace_non_ascii(all_data9$locality)
all_data9$municipality <- replace_non_ascii(all_data9$municipality)
all_data9$county <- replace_non_ascii(all_data9$county)
all_data9$state <- replace_non_ascii(all_data9$state)
all_data9$orig_source <- replace_non_ascii(all_data9$orig_source)
all_data9$notes <- replace_non_ascii(all_data9$notes)
  # create all_locality column
all_data9 <- unite(all_data9, "all_locality",
                    c(locality,municipality,county,state,orig_source,notes),
                    sep = " | ", remove = F)
  # create collector column
all_data9 <- unite(all_data9, "collector",
                    c(coll_name,coll_num,coll_year),
                    sep = " | ", remove = F)

# write file
write.csv(all_data9, file = "exsitu_working/exsitu_compiled_standardized.csv")

##############################
# 5. Remove duplicate records
##############################

# make num_plants numeric
all_data9$num_plants <- as.numeric(all_data9$num_plants)
str(all_data9)

# remove duplicates
all_data10 <- ddply(all_data9,
                  .(sp_full_name,inst_short_added,prov_type,lat_dd,long_dd,all_locality,gps_det,
                    locality,municipality,county,state,orig_source,notes,country,collector,
                    sp_full_name_orig,sp_full_name_concat,
                    genus,species,infra_rank,infra_name,cultivar,
                    acc_num,lin_num,acq_year,germ_type,rec_as,garden_loc),
                    summarise, sum_num_plt = sum(num_plants)) #species_name_acc,hyrbid,
  str(all_data10); nrow(all_data10) #1472

# replace commas so no issues with semicolon, just to be sure CSV works properly
all_data10[] <- lapply(all_data10, function(x) gsub(",", ";", x))

# write file
write.csv(all_data10, file = "exsitu_working/exsitu_compiled_noDuplicates.csv")
