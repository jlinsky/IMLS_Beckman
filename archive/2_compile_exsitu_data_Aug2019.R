### Author: Emily Beckman  ###  Date: 07/19/19

### DESCRIPTION:
  # This script    .

### INPUTS:
    # 1.     (sp_list_joined.csv)

### OUTPUT:
    # 1.      (sernec_raw.csv)


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

# trim white space at beginning and end of string; takes a vector (e.g., column of a data frame)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# count the number of spaces in a string; takes a vector (e.g., column of a data frame)
  # and returns all rows in dataframe with spaces in strings in target column
count.spaces <- function(x) { sapply(gregexpr(" ", x), function(y) { sum(y>=0) } ) }

######## !!! I want to fix this to make it more efficient !!! ############

# Marks rows by placing "mark" in the "marked" column
# search "df.column" based on each element in list "search.list"
# "fixed.tf" takes either "T" or "F" and alters the grepl command
mark.rows <- function(search.list, df, df.column, fixed.tf) {
  # cycle through search.list and mark matching rows
  for(char in 1:length(search.list)){
    for(i in 1:nrow(df)){
      match <- grepl(pattern = search.list[char], x = df.column[i], fixed = fixed.tf)
      if(match == TRUE){
        df$marked[i] <- "mark"
        print("TRUE")
      }
    }
    print(search.list[char]) # print search.list element so you know how long its taking
  }
  print(paste("number of rows marked:", nrow(df[which(df$marked == "mark"),]))) # number of rows marked
  return(df)
}
#t <- sapply(all_data7[,4], function(x) {
#      sapply(infra, function(y) {
#        ifelse(grepl(y,x),print(all_data7[,4]),print("no")) #all_data7[,53] <- all_data7[,4]
#      })
#    })

#t <- function(df,check.col) {
#  sapply(df[,check.col], function(x) {
#    sapply(infra, function(y) {
#      grep(y,x)
#      })
#    })
#  }


##############
### SCRIPT ###
##############

setwd("./Desktop/GA2")

##################
# 1. Compile files
##################

# read in ex situ accessions CSV files from folder and create data frame for each
file_list <- list.files(path="./GA_2_exsitu",pattern=".csv",full.names=TRUE); str(file_list)
file_dfs <- sapply(file_list,read.csv,header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character")
  length(file_dfs) #135
  #sapply(file_dfs, nrow)

# inst_short_added [nickname I've created for each institution] column added, based on file name
  ### way to do this more cleanly with apply family?
for(file in seq_along(file_dfs)){
  file_dfs[[file]]$inst_short_added <- rep(file_list[file], nrow(file_dfs[[file]]))
}

# call merge/stack function; 'Reduce' iterates through list and merges with previous dataframe in the list
all_data_raw <- Reduce(rbind.all.columns, file_dfs)
  nrow(all_data_raw) #1148790
  ncol(all_data_raw) #240

##############################
# 2. Standardize column schema
##############################

# remove extra columns (created through Excel to CSV issues)
all_data <- all_data_raw[, -grep("^X", names(all_data_raw))]
  str(all_data); ncol(all_data) # check schema to see if problems still exist; 96

# merge similar columns (you may not need to do this if no schema mistakes were made)
all_data2 <- tidyr::unite(all_data,"inst_short", c("inst_name","inst_short","isnt_name"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"acc_num", c("acc_num","acc_no"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"acq_year", c("acq_year","aqu_year","aqu_yr","aqu_year.1","year"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"coll_num", c("coll_num","coll_no"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"coll_year", c("coll_year","coll_yr"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"orig_lat", c("orig_lat","CoordLatDD","lat"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"orig_long", c("orig_long","CoordLongDD","long"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"country", c("country","CountryName","country_code"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"cultivar", c("cultivar","Cultivar"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"sp_full_name", c("sp_full_name","full_sp_name","TaxonNameFull"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"germ_type", c("germ_type","garden_loc","loc"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"genus", c("genus","Genus"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"hybrid", c("hybrid","hybrid.1"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"infra_name", c("infra_name","intra_name","InfraName1","specific_name"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"infra_rank", c("infra_rank","intra_rank","InfraType1","specific_rank"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"lin_num", c("lin_num","lin_no","pedigree"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"locality", c("locality","Locality","locality.1","locallity","loaclity","locality_notes"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"state", c("state","maj_region"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"notes", c("notes","note"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"num_plants", c("num_plants","num_alive"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"prov_type", c("prov_type","ProvenanceCode"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"rec_as", c("rec_as","rec_material"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"orig_source", c("orig_source","source"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"species", c("species","Species"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"condition", c("condition","status"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"name_determ", c("name_determ","taxa.ID"),sep="",remove=T)
all_data2 <- tidyr::unite(all_data2,"municipality", c("municipality","city"),sep="",remove=T)
  str(all_data2); ncol(all_data2) # check schema to see if problems still exist : 51

# remove unused columns & rename others
all_data2 <- all_data2[ , -which(names(all_data2) %in% c("habit","order","seed_zone","Diameter","InfraGroup","specific2","specific","family"))]
colnames(all_data2)[colnames(all_data2)=="elevation"] <- "altitude"
colnames(all_data2)[colnames(all_data2)=="elevation_ft"] <- "altitude_ft"
  str(all_data2); ncol(all_data2) # check schema to see if problems still exist : 43

# update inst_short_added column to contain only garden name, not filepath
all_data2$inst_short_added <- sub("./GA_2_exsitu/","",all_data2$inst_short_added)
all_data2$inst_short_added <- sub(".csv","",all_data2$inst_short_added)
# update inst_short_added for gardens in the file of short csv combined
all_data2$inst_short_added <- ifelse(all_data2$inst_short_added == "ShortCombined",all_data2$inst_short,all_data2$inst_short_added)
  sort(unique(all_data2$inst_short_added))

# write raw file
write.csv(all_data2, "exsitu_working/GA2_exsitu_compiled_raw.csv")

##############################################
# 3. Standardize genus and species name fields
##############################################

# read in compiled raw data
all_data2 <- read.csv("exsitu_working/GA2_exsitu_compiled_raw.csv",header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character",na.strings=c("NA",""))
all_data2 <- all_data2[,2:ncol(all_data2)]

# OPTIONAL: remove known hybrids
# check hybrid column for anything you want to keep and replace with ""
sort(unique(all_data2$hybrid))
all_data3 <- replace.value(all_data2,"hybrid",c("species","Standardized text: ((X)) or ((H)) if hybrid",""),NA)
# remove rows which are not NA in hybrid column
all_data3 <- all_data3[which(is.na(all_data3$hybrid)),]; nrow(all_data3) #1145301

# preserve original species name
all_data3$sp_full_name_orig <- all_data3$sp_full_name
  sort(unique(all_data3$sp_full_name_orig))
  nrow(is.na(all_data3$sp_full_name_orig))

# create original sp_full_name col if does not already have one, for record keeping
test <- all_data3[which(grepl("\\.",all_data3$species)==TRUE),]; cbind(test$species,test$inst_short_added) # check for rows that may need to be altered
all_data3 <- unite(all_data3, "sp_full_name_concat",
                      c(genus,species,infra_rank,infra_name,cultivar),
                      sep = " ", remove = F)
  all_data3$sp_full_name_concat <- mgsub(all_data3$sp_full_name_concat, c("NA "," NA"," NA"," NA"," NA"), "")
# separate out species full name and trim white space
all_data3$sp_full_name <- trim(all_data3$sp_full_name)
  all_data3$sp_full_name <- gsub("  ", " ",all_data3$sp_full_name)
all_data4 <- all_data3 %>% separate("sp_full_name", c("likely_genus","likely_species","extra1",
                                    "extra2","extra3","extra4","extra5","extra6"),sep=" ", fill="right", extra="warn")
### would like to do this using the apply family..
  all_data4$likely_genus <- trim(all_data4$likely_genus)
  all_data4$likely_species <- trim(all_data4$likely_species)
  all_data4$extra1 <- trim(all_data4$extra1)
  all_data4$extra2 <- trim(all_data4$extra2)
  all_data4$extra3 <- trim(all_data4$extra3)
  all_data4$extra4 <- trim(all_data4$extra4)
  all_data4$extra5 <- trim(all_data4$extra5)
  all_data4$extra6 <- trim(all_data4$extra6)
    nrow(all_data4) #1145281

# check if genus column is blank, and fill if necessary; then remove rows without genus
all_data4$likely_genus[is.na(all_data4$likely_genus) & !is.na(all_data4$genus)] <- all_data4$genus[is.na(all_data4$likely_genus) & !is.na(all_data4$genus)]
all_data4 <- all_data4[which(!is.na(all_data4$likely_genus)),]; nrow(all_data4) #101178

# check if species column is blank, and fill if necessary; then remove rows without species
all_data4$likely_species[is.na(all_data4$likely_species) & !is.na(all_data4$species)] <- all_data4$species[is.na(all_data4$likely_species) & !is.na(all_data4$species)]
all_data4 <- all_data4[which(!is.na(all_data4$likely_species)),]; nrow(all_data4) #99314

# fill extra columns with infra_rank and infra_name when available, because these are more accurate?
all_data4$extra1[!is.na(all_data4$infra_rank) & !is.na(all_data4$infra_name)] <- all_data4$infra_rank[!is.na(all_data4$infra_rank) & !is.na(all_data4$infra_name)]
all_data4$extra2[!is.na(all_data4$infra_rank) & !is.na(all_data4$infra_name)] <- all_data4$infra_name[!is.na(all_data4$infra_rank) & !is.na(all_data4$infra_name)]

# correct genus and species capitalization issues
all_data4$likely_genus <- str_to_title(tolower(all_data4$likely_genus))
all_data4$likely_species <- tolower(all_data4$likely_species)

# see if any genera are misspelled, and correct these
sort(unique(all_data4$likely_genus))
### do this all in one step instead???
  all_data5 <- replace.value(all_data4, "likely_genus", "Carrya", "Carya")
  all_data5 <- replace.value(all_data5, "likely_genus", "Fagusfagus", "Fagus")
  all_data5 <- replace.value(all_data5, "likely_genus", "Linder", "Lindera")
  all_data5 <- replace.value(all_data5, "likely_genus", "Sassafrass", "Sassafras")
  all_data5 <- replace.value(all_data5, "likely_genus", "Juglans Cinerea", "Juglans")
  all_data5 <- replace.value(all_data5, "likely_genus", "Sasafras", "Sassafras")
  all_data5 <- replace.value(all_data5, "likely_genus", "(=Pinus", "Pinus")
  all_data5 <- replace.value(all_data5, "likely_genus", "Taxis", "Taxus")

# replace any strange characters in species column
sort(unique(all_data5$likely_species))
  current <- c("_","«","\\(","\\'","\\[","\\]","Â","Ã","ÌÑ","\\(","\\)","\"","\\["," ","\\.","ã","â",
               "[\\\u0091]","[\\\u0092]","[\\\u0094]","[\\\u0095]","[\\\u0097]","[\\\u008a]","[\\\u008c]")
### MAKE THIS AN APPLY STATEMENT INSTEAD OF FOR LOOP ??
for (i in 1:length(current)){
  all_data5$likely_species <- gsub(pattern = current[i], x = all_data5$likely_species, replacement = "")
}; sort(unique(all_data5$likely_species))

# write file
write.csv(all_data5, "exsitu_working/GA2_exsitu_compiled_namesAdded.csv")

#######################################
# 4. Filter by target genus and species
#######################################

# read in target species list and ex situ data
all_data5 <- read.csv("exsitu_working/GA2_exsitu_compiled_namesAdded.csv",header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character",na.strings=c("NA",""))
all_data5 <- all_data5[,2:ncol(all_data5)]
species_list <- read.csv("species_list_joined_8_22.csv",fileEncoding="latin1",strip.white=T,colClasses="character",as.is=T,na.strings=c("","NA"))
species_list <- species_list[,2:length(species_list)]

# keep only rows from target genera
all_data6 <- all_data5 %>% filter(all_data5$likely_genus %in% unique(species_list$genus))
  nrow(all_data6) #72667

# keep only rows from target genera and species
all_data6$genus_species <- paste(all_data6$likely_genus,all_data6$likely_species); unique(all_data6$genus_species)
all_data6 <- all_data6 %>% filter(all_data6$genus_species %in% unique(species_list$genus_species))
  nrow(all_data6) #35335

## check to see which institutions get excluded, and manually check those files to see if issues
setdiff(unique(all_data2$inst_short_added),unique(all_data6$inst_short_added))

#######################################
# 5. Create infra specific name columns
#######################################

all_data7 <- all_data6
# search each "extra" column for infra indicators then add the text in the next "extra" column to the likley_infraname col
  infra <- c("^variety$","^var$","^var.$","^v$","^v.$",
             "^ssp.$","^subspecies$","^ssp$","^subsp.$",
             "^fo.$","^forma$","^fma.$","^f.$","^form$",
             "^x$","hybrid","^_$",
             "^infra.$")
             ## not using these to look for cultivars right now: "^cultivar$", "^cv.$", "^\'", "\'$", "\"", "\\[", "Â«"
all_data7$marked <- NA
all_data7$likely_infrarank <- NA
all_data7$likely_infraname <- NA
### I WANT TO GET RID OF THE FOR LOOPS IN THIS FUNCTION !! (see top of script)
all_data7 <- mark.rows(infra,all_data7,all_data7$extra5,F)
### THIS LOOP SHOULD BE SOMETHING FROM THE APPLY FAMILY OR TIDYVERSE !!?!!!
    for (i in 1:nrow(all_data7)){
      if(!is.na(all_data7$marked[i])){
        all_data7$likely_infrarank[i] <- all_data7$extra5[i]
        all_data7$likely_infraname[i] <- all_data7$extra6[i]
      }
    }; sort(unique(all_data7$likely_infrarank)); sort(unique(all_data7$likely_infraname))
all_data7$marked <- NA
all_data7 <- mark.rows(infra,all_data7,all_data7$extra4,F)
    for (i in 1:nrow(all_data7)){
      if(!is.na(all_data7$marked[i])){
        all_data7$likely_infrarank[i] <- all_data7$extra4[i]
        all_data7$likely_infraname[i] <- all_data7$extra5[i]
      }
    }; sort(unique(all_data7$likely_infrarank)); sort(unique(all_data7$likely_infraname))
all_data7$marked <- NA
all_data7 <- mark.rows(infra,all_data7,all_data7$extra3,F)
    for (i in 1:nrow(all_data7)){
      if(!is.na(all_data7$marked[i])){
        all_data7$likely_infrarank[i] <- all_data7$extra3[i]
        all_data7$likely_infraname[i] <- all_data7$extra4[i]
      }
    }; sort(unique(all_data7$likely_infrarank)); sort(unique(all_data7$likely_infraname))
all_data7$marked <- NA
all_data7 <- mark.rows(infra,all_data7,all_data7$extra2,F)
    for (i in 1:nrow(all_data7)){
      if(!is.na(all_data7$marked[i])){
        all_data7$likely_infrarank[i] <- all_data7$extra2[i]
        all_data7$likely_infraname[i] <- all_data7$extra3[i]
      }
    }; sort(unique(all_data7$likely_infrarank)); sort(unique(all_data7$likely_infraname))
all_data7$marked <- NA
all_data7 <- mark.rows(infra,all_data7,all_data7$extra1,F)
    for (i in 1:nrow(all_data7)){
      if(!is.na(all_data7$marked[i])){
        all_data7$likely_infrarank[i] <- all_data7$extra1[i]
        all_data7$likely_infraname[i] <- all_data7$extra2[i]
      }
    }; sort(unique(all_data7$likely_infrarank)); sort(unique(all_data7$likely_infraname))

# make sure empty cells are NA
all_data7[] <- lapply(all_data7, function(x) gsub("^$", NA, x))
all_data7[] <- lapply(all_data7, function(x) gsub("^NA$", NA, x))

# add infra name to rows where no infra rank is present
all_data7$likely_infrarank[!is.na(all_data7$infra_rank) & is.na(all_data7$likely_infrarank)] <- "infra."
all_data7$likely_infraname[!is.na(all_data7$infra_rank) & is.na(all_data7$likely_infrarank)] <- all_data7$infra_name[!is.na(all_data7$infra_rank) & is.na(all_data7$likely_infrarank)]

# standardize the likely_infrarank column
all_data8 <- replace.value(all_data7,"likely_infrarank",c("ssp.","subspecies","ssp"),"subsp.")
all_data8 <- replace.value(all_data8,"likely_infrarank",c("variety","var","v","v."),"var.")
all_data8 <- replace.value(all_data8,"likely_infrarank",c("fo.","forma","fma.","form"),"f.")
all_data8 <- replace.value(all_data8,"likely_infrarank",c("(hybrid)","hybrid)","hybrid","_"),"x")
    sort(unique(all_data8$likely_infrarank))

# replace any strange characters in infraname column
sort(unique(all_data8$likely_infraname))
  current <- c("\\(","\\'","\\)","\"","\\]","\\[")
### MAKE THIS AN APPLY STATEMENT INSTEAD OF FOR LOOP ??
for (i in 1:length(current)){
  all_data8$likely_infraname <- gsub(pattern = current[i], x = all_data8$likely_infraname, replacement = "")
}; sort(unique(all_data8$likely_infraname))

######################################
# 6. Final filter by species full name
######################################

# create new sp_full_name column
all_data9 <- all_data8
all_data9$sp_full_name <- ""
### GET RID OF FOR LOOP ??
  for (i in 1:nrow(all_data9)){
    if(is.na(all_data9$likely_infraname[i])){
      all_data9$sp_full_name[i] <- paste(all_data9$likely_genus[i],all_data9$likely_species[i],sep=" ")
    } else {
      all_data9$sp_full_name[i] <- paste(all_data9$likely_genus[i],all_data9$likely_species[i],all_data9$likely_infrarank[i],all_data9$likely_infraname[i],sep=" ")
    }
  }; sort(unique(all_data9$sp_full_name)); nrow(all_data9) #35335

# join to species list
  all_data9 <- subset(all_data9, select = -c(extra1,extra2,extra3,extra4,extra5,extra6,genus,species,infra_name,infra_rank,marked))
  all_data9 <- plyr::join(all_data9, species_list, type = "left", match = "first")
  # only keep matches
  all_data9 <- all_data9[which(!is.na(all_data9$species_name_acc)),]
  nrow(all_data9) #35335

# write file
write.csv(all_data9, "exsitu_working/GA2_exsitu_compiled_targetSpecies.csv")

##################################
# 7. Standardize important columns
#################################

# read in compiled filtered data
all_data9 <- read.csv("exsitu_working/GA2_exsitu_compiled_targetSpecies.csv",strip.white=T,colClasses="character",as.is=T,fileEncoding="latin1")
all_data9 <- all_data9[,2:ncol(all_data9)]

# standardize provenence type column
sort(unique(all_data9$prov_type))
    # transfer contents of one column to another column, if data needs to be preserved but is in wrong place
all_data9$notes[grep("Of known, direct wild origin - Florence County, SC.", all_data9$prov_type)] <- "Florence County, SC"
all_data9$notes[grep("ClearLake,California(38'N)", all_data9$prov_type)] <- "ClearLake,California(38'N)"
    # search for part of string and replace whole row contents
wild <- c("California","wild","wld","collect","^w$")
all_data9$prov_type <- ifelse(grepl(paste(wild, collapse = "|"), all_data9$prov_type, ignore.case=T),"W",all_data9$prov_type)
hort <- c("cultiva","garden","nursery","^c$","^g$")
all_data9$prov_type <- ifelse(grepl(paste(hort, collapse = "|"), all_data9$prov_type, ignore.case=T),"H",all_data9$prov_type)
exwild <- c("indirect","ex wild","^z$")
all_data9$prov_type <- ifelse(grepl(paste(exwild, collapse = "|"), all_data9$prov_type, ignore.case=T),"Z",all_data9$prov_type)
all_data9$prov_type <- ifelse(grepl("native", all_data9$prov_type, ignore.case=T),"N",all_data9$prov_type)
    # everything else is unknown ("U")
all_data9$prov_type <- ifelse(all_data9$prov_type!= "W" & all_data9$prov_type != "Z" &
     all_data9$prov_type != "H" & all_data9$prov_type != "N","U",all_data9$prov_type)
all_data9$prov_type[which(is.na(all_data9$prov_type))] <- "U"

# standardize number of plants column
all_data10 <- all_data9
sort(unique(all_data10$num_plants))
    # search for part of string and replace that part
search_list <- c(" at VC","\\+","ca "," S","1inG7"," in nur"," shoot","deck","mass","(4 in nur)","3-","\\?",
                 "\\(4\\)"," in pot"," \\(1\\)"," in pots"," RJ"," \\*KH","-10"," \\*","\\(4 B&B\\)")
for (i in 1:length(search_list)){
  all_data10$num_plants <- gsub(pattern = search_list[i], x = all_data10$num_plants, replacement = "")
}; sort(unique(all_data10$num_plants))
    # change to numeric and replace NAs with 1
all_data10$num_plants <- as.numeric(all_data10$num_plants)
all_data10$num_plants[which(is.na(all_data10$num_plants))] <- 1

# standardize latitude and longitude columns
all_data11 <- all_data10
all_data11$lat_dd <- all_data11$orig_lat
all_data11$long_dd <- all_data11$orig_long
      # replace comma with decimal (european notation)
all_data11$lat_dd <- mgsub(all_data11$lat_dd, c(",",";"," ."), ".")
all_data11$long_dd <- mgsub(all_data11$long_dd, c(",",";"), ".")
      # replace non-ascii characters
all_data11$lat_dd <- replace_non_ascii(all_data11$lat_dd)
all_data11$long_dd <- replace_non_ascii(all_data11$long_dd)
      # replace remaining unwanted characters
all_data11$lat_dd <- gsub("AA"," ",all_data11$lat_dd)
all_data11$lat_dd <- mgsub(all_data11$lat_dd, c("\'","\"","\\","W","E","N","S","_"),"")
all_data11$lat_dd <- gsub("  "," ",all_data11$lat_dd)
  sort(unique(all_data11$lat_dd))
all_data11$long_dd <- gsub("AA"," ",all_data11$long_dd)
all_data11$long_dd <- mgsub(all_data11$long_dd, c("\'","\"","\\","W","E","N","S","_"),"")
all_data11$long_dd <- gsub("  "," ",all_data11$long_dd)
  sort(unique(all_data11$long_dd))
      # trim whitespace on ends
all_data11$lat_dd <- trim(all_data11$lat_dd)
all_data11$long_dd <- trim(all_data11$long_dd)
      # mark rows that need to be converted to decimal degrees
convert <- all_data11[which(grepl(" ",all_data11$lat_dd) | grepl(" ",all_data11$long_dd)),]; nrow(convert) #498
good <- subset(all_data11, !(convert %in% all_data11))
      # separate by dec_min_sec and deg_dec_min then convert to decimal degrees (d, m, and s must be in the same cell, with 1 space between each value)
dms <- convert[which(count.spaces(convert$lat_dd) == 2),]; nrow(dms) #277
ddm <- convert[which(count.spaces(convert$lat_dd) == 1),]; nrow(ddm) #220
extra <- convert[which(count.spaces(convert$lat_dd) == 0),]; nrow(extra) #1
  dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #498
dms <- convert[which(count.spaces(convert$long_dd) == 2),]; nrow(dms) #280
ddm <- convert[which(count.spaces(convert$long_dd) == 1),]; nrow(ddm) #217
extra <- convert[which(count.spaces(convert$long_dd) != 1 & count.spaces(convert$long_dd) != 2),]; nrow(extra) #1
  dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #498
      # join everything back together
all_data12 <- rbind(good,convert); nrow(all_data12) #34323
    # clean up lat and long coordinates
all_data12$lat_dd <- as.numeric(all_data12$lat_dd); unique(all_data12$lat_dd)
all_data12$long_dd <- as.numeric(all_data12$long_dd); unique(all_data12$long_dd)
all_data12$lat_dd[is.na(all_data12$lat_dd)] <- 0
all_data12$long_dd[is.na(all_data12$long_dd)] <- 0
  unique(all_data12$lat_dd)
  unique(all_data12$long_dd)
  # switch lat and long values if lat is greater than 50 or less than 0
for (r in seq_along(all_data12$lat_dd)){
  temp <- all_data12$lat_dd[r]
  if (temp > 50 || temp < -50){
    all_data12$lat_dd[r] <- all_data12$long_dd[r]
    all_data12$long_dd[r] <- temp
  }
}
  # add a minus sign if the longitude is positive
for (r in 1:nrow(all_data12)){
  if (all_data12$long_dd[[r]] > 0){
      all_data12$long_dd[[r]] <- -1 * all_data12$long_dd[[r]]
  }
}; unique(all_data12$long_dd)
  # add gps_det column
all_data12$gps_det <- NA
all_data12$gps_det[which(all_data12$prov_type == "H")] <- "H"
all_data12$gps_det[which(all_data12$lat_dd != 0 & all_data12$long_dd != 0)] <- "G"
all_data12$prov_type[which(all_data12$gps_det == "G" & all_data12$prov_type == "H")] <- "H?"

# create all_locality and collector columns
    # replace non-ascii characters
  all_data12$locality <- replace_non_ascii(all_data12$locality)
  all_data12$municipality <- replace_non_ascii(all_data12$municipality)
  all_data12$county <- replace_non_ascii(all_data12$county)
  all_data12$state <- replace_non_ascii(all_data12$state)
  all_data12$orig_source <- replace_non_ascii(all_data12$orig_source)
  all_data12$notes <- replace_non_ascii(all_data12$notes)
    # create all_locality column
  all_data13 <- unite(all_data12, "all_locality",
                      c(locality,municipality,county,state,orig_source,notes),
                      sep = " | ", remove = F)
    # create collector column
  all_data13 <- unite(all_data13, "collector",
                      c(coll_name,coll_num,coll_year),
                      sep = " | ", remove = F)
    # replace commas so no issues with semicolon just to be sure CSV works properly
  all_data13[] <- lapply(all_data13, function(x) gsub(",", ";", x))

# write file
write.csv(all_data13, file = "exsitu_working/GA2_exsitu_compiled_targetSpecies_standardized.csv")

######################
# 7. Remove duplicates
######################

# read in dataset
all_data13 <- read.csv("exsitu_working/GA2_exsitu_compiled_targetSpecies_standardized.csv",strip.white=T,colClasses="character",as.is=T,fileEncoding="latin1")
all_data13 <- all_data13[,2:ncol(all_data13)]
# make num_plants field numeric so it can be summed
all_data13$num_plants <- as.numeric(all_data13$num_plants)
# remove duplicates
all_data14 <- ddply(all_data13, .(sp_full_name,inst_short_added,prov_type,lat_dd,long_dd,all_locality,gps_det,
                                  locality,municipality,county,state,orig_source,notes,country,collector,
                                  sp_full_name_orig,sp_full_name_concat,species_name_acc,genus,species,infra_rank,infra_name,
                                  cultivar,inst_short,acq_year,germ_type), summarise, sum_num_plt = sum(num_plants))
  str(all_data14); nrow(all_data14) #18727

### extra join step NOT NEEDED NORMALLY
to_join <- read.csv("exsitu_working/GA2_exsitu_compiled_targetSpecies_standardized_nodup_8_22_working_TOJOIN.csv",strip.white=T,colClasses="character",as.is=T,fileEncoding="latin1")
all_data15 <- join(all_data14,to_join,type="left",match="first"); nrow(all_data15)

can_match <- all_data15[which(all_data15$lat_dd == 0 & all_data15$long_dd == 0 & is.na(all_data15$gps_det)),]; nrow(can_match)
cant_match <- anti_join(all_data15,can_match); nrow(cant_match)
can_match[,c(6:8)] <- can_match[,28:30]
all_data16 <- rbind(can_match,cant_match); nrow(all_data16)


# write file
write.csv(all_data16, file = "exsitu_working/GA2_exsitu_compiled_targetSpecies_standardized_nodup_8_22_3.csv")












###!!!### Now pull into Excel and geolocate records for species of concern which are not of "H" prov_type
        # Name the new document "GA2_exsitu_compiled_targetSpecies_standardized_nodup_GEOLOCATING.csv"


old <- read.csv("exsitu_working/GA2_exsitu_compiled_TargetSpecies_standardized_nodup_GEOLOCATING_need.csv",strip.white=T,colClasses="character",as.is=T,fileEncoding="latin1")
  nrow(old) #1541
  #old <- ddply(old, .(sp_full_name,inst_short_added,all_locality))
new <- read.csv("exsitu_working/GA2_exsitu_compiled_TargetSpecies_standardized_nodup_8_6_new.csv",strip.white=T,colClasses="character",as.is=T,fileEncoding="latin1")
  nrow(new) #17558 ; 17556

old$all_locality2 <- str_replace_all(old$all_locality, "[^[:alnum:]]", "")
new$all_locality2 <- str_replace_all(new$all_locality, "[^[:alnum:]]", "")
#old$sp_full_name_orig <- str_replace_all(old$sp_full_name_orig, "[^[:alnum:]]", "")
#new$sp_full_name_orig <- str_replace_all(new$sp_full_name_orig, "[^[:alnum:]]", "")

search_list <- c(" ","Î","ç","Ã","Â","º","ê","ƒ","â","N","W","å","ö","Ç","Õ","ë")
for(i in 1:length(search_list)){
  old$all_locality2 <- gsub(pattern = search_list[i], x = old$all_locality2, replacement = "")
  new$all_locality2 <- gsub(pattern = search_list[i], x = new$all_locality2, replacement = "")
}
  new$all_locality2 <- gsub("Californie", "California",new$all_locality2)
  old$all_locality2 <- gsub("IthacaSofLakeCayug4243474076504753aewYorkArboretumStaffAcquisition","IthacaSofLakeCayugaewYorkArboretumStaffAcquisition", old$all_locality2)
  old$all_locality2 <- gsub("LibertyCountyFloridaTallTimbersRSAlongashadyriverbank","TorreyaStateParkLibertyCountyFloridaTallTimbersRSAlongashadyriverbank", old$all_locality2)

search_list <- c("Ã","Â","¨","ª","«","_","ë")
for(i in 1:length(search_list)){
  old$sp_full_name_orig <- gsub(pattern = search_list[i], x = old$sp_full_name_orig, replacement = "")
  new$sp_full_name_orig <- gsub(pattern = search_list[i], x = new$sp_full_name_orig, replacement = "")
}

old[] <- lapply(old, function(x) gsub("  "," ", x))
new[] <- lapply(new, function(x) gsub("  "," ", x))
old[] <- lapply(old, function(x) gsub("  "," ", x))
new[] <- lapply(new, function(x) gsub("  "," ", x))

no_latlong <- new[which(new$lat_dd=="0" | new$long_dd=="0"),]; nrow(no_latlong) #12005 ; 12003
latlong <- new[which(new$lat_dd!="0" & new$long_dd!="0"),]; nrow(latlong) #5553
  latlong$gps_det <- NA
  no_latlong <- no_latlong[,c(1:4,7:39)]
  old_pts <- old[,c(20,4,6:8,26)]
  filled <- plyr::join(no_latlong,old_pts,match="first"); nrow(filled) #12005
  new2 <- rbind(latlong,filled); nrow(new2) #17556
    #new2$lat_dd[which(is.na(new2$lat_dd))] <- "0"
    #new2$long_dd[which(is.na(new2$long_dd))] <- "0"
    t <- anti_join(old_pts,new2); nrow(t)

#no_latlong2 <- new2[which(is.na(new2$gps_det) & (new2$lat_dd=="0" | new2$long_dd=="0")),]; nrow(no_latlong2) #10407
#latlong2 <- anti_join(new2,no_latlong2); nrow(latlong2) #7149
#  no_latlong2 <- no_latlong2[,c(1:4,7:45)]
#  old_pts2 <- old[,c(3:4,6:8,10)]
#  y_latlong2 <- plyr::join(no_latlong2,old_pts2,match="first"); nrow(y_latlong2) #
#  joined2 <- rbind(latlong2,y_latlong2); nrow(joined2) #17556
#  t <- anti_join(old_pts,joined2); nrow(t)

# clean up lat and long
new2$lat_dd <- trim(new2$lat_dd)
new2$long_dd <- trim(new2$long_dd); unique(new2$long_dd)
      # mark rows that need to be converted to decimal degrees
new2$marked <- ""
convert_list <- c(" ")
new2 <- mark.rows(convert_list,new2,new2$lat_dd)
new2 <- mark.rows(convert_list,new2,new2$long_dd)
convert <- new2[which(new2$marked == "mark"),]; nrow(convert) #549
good <- new2[which(new2$marked != "mark"),]; nrow(good) #38856
      # separate by dec_min_sec and deg_dec_min then convert to decimal degrees (d, m, and s must be in the same cell, with 1 space between each value)
dms <- convert[which(count.spaces(convert$lat_dd) == 2),]; nrow(dms) #309
ddm <- convert[which(count.spaces(convert$lat_dd) == 1),]; nrow(ddm) #239
extra <- convert[which(count.spaces(convert$lat_dd) == 0),]; nrow(extra) #1
  dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #549
dms <- convert[which(count.spaces(convert$long_dd) == 2),]; nrow(dms) #312
ddm <- convert[which(count.spaces(convert$long_dd) == 1),]; nrow(ddm) #236
extra <- convert[which(count.spaces(convert$long_dd) != 1 & count.spaces(convert$long_dd) != 2),]; nrow(extra) #1
  dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #549
      # join everything back together
joined2 <- rbind(good,convert); nrow(joined2) #39405
    # clean up lat and long coordinates
joined2$lat_dd <- as.numeric(joined2$lat_dd); unique(joined2$lat_dd)
joined2$long_dd <- as.numeric(joined2$long_dd); unique(joined2$long_dd)
joined2$lat_dd[is.na(joined2$lat_dd)] <- 0
joined2$long_dd[is.na(joined2$long_dd)] <- 0
  unique(joined2$lat_dd)
  unique(joined2$long_dd)
  # switch lat and long values if lat is greater than 50 or less than 0
for (r in seq_along(joined2$lat_dd)){
  temp <- joined2$lat_dd[r]
  if (temp > 50 || temp < -50){
    joined2$lat_dd[r] <- joined2$long_dd[r]
    joined2$long_dd[r] <- temp
  }
}; unique(joined2$long_dd)
  # add a minus sign if the longitude is positive
for (r in 1:nrow(joined2)){
  if (joined2$long_dd[r] > 0){
      print(joined2$long_dd[r])
      joined2$long_dd[r] <- joined2$long_dd[r] * -1
      print(joined2$long_dd[r])
  }
}; sort(unique(joined2$long_dd))
  # remove minus sign if the latitude is negative
for (r in 1:nrow(joined2)){
  if (joined2$lat_dd[r] < 0){
      joined2$lat_dd[r] <- joined2$lat_dd[r] * -1
  }
}; sort(unique(joined2$lat_dd))


write.csv(joined2, file = "exsitu_working/GA2_exsitu_compiled_targetSpecies_standardized_nodup_6_20.csv")








## create subsets based on locality data
  # records with valid lat/long values
conc_has_latlong <- all_data13[which(all_data13$long_dd != 0),]
#conc_has_latlong <- conc_has_latlong %>% group_by(all_locality,inst_short_added,sp_full_name,acq_year,prov_type,lat_dd,long_dd,inst_name2,speciesKey,keep.all=T) %>% summarise(sum_num_plants = sum(sum_num_plants))
  nrow(conc_has_latlong) #5552
write.csv(conc_has_latlong, file = "exsitu_working/GA2_exsitu_compiled_haslatlong.csv")
    # remove duplicate rows and sum no_alive [number of individuals alive] for these deleted rows
#conc_has_latlong_unq <- as.tbl(conc_has_latlong) %>% group_by(inst_short,inst_short_added,sp_full_name,prov_type,all_locality) %>% summarise(sum_num_plants = sum(num_plants))
#head(conc_has_latlong_unq); nrow(conc_has_latlong_unq) #828
  # records without lat/long
conc_no_latlong <- all_data13[which(all_data13$long_dd == 0),]; nrow(conc_no_latlong) #12146
conc_locality <- conc_no_latlong[which(conc_no_latlong$all_locality_concat != "NA | NA | NA | NA | NA | NA | NA | NA | NA | 0 | 0"),]; nrow(conc_locality) #10918
conc_locality2 <- conc_no_latlong[which(conc_locality$prov_type != "H"),]; nrow(conc_locality2) #7754
conc_locality3 <- conc_no_latlong[which(!is.na(conc_locality2$all_locality)),]; nrow(conc_locality3) #7026
conc_locality4 <- conc_no_latlong[which(conc_locality3$all_locality != "NA"),]; nrow(conc_locality4) #6572

write.csv(conc_locality4, file = "exsitu_working/GA2_exsitu_compiled_nolatlong_yeslocality.csv")





















# just an interesting analysis:
  # replace "" with NA
all_data9 <- all_data8
all_data9[] <- lapply(all_data9, function(x) gsub("^$",NA, x))
  # calculate the % of cells which are empty in each column
percent_empty <- vector()
for(c in 1:ncol(all_data9)){
  count <- 0
  for(r in 1:nrow(all_data9)){
    if(is.na(all_data9[r,c])){
      count <- count+1
    }
  }
  percent_empty[c] <- count/nrow(all_data9)*100
}
per_empty <- as.data.frame(cbind(colnames(all_data9),percent_empty))
per_empty <- per_empty[order(per_empty$percent_empty),]; per_empty










# records without name in 'species' column
no_sp_name <- working[which(is.na(working$species_standard)),]
  nrow(no_sp_name) #18741

  # round 1
sort(unique(no_sp_name$species1)) # look at unique species names and fill 'transfer' vector with them
transfer <- c("coccinea","dentata","hartwissiana","imbricaria","macranthera","mongolica","petraea","pubescens")
x <- filter(no_sp_name, species1 %in% transfer) %>% mutate(species_standard = species1); nrow(x) #16
no_sp_name2 <- setdiff(no_sp_name, x); nrow(no_sp_name2) #18357
  # round 2
sort(unique(no_sp_name2$species2)) # look at unique species names and fill 'transfer' vector with them
no_transfer <- c("",NA,"Quercus")
x <- filter(no_sp_name2, !species2 %in% no_transfer) %>% mutate(species_standard = species2); nrow(x) #18032
no_sp_name3 <- setdiff(no_sp_name2, x); nrow(no_sp_name3) #18357
  # round 3
sort(unique(no_sp_name2$species3)) # look at unique species names and fill 'transfer' vector with them


# lower case for sp column


# remove unwanted "delete" column
idigbio <- subset(idigbio, select = -(delete))


##### DIDNT DO THIS RIGHT NOW; NEEDS TO BE STREAMLINED #######
  # location in collections
#sort(unique(all_data9$germ_type))
    # search for part of string and replace whole row contents
#search_list <- c("conservatory","nursery","outdoor","collection","along","garden","bank",
#                 "arboretum","bed","between","lawn","glasshouse","side","section",
#                 "cutting","gdn","park","plant","road","upper","existing",
#                 "graft","greenhouse","grounds","landscap","woods","natural","area",
#                 "outside","path","rooting","rock","seed","seedling","unknown",
#                 "nursey","walk","lookout","trail","about","off","hill",
#                 "north","south","east","west","border","grove","meadow","field","drive","site","forest","gate","homesite","pond","slope","berm","stream")
#replace_list <- c("GH","NR","CO","CO","CO","CO","SB",
#                  "CO","CO","CO","CO","GH","CO","CO",
#                  "cutting","CO","CO","CO","CO","CO","CO",
#                  "graft","GH","CO","CO","CO","CO","CO",
#                  "CO","CO","rooting","CO","SB","seedling","UN",
#                  "NR","CO","CO","CO","CO","CO","CO",
#                  "CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO")
#all_data9 <- replace.row.content(search_list, replace_list, all_data9, 19, T); sort(unique(all_data9$germ_type))
#search_list <- c("^B$","^C$","NUR","SD","^S$","^g$","^G$","^N$","PT")
#replace_list <- c("SB","CO","NR","SB","SB","CO","CO","NR","CO")
#all_data9 <- replace.row.content(search_list, replace_list, all_data9, 19, F); unique(all_data9$germ_type)
    # everything else is unknown ("UN")
#for (i in 1:nrow(all_data9)){
#  if(all_data9$germ_type[i] != "CO" & all_data9$germ_type[i] != "SB" &
#     all_data9$germ_type[i] != "GH" & all_data9$germ_type[i] != "NR"){
#       all_data9$germ_type[i] <- "UN"
#  }
#}; sort(unique(all_data9$germ_type))
