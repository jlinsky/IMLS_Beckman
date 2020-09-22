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

setwd("./Desktop/exsitu")

##################
# 1. Compile files
##################

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

##############################
# 2. Standardize column schema
##############################

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

# write raw file
write.csv(all_data, "exsitu_working/exsitu_compiled_raw.csv")

##############################################
# 3. Standardize genus and species name fields
##############################################

# read in compiled raw data
#all_data <- read.csv("exsitu_working/exsitu_compiled_raw.csv",header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character",na.strings=c("NA",""))
#  all_data2 <- all_data[,2:ncol(all_data)]
all_data2 <- all_data

# standardize hybrid column
sort(unique(all_data2$hybrid))
all_data3 <- replace.value(all_data2,"hybrid",c("H","X"),"x") # make the current values all the same
  nrow(all_data3[which(all_data3$hybrid=="x"),]) #162
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
all_data3$sp_full_name_concat <- trim(all_data3$sp_full_name_concat) # trim white space
  unique(all_data3$sp_full_name_concat)

# when blank, fill sp_full_name column with concatenated species full name
all_data3$sp_full_name[all_data3$sp_full_name==""] <- all_data3$sp_full_name_concat[all_data3$sp_full_name==""]

# NOT USING RIGHT NOW
# remove hybrid symbol in sp_full_name column
#all_data3$sp_full_name <- mgsub(all_data3$sp_full_name, c("_"," X ","hybrid","×"," x "), " ")
#all_data3$sp_full_name <- gsub("  ", " ",all_data3$sp_full_name) # replace double space with single space
#  all_data3[which(all_data3$hybrid=="x"),]$sp_full_name

# separate out species full name and trim white space
all_data3$sp_full_name <- trim(all_data3$sp_full_name)
all_data3$sp_full_name  <- mgsub(all_data3$sp_full_name, c("\'","(","\\","\"",".",")"), "") # replace unwanted characters
all_data3$sp_full_name <- gsub("  ", " ",all_data3$sp_full_name) # replace double space with single space
all_data4 <- all_data3 %>% separate("sp_full_name", c("genus_new","species_new","extra1","extra2",
                                    "extra3","extra4","extra5","extra6"),sep=" ", fill="right", extra="warn")
  ### would like to do this using the apply family..?
all_data4$genus_new <- trim(all_data4$genus_new)
all_data4$species_new <- trim(all_data4$species_new)
all_data4$extra1 <- trim(all_data4$extra1)
all_data4$extra2 <- trim(all_data4$extra2)
all_data4$extra3 <- trim(all_data4$extra3)
all_data4$extra4 <- trim(all_data4$extra4)
all_data4$extra5 <- trim(all_data4$extra5)
all_data4$extra6 <- trim(all_data4$extra6)

# remove rows without genus and species
  nrow(all_data4) #3563
all_data4 <- all_data4[which(!is.na(all_data4$genus_new) & !is.na(all_data4$species_new)),]; nrow(all_data4) #3563

# check genus names
sort(unique(all_data4$genus_new))
# IF NEEDED: fix misspellings
  #all_data4 <- replace.value(all_data4, "genus_new", "Carrya", "Carya")

# check species names
sort(unique(all_data4$species_new))
# replace unwanted characters in species column
all_data4$species_new <- mgsub(all_data4$species_new, c("\'","(","\\","\"","."), "")

# write file
write.csv(all_data4, "exsitu_working/exsitu_compiled_namesAdded.csv")

#######################################
# 4. Filter by target genus and species
#######################################

# read in target species list and ex situ data
all_data5 <- read.csv("exsitu_working/exsitu_compiled_namesAdded.csv",header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character",na.strings=c("NA",""))
all_data5 <- all_data5[,2:ncol(all_data5)]
species_list <- read.csv("species_list_joined_IMLS.csv",fileEncoding="latin1",strip.white=T,colClasses="character",as.is=T,na.strings=c("","NA"))
species_list <- species_list[,2:length(species_list)]

# keep only rows from target genera
all_data6 <- all_data5 %>% filter(all_data5$genus_new %in% unique(species_list$genus))
  nrow(all_data6) #72667

# keep only rows from target genus and species
all_data6$genus_species <- paste(all_data6$genus_new,all_data6$species_new); unique(all_data6$genus_species)
all_data6 <- all_data6 %>% filter(all_data6$genus_species %in% unique(species_list$genus_species))
  nrow(all_data6) #35335

## check to see if institutions get excluded, and manually check those files to see if issues
setdiff(unique(all_data2$inst_short_added),unique(all_data6$inst_short_added))

# write file
write.csv(all_data6, "exsitu_working/exsitu_compiled_speciesFiltered.csv")

#######################################
# 5. Create infra specific name columns
#######################################

# read in data
all_data7 <- read.csv("exsitu_working/exsitu_compiled_namesAdded.csv",header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character",na.strings=c("NA",""))
all_data7 <- all_data7[,2:ncol(all_data7)]
#all_data7 <- all_data4

# create matrix of all "extra" species name columns, to search for infraspecific key words
search.col <- matrix(cbind(all_data7$extra1,all_data7$extra2,all_data7$extra3,
                           all_data7$extra4,all_data7$extra5all_data7$extra6),nrow=3563)
# search the "extra" column matrix for matches to infraspecific key words
matches <- which(search.col=="variety"|search.col=="var"|search.col=="v"|
                 search.col=="subspecies"|search.col=="ssp"|search.col=="subsp"|
                 search.col=="fo"|search.col=="forma"|search.col=="fma"|search.col=="f"|search.col=="form"|
                 search.col=="infra",arr.ind=T)
colnames(all_data7) # look at columns to check where the "extra" ones are
# change the number added based on where the extra1 column is in your dataset
  # e.g., add 19 if extra1 is in the 20th column
matches[,2] <- matches[,2]+19
# create new infra_rank column and fill with "extra" contents that matched infraspecific key word
all_data7$infra_rank_new <- NA
all_data7$infra_rank_new[matches] <- all_data7[matches]
  unique(all_data7$infra_rank_new) # check results
# create new infra_name column and fill with next column over from "extra" contents that matched infraspecific key word
all_data7$infra_name_new <- NA
matches[,2] <- matches[,2]+1
all_data7$infra_name_new[matches] <- all_data7[matches]
unique(all_data7$infra_name_new)




# search each "extra" column for infra indicators then add the text in the next "extra" column to the likley_infraname col
  infra <- c("^variety$","^var$","^var.$","^v$","^v.$",
             "^ssp.$","^subspecies$","^ssp$","^subsp.$",
             "^fo.$","^forma$","^fma.$","^f.$","^form$",
             "^infra.$")
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
#??? all_data7$likely_infrarank[!is.na(all_data7$infra_rank) & is.na(all_data7$likely_infrarank)] <- "infra."
#??? all_data7$likely_infraname[!is.na(all_data7$infra_rank) & is.na(all_data7$likely_infrarank)] <- all_data7$infra_name[!is.na(all_data7$infra_rank) & is.na(all_data7$likely_infrarank)]

# standardize the likely_infrarank column
all_data8 <- replace.value(all_data7,"likely_infrarank",c("ssp.","subspecies","ssp"),"subsp.")
all_data8 <- replace.value(all_data8,"likely_infrarank",c("variety","var","v","v."),"var.")
all_data8 <- replace.value(all_data8,"likely_infrarank",c("fo.","forma","fma.","form"),"f.")
all_data8 <- replace.value(all_data8,"likely_infrarank",c("(hybrid)","hybrid)","hybrid","_"),"x")
    sort(unique(all_data8$likely_infrarank))
    sort(unique(all_data8$likely_infraname))

# write file
write.csv(all_data8, "exsitu_working/exsitu_compiled_infraStandardized.csv")

######################################
# 6. Final filter by species full name
######################################

# read in data
all_data9 <- read.csv("exsitu_working/exsitu_compiled_infraStandardized.csv",header=TRUE,fileEncoding="latin1",strip.white=TRUE,colClasses="character",na.strings=c("NA",""))
all_data9 <- all_data9[,2:ncol(all_data9)]

# create new sp_full_name column
all_data9$sp_full_name <- ""
### GET RID OF FOR LOOP ??
  for (i in 1:nrow(all_data9)){
    if(is.na(all_data9$likely_infraname[i])){
      all_data9$sp_full_name[i] <- paste(all_data9$genus_new[i],all_data9$species_new[i],sep=" ")
    } else {
      all_data9$sp_full_name[i] <- paste(all_data9$genus_new[i],all_data9$species_new[i],all_data9$likely_infrarank[i],all_data9$likely_infraname[i],sep=" ")
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
