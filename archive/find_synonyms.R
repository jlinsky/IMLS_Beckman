### Author: Emily Beckman  ###  Date: 08/02/19


#################
### LIBRARIES ###
#################

library(taxize) # this is the central package utilized in this script
library(plyr)
library(dplyr)
library(data.table)


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


##############
### SCRIPT ###
##############

setwd("./Desktop")

#######################
# 1. Load species list
#######################

# read in species list
  #drive_download("species_list",type="csv",overwrite=T) # download from Google Drive
sp_list_acc <- read.csv("Acer_IUCN_accepted_names.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
sp_names <- sp_list_acc[,1] # create list of target species names

########################################
# 2. Find synonyms (taxize package)
########################################

# Integrated Taxonomic Information Service (ITIS)
  # get synonyms
  names_itis <- synonyms(sp_names, db="itis")
  ### !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  # add a species_name column based on each df name in list and create list (found)
    # of indices of species name matches which did not come back NA/NULL
  found <- NA
  for(i in 1:length(names_itis)){
    if(length(names_itis[[i]])>1){
      if(names_itis[[i]][1,3]!="no syns found"){
        found <- c(found,i)
        names_itis[[i]]$species_name <- rep(names(names_itis[i]), nrow(names_itis[[i]]))
      }
    }
  }; found <- found[-1]
  # create data frame of synonyms
  names_itis_df <- Reduce(rbind.all.columns, names_itis[found])
  # add dataset column to record these came from itis
  names_itis_df$database <- "itis"
  # concatenate some columns to standardize, rename other cols, then keep only necessary cols
  names_itis_df$acc_name_with_authors <- paste(names_itis_df$acc_name,names_itis_df$acc_author)
  names_itis_df$syn_name_with_authors <- paste(names_itis_df$syn_name,names_itis_df$syn_author)
  colnames(names_itis_df)[colnames(names_itis_df)=="acc_tsn"] <- "acc_id"
  colnames(names_itis_df)[colnames(names_itis_df)=="syn_tsn"] <- "syn_id"
  names_itis_df <- names_itis_df[,c(8,6,11,9,2,10,3,7)]

# Tropicos (from Missouri Botanical Garden)
  # get synonyms
  names_tp <- synonyms(sp_names, db="tropicos")
  ### !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  # add a species_name column based on each df name in list and create list (found)
    # of indices of species name matches which did not come back NA/NULL
  found <- NA
  for(i in 1:length(names_tp)){
    if(length(names_tp[[i]])>1){
      if(names_tp[[i]][1,2]!="no syns found"){
        found <- c(found,i)
        names_tp[[i]]$species_name <- rep(names(names_tp[i]), nrow(names_tp[[i]]))
      }
    }
  }; found <- found[-1]
  # create data frame of synonyms
  names_tp_df <- Reduce(rbind.all.columns, names_tp[found])
  # add dataset column to record these came from tropicos (tp)
  names_tp_df$database <- "tropicos"
  # change column names for joining later
  colnames(names_tp_df)[colnames(names_tp_df)=="nameid"] <- "syn_id"
  colnames(names_tp_df)[colnames(names_tp_df)=="scientificname"] <- "syn_name"
  colnames(names_tp_df)[colnames(names_tp_df)=="scientificnamewithauthors"] <- "syn_name_with_authors"
  # keep only necessary columns
  names_tp_df <- names_tp_df[,c(5,2,3,6,1)]

# Catalogue of Life
  # get synonyms
  names_col <- synonyms(sp_names, db="col")
  ### !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  # add a species_name column based on each df name in list and create list (found)
    # of indices of species name matches which did not come back NA/NULL
  found <- NA
  for(i in 1:length(names_col)){
    if(length(names_col[[i]])>1){
      found <- c(found,i)
      names_col[[i]]$species_name <- rep(names(names_col[i]), nrow(names_col[[i]]))
    }
  }; found <- found[-1]
  # create data frame of synonyms
  names_col_df <- Reduce(rbind.all.columns, names_col[found])
  # add dataset column to record these came from col
  names_col_df$database <- "col"
  # change column names for joining later
  for(i in 1:nrow(names_col_df)){
    if(names_col_df$rank[i]=="infraspecies"){
      names_col_df$syn_name[i] <- paste(names_col_df$genus[i],names_col_df$species[i],
                                        names_col_df$infraspecies_marker[i],names_col_df$infraspecies[i])
    } else {
      names_col_df$syn_name[i] <- names_col_df$name[i]
    }
  }
  names_col_df$syn_name_with_authors <- paste(names_col_df$syn_name,names_col_df$author)
  colnames(names_col_df)[colnames(names_col_df)=="id"] <- "syn_id"
  # keep only necessary columns
  names_col_df <- names_col_df[,c(16,18,19,17,1,15,4)]


# Create dataframe of all synonyms found
datasets <- list(names_itis_df,names_tp_df,names_col_df) # list of data frames
all_names <- Reduce(rbind.all.columns,datasets); colnames(all_names) # go through list of data frames and stack each
all_names <- setorder(all_names,"species_name") # order rows by species name

# write CSV file
write.csv(all_names,"taxize_all_names.csv")
