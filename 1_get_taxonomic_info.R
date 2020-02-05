### Author: Emily Beckman  ###  Date: 10/17/19

### DESCRIPTION:
  # This script takes a list of taxa and uses the taxize package to pull
  # taxonomic information from multiple databases; information pulled includes:
    # - Global Names Resolver (GNR) matches
    # - Synonyms from Tropicos, Integrated Taxonomic Information Service (ITIS),
    #   and CATALOGUE OF LIFE (COL)
    # - Children (var./subsp.) from ITIS and COL
  # The outputs can then be used to create a final "target_taxa_inclu_syn.csv"
  #   file by hand

### SCRIPT OUTLINE:
  # 1. Load taxa list
  # 2A. Create file of matching names from Global Names Resolver (GNR)
  # 2B. Download CSV of The Plant List (TPL) names
  # 3. Find synonyms for target taxa
  # 4. Create master synonym list
  # 5. Find children for target taxa
  # 6. Create master children list
  # 7. Create master target taxa list

### INPUTS:
  # target_taxa.csv (list of target taxa)
    # two columns:
      # 1. "taxon_full_name" (genus, species, infra rank, and infra name, all
      #    separated by one space each; hybrid symbol should be " x ", rather
      #    than "_" or "✕", and go between genus and species)
      # 2. "orig_list" (can say where name came from, if you are using more
      #    than one source list)

### OUTPUTS:
  ## gnr_output_unique.csv
    # taxize_tropicos_names.csv
    # taxize_itis_names.csv
    # taxize_itis_children.csv
    # taxize_col_names.csv
    # taxize_col_children.csv
    # taxize_all_names_raw.csv
  ## taxize_synonyms.csv
  ## taxize_children.csv

### NOTES:
  # TAXIZE DATABASE INPUTS:                           # ABBREVIATION:
    # Encyclopedia of Life (EOL)                        # eol
    # Taxonomic Name Resolution Service                 # tnrs
    # Integrated Taxonomic Information Service (ITIS)   # itis
    # Global Names Resolver (from EOL/GBIF)             # gnr
    # Global Names Index (from EOL/GBIF)                # gni
    # IUCN Red List                                     # iucn
    # Tropicos (from Missouri Botanical Garden)         # tp / tropicos
    # Theplantlist.org                                  # tpl
    # Catalogue of Life                                 # col
    # National Center for Biotechnology Information     # ncbi
    # CANADENSYS Vascan name search API                 # vascan
    # International Plant Names Index (IPNI)            # ipni
    # Barcode of Life Data Systems (BOLD)               # bold
    # National Biodiversity Network (UK)                # nbn
    # Index Fungorum                                    # fg
    # EU BON                                            # eubon
    # Index of Names (ION)                              # ion
    # Open Tree of Life (TOL)                           # tol
    # NatureServe                                       # natserv


#################
### LIBRARIES ###
#################

library(plyr)
library(tidyverse) #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
library(rgbif)
library(data.table)
library(taxize)
library(anchors)
library(batchtools)
library(textclean)


#################
### FUNCTIONS ###
#################

# remove speices/taxa that did not have any synonyms (they create errors
  # in next step), create data frame of synonyms, and add column stating
  # which database it came from
synonyms.compiled <- function(syn_output,db_name){
  found <- NA
    for(i in 1:length(syn_output)){
      if(length(syn_output[[i]])>1){
        if(syn_output[[i]][1,3]!="no syns found"){
          found <- c(found,i)
          syn_output[[i]]$taxon_name_acc <- rep(names(syn_output[i]),
            nrow(syn_output[[i]]))
        }
      }
    }
  found <- found[-1]
  syn_output_df <- Reduce(rbind.fill, syn_output[found])
  syn_output_df$database <- db_name
  return(syn_output_df)
}

# remove speices/taxa that did not have any children (they create errors
  # in next step), create data frame of children, and add column stating
  # which database it came from
children.compiled <- function(child_output,db_name,greater_than){
  found <- NA
    for(i in 1:length(child_output)){
      if(length(child_output[[i]])>greater_than){
        found <- c(found,i)
        child_output[[i]]$taxon_name_acc <- rep(names(child_output[i]),
          nrow(child_output[[i]]))
      }
    }
  found <- found[-1]
  child_output_df <- Reduce(rbind.fill, child_output[found])
  child_output_df$database <- db_name
  return(child_output_df)
}


##############
### SCRIPT ###
##############

setwd("./Desktop")

####################
# 1. Load taxa list
####################

# read in taxa list
  # download from Google Drive
  #drive_download("species_list",type="csv",overwrite=T)
taxa_list_acc <- read.csv("target_taxa.csv", header = T, na.strings=c("","NA"),
  colClasses="character")

# create list of target taxa names
taxa_names <- taxa_list_acc[,1]
unique(taxa_names)

# create list of target species names only
species_names <- taxa_names[
  !grepl(" var. ",taxa_names) &
  !grepl(" subsp.",taxa_names) &
  !grepl(" f. ",taxa_names)]
unique(species_names)

# create list of target species names only, with hybrids removed
species_only <- species_names[
  !grepl(" x ",species_names)]
unique(species_only)

####################################################################
# 2A. Create file of matching names from Global Names Resolver (GNR)
####################################################################

# check out the datasources included in the GNR and when they were last updated
#gnr_datasources()

# run all names through GNR; may take a few minutes if you have lots of names
taxa_names2 <- taxa_names
chunked <- split(taxa_names2,chunk(taxa_names2,chunk.size=100))
gnr_output <- data.frame()
for(i in 1:length(chunked)){
  gnr_output_new <- gnr_resolve(names=chunked[[i]],with_context=T)
  gnr_output <- rbind(gnr_output,gnr_output_new)
  print(chunked[[i]])
}
# IF A NAME THROWS AN ERROR, remove it and run code chunk above again
  #taxa_names2 <- taxa_names[-(109)]

# keep unique values and create "ref" column listing all source databases
unique_gnr <- gnr_output %>% group_by(user_supplied_name,matched_name) %>%
  summarize(ref = paste(data_source_title, collapse = ', ')) %>% ungroup()
nrow(gnr_output); nrow(unique_gnr)

# count the number of references for each record
num_ref <- unlist(lapply(unique_gnr$ref, function(x) str_count(x, ",")+1))
unique_gnr <- cbind(unique_gnr,num_ref)

# standardize hybrid character
unique_gnr$matched_name <- mgsub(unique_gnr$matched_name,
  c(" × "," X "," _ ")," x ")

write.csv(unique_gnr,"gnr_output_unique.csv")

#################################################
# 2B. Download CSV of The Plant List (TPL) names
#################################################

# see list of all TPL families
tpl_families()

# download all TPL data; slow!
#tpl_get("tpl_all_names_raw")

# download data for specific family or families
tpl_get("tpl_names_raw", family=c("Magnoliaceae","Sapindaceae"))

###################################
# 3. Find synonyms for target taxa
###################################

##
### A) Tropicos (from Missouri Botanical Garden)
##

# IF NEEDED: set API key and restart R
  #taxize::use_tropicos() # get API
  #usethis::edit_r_environ() # set API
    # TROPICOS_KEY='________' # paste this in

# replace hybrid character to match Tropicos system
species_names <- gsub(" x "," × ",species_names,fixed=T)

# get synonyms
# Tropicos does not search for infrataxa, so we will use species list
names_tp <- synonyms(species_names, db="tropicos")

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
names_tp_df <- synonyms.compiled(names_tp,"tropicos")
# standardize column names for joining later
colnames(names_tp_df)[colnames(names_tp_df)=="nameid"] <- "syn_id"
colnames(names_tp_df)[colnames(names_tp_df)=="scientificname"] <- "syn_name"
colnames(names_tp_df)[colnames(names_tp_df)=="scientificnamewithauthors"] <-
  "syn_name_with_authors"
# keep only necessary columns
names_tp_df <- names_tp_df[,c("taxon_name_acc","database","syn_name",
  "syn_name_with_authors","syn_id")]
  colnames(names_tp_df)

# write file
write.csv(names_tp_df,"taxize_tropicos_names.csv")

##
### B) Integrated Taxonomic Information Service (ITIS)
##

# replace characters to match ITIS system
taxa_names <- gsub(" x "," X ",taxa_names,fixed=T)
taxa_names <- gsub(" subsp. "," ssp. ",taxa_names)

# get synonyms
names_itis <- synonyms(taxa_names, db="itis")

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
names_itis_df <- synonyms.compiled(names_itis,"itis")
# standardize column names for joining later
colnames(names_itis_df)[colnames(names_itis_df)=="acc_tsn"] <- "acc_id"
colnames(names_itis_df)[colnames(names_itis_df)=="syn_tsn"] <- "syn_id"
# keep only necessary columns
names_itis_df <- names_itis_df[,c("taxon_name_acc","database","syn_name",
  "syn_author","syn_id","acc_name","acc_author","acc_id")]
  colnames(names_itis_df)

# write file
write.csv(names_itis_df,"taxize_itis_names.csv")

##
### C) Catalogue of Life
##

# COL isn't working right now; keeps throwing "Too Many Requests (HTTP 429)"
#   error, but even when you only do a few at a time it doesn't work

#chunked <- split(species_only,chunk(species_only,chunk.size=5))
#names_col <- data.frame()
#for(i in 1:length(chunked)){
#  names_col_new <- synonyms(chunked[[i]], db="col")
#  names_col <- rbind(names_col,names_col_new)
#  print(chunked[[i]])
#  Sys.sleep()
#}

# get synonyms
# COL does not search for infrataxa or hybrids, so we will use species-only list
#names_col <- synonyms(species_only, db="col")

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
#names_col_df <- synonyms.compiled(names_col,"col")
# standardize column names for joining later
#for(i in 1:nrow(names_col_df)){
#  if(names_col_df$rank[i]=="infraspecies"){
#    names_col_df$syn_name[i] <- paste(names_col_df$genus[i],
#                                      names_col_df$species[i],
#                                      names_col_df$infraspecies_marker[i],
#                                      names_col_df$infraspecies[i])
#  } else {
#    names_col_df$syn_name[i] <- names_col_df$name[i]
#  }
#}
#colnames(names_col_df)[colnames(names_col_df)=="id"] <- "syn_id"
#colnames(names_col_df)[colnames(names_col_df)=="author"] <- "syn_author"
# keep only necessary columns
#names_col_df <- names_col_df[,c("taxon_name_acc","database","syn_name",
#  "syn_author","syn_id")]; colnames(names_col_df)

# write file
#write.csv(names_col_df,"taxize_col_names.csv")

################################
# 4. Create master synonym list
################################

# create dataframe of all synonyms found
  # list of data frames
datasets <- list(names_itis_df,names_tp_df)#,names_col_df)
  # go through list of data frames and stack each
all_names <- Reduce(rbind.fill,datasets); colnames(all_names)
  # order rows by taxa name
all_names <- setorder(all_names,"taxon_name_acc")
# write CSV file of all names
write.csv(all_names,"taxize_all_names_raw.csv")

# create dataframe of unique syn names
  # standardize the infrarank abbreviations
all_names$syn_name <- gsub("ssp.","subsp.",all_names$syn_name,fixed=T)
all_names$syn_name <- gsub("fo.","f.",all_names$syn_name,fixed=T)
all_names$syn_name <- gsub(" X "," x ",all_names$syn_name,fixed=T)
all_names$syn_name <- gsub("_","x",all_names$syn_name,fixed=T)
all_names$taxon_name_acc <- gsub("ssp.","subsp.",all_names$taxon_name_acc,
                                fixed=T)
all_names$taxon_name_acc <- gsub("fo.","f.",all_names$taxon_name_acc,fixed=T)
all_names$taxon_name_acc <- gsub(" X "," x ",all_names$taxon_name_acc,fixed=T)
all_names$taxon_name_acc <- gsub("_","x",all_names$taxon_name_acc,fixed=T)
all_names$syn_name_with_authors <- gsub("ssp.","subsp.",
                                        all_names$syn_name_with_authors,fixed=T)
all_names$syn_name_with_authors <- gsub("fo.","f.",
                                        all_names$syn_name_with_authors,fixed=T)
all_names$syn_name_with_authors <- gsub(" X "," x ",
                                        all_names$syn_name_with_authors,fixed=T)
all_names$syn_name_with_authors <- gsub("_","x",
                                        all_names$syn_name_with_authors,fixed=T)
  # create standard syn_name_with_authors column
all_names$syn_name_with_authors[is.na(all_names$syn_name_with_authors)] <-
  paste(all_names[is.na(all_names$syn_name_with_authors),]$syn_name,
  all_names[is.na(all_names$syn_name_with_authors),]$syn_author)
  # keep unique values and create "ref" column of all databases with duplicates
unique_names <- all_names %>% group_by(taxon_name_acc,syn_name) %>%
  summarize(ref = paste(database, collapse = ',')) %>% ungroup()
nrow(all_names); nrow(unique_names)

# join syn list to main taxa list
taxa_list_acc$taxon_full_name <- gsub("_","x",taxa_list_acc$taxon_full_name)
colnames(unique_names)[colnames(unique_names)=="taxon_name_acc"] <-
  "taxon_full_name"
unique_names_join <- join(unique_names,taxa_list_acc,type="full")

# keep only necessary columns
unique_names_join2 <- unique_names_join[,c("taxon_full_name","syn_name","ref",
  "orig_list")]; colnames(unique_names_join2)

# reorder then write file
unique_names_join2 <- setorder(unique_names_join2,"taxon_full_name","syn_name")
write.csv(unique_names_join2,"taxize_synonyms.csv")

###################################
# 5. Find children for target taxa
###################################

##
### B) Integrated Taxonomic Information Service (ITIS)
##

# replace hybrid character to match ITIS system
species_names <- gsub(" × "," X ",species_names,fixed=T)

# get children names (var. and subsp.)
children_itis <- children(species_names, db="itis")

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any children,
#   create data frame of children,
#   and add column stating which database it came from
children_itis_df <- children.compiled(children_itis,"itis",4)
colnames(children_itis_df)

# write file
write.csv(children_itis_df,"taxize_itis_children.csv")

##
### C) Catalogue of Life
##

# COL isn't working right now; keeps throwing "Too Many Requests (HTTP 429)"
#   error, but even when you only do a few at a time it doesn't work

# get children names (var. and subsp.)
#children_col <- children(species_only, db="col")

### !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any children,
#   create data frame of children,
#   and add column stating which database it came from
#children_col_df <- children.compiled(children_col,"col",3)
#colnames(children_col_df)

# write file
#write.csv(children_col_df,"taxize_col_children.csv")

#################################
# 6. Create master children list
#################################

# create dataframe of all children found
 # list of data frames
datasets2 <- list(children_itis_df)#,children_col_df)
 # go through list of data frames and stack each
all_children <- Reduce(rbind.fill,datasets2); colnames(all_children)
  # order rows by taxa name
colnames(all_children)[colnames(all_children)=="taxon_name_acc"] <-
  "taxon_full_name"
all_children <- setorder(all_children,"taxon_full_name")
  # keep unique values and create "ref" column of all databases with duplicates
unique_children <- all_children %>% group_by(taxon_full_name,taxonname) %>%
  summarize(ref = paste(database, collapse = ',')) %>% ungroup()
nrow(all_children); nrow(unique_children)

# write CSV file of all names
write.csv(unique_children,"taxize_children.csv")

####################################
# 7. Create master target taxa list
####################################

# go through outputs of this script BY HAND and create a CSV file with three col:
#   1) "taxon_full_name_acc" (accepted name)
#   2) "taxon_full_name" (synonym name or accepted name,for accepted taxa)
#   3) "orig_list" (note what is part of accepted list, synonym, other category)
# save the file as "target_taxa_inclu_syn.csv"
