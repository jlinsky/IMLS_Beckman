### Author: Emily Beckman  ###  Date: 10/17/19
                                                                               |
### DESCRIPTION:
  # This script takes a list of taxa and uses the taxize package to pull
  # taxonomic information from multiple databases.
  # Main information pulled includes:
    # - Synonyms from Tropicos and Integrated Taxonomic Information
    #   Service (ITIS), and The Plant List (TPL)
  # The outputs can then be used to create a final "target_taxa_inclu_syn.csv"
  #   file by hand

### INPUTS:
  # target_taxa.csv (list of target taxa)
    # columns:
      # 1. "taxon_name" (genus, species, infra rank, and infra name, all
      #    separated by one space each; hybrid symbol should be " x ", rather
      #    than "_" or "✕", and go between genus and species)
      # 2+ other (can say where name came from, if you are using more
      #    than one source list, etc.)

### OUTPUTS:

    # taxize_tropicos_names.csv
    # taxize_tropicos_names_noDup.csv
    # taxize_tropicos_syn.csv
    # taxize_tropicos.csv

    # taxize_itis_names.csv
    # taxize_itis_names_noDup.csv
    # taxize_itis_syn.csv
    # taxize_itis.csv

    # taxize_tpl_names.csv
    # taxize_tpl_names_noDup.csv
    # taxize_tpl.csv

    # taxize_all_names_raw.csv
  ### taxize_all_names.csv

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
          syn_output[[i]]$taxon_name <- rep(names(syn_output[i]),
            nrow(syn_output[[i]]))
        }
      }
    }
  found <- found[-1]
  syn_output_df <- Reduce(rbind.fill, syn_output[found])
  syn_output_df$database <- db_name
  return(syn_output_df)
}


##############
### SCRIPT ###
##############

setwd("./Desktop")

####################
# 1. Load taxa list
####################

# read in taxa list
taxa_list_acc <- read.csv("target_taxa.csv", header = T, na.strings=c("","NA"),
  colClasses="character"); nrow(taxa_list_acc)

# create list of target taxa names
taxa_names <- taxa_list_acc[,1]
unique(taxa_names)

# create list of target species names, with infraspecific taxa removed
species_names <- taxa_names[
  !grepl(" var. ",taxa_names) &
  !grepl(" subsp.",taxa_names) &
  !grepl(" f. ",taxa_names)]
unique(species_names)

# create list of target species names only, with hybrids removed
species_only <- species_names[
  !grepl(" x ",species_names)]
unique(species_only)

###################################################
# 2. Check names and find synonyms for target taxa
###################################################

##
### A) Tropicos (from Missouri Botanical Garden)
##

# IF NEEDED: set API key and restart R
  #taxize::use_tropicos() # get API
  #usethis::edit_r_environ() # set API
    # TROPICOS_KEY='________' # paste this in

# Tropicos does not search for infrataxa, so we will use species list

# replace characters to match Tropicos system
species_names <- gsub(" x "," × ",species_names,fixed=T)

## MATCH NAMES

  # takes a while if lots of names
tp_names <- data.frame()
for(i in 1:length(species_names)){
  output_new <- tp_search(species_names[[i]])
  output_new$taxon_name <- species_names[[i]]
  tp_names <- rbind.fill(tp_names,output_new)
}
  #head(tp_names); class(tp_names); names(tp_names)
  # COLNAMES: error|nameid|scientificname|scientificnamewithauthors|family|
  #           rankabbreviation|nomenclaturestatusname|author|displayreference|
  #           displaydate|totalrows|nomenclaturestatusid|symbol|
# standardize column names for joining later
setnames(tp_names,
  old = c("scientificname","displayreference","nameid",
          "nomenclaturestatusname","scientificnamewithauthors"),
  new = c("taxon_name_match","source","match_id",
          "acceptance","match_name_with_authors"),
  skip_absent=T)
# keep only necessary columns
tp_names <- tp_names[,c("taxon_name","taxon_name_match","family",
  "source","match_id","acceptance","author","match_name_with_authors")]
  colnames(tp_names)
tp_names$database <- "tropicos"
# replace characters in taxa names
tp_names[] <- lapply(tp_names, function(x) gsub(" × "," x ", x))
tp_names[] <- lapply(tp_names, function(x) gsub(" fo. "," f. ", x))
# write file
write.csv(tp_names,"taxize_tropicos_names.csv")

# remove duplicates except those matching legitimate names
tp_names_noDup <- tp_names
  # remove rows with no match
tp_names_noDup <- tp_names_noDup[which(
  !is.na(tp_names_noDup$taxon_name_match)),]
  # remove taxon_name duplicates
tp_names_noDup$dup <- c(duplicated(tp_names_noDup$taxon_name,fromLast=T)
  | duplicated(tp_names_noDup$taxon_name))
tp_names_noDup <- setdiff(tp_names_noDup,tp_names_noDup[which(
  tp_names_noDup$acceptance != "Legitimate" & tp_names_noDup$dup == T),])
  # remove taxon_name_match duplicates
tp_names_noDup$dup <- c(duplicated(tp_names_noDup$taxon_name_match,fromLast=T)
  | duplicated(tp_names_noDup$taxon_name_match))
tp_names_noDup <- setdiff(tp_names_noDup,tp_names_noDup[which(
  tp_names_noDup$taxon_name != tp_names_noDup$taxon_name_match &
  tp_names_noDup$dup == T),])
  # remove dup column
tp_names_noDup <- tp_names_noDup[,(-10)]
# add column with authors
tp_names_noDup$match_name_with_authors <- paste(
  tp_names_noDup$taxon_name_match,tp_names_noDup$author)
# write file
write.csv(tp_names_noDup,"taxize_tropicos_names_noDup.csv")

## GET SYNONYMS

tp_syn <- synonyms(species_names, db="tropicos")

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
tp_syn_df <- synonyms.compiled(tp_syn,"tropicos")
# standardize column names for joining later
setnames(tp_syn_df,
  old = c("nameid","scientificname","scientificnamewithauthors"),
  new = c("match_id","taxon_name_match","match_name_with_authors"),
  skip_absent=T)
  colnames(tp_syn_df)
tp_syn_df$acceptance <- "synonym"
# replace characters in taxa names
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" × "," x ", x))
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" fo. "," f. ", x))
# write file
write.csv(tp_syn_df,"taxize_tropicos_syn.csv")

## STACK ALL DATA

# bind together
tp_all <- rbind.fill(tp_names_noDup,tp_syn_df)
# look at duplicates
#tp_all[which(duplicated(tp_all$taxon_name) & tp_all$acceptance != "synonym"),]
#tp_all[which(duplicated(tp_all$taxon_name_match)),]
# write file
write.csv(tp_all,"taxize_tropicos.csv")

##
### B) Integrated Taxonomic Information Service (ITIS)
##

# replace characters to match ITIS system
taxa_names <- gsub(" x "," X ",taxa_names,fixed=T)
taxa_names <- gsub(" subsp. "," ssp. ",taxa_names)

## MATCH NAMES

  # takes a while if lots of names
itis_names <- itis_terms(taxa_names,what="scientific")
  itis_names <- ldply(itis_names, data.frame) # list to data frame
  itis_names <- itis_names[,c(1:2,4:6)]
  #head(itis_output); class(itis_output); names(itis_output)
  # COLUMNS: .id|author|nameUsage|scientificName|tsn
# standardize column names for joining later
setnames(itis_names,
  old = c(".id","scientificName","nameUsage","tsn"),
  new = c("taxon_name","taxon_name_match","acceptance","match_id"),
  skip_absent=T)
  itis_names$database <- "itis"
# replace characters in taxa names
itis_names[] <- lapply(itis_names, function(x) gsub(" X "," x ", x))
itis_names[] <- lapply(itis_names, function(x) gsub(" ssp. "," subsp. ", x))
# write file
write.csv(itis_names,"itis_names.csv")

# remove duplicates except those matching legitimate names
itis_names_noDup <- itis_names
  # remove rows with no match
itis_names_noDup <- itis_names_noDup[which(
  !is.na(itis_names_noDup$taxon_name_match)),]
  # remove taxon_name duplicates
itis_names_noDup$dup <- c(duplicated(itis_names_noDup$taxon_name,fromLast=T)
  | duplicated(itis_names_noDup$taxon_name))
itis_names_noDup <- setdiff(itis_names_noDup,itis_names_noDup[which(
  itis_names_noDup$acceptance != "accepted" & itis_names_noDup$dup == T),])
  # remove taxon_name_match duplicates
itis_names_noDup$dup <- c(duplicated(itis_names_noDup$taxon_name_match,
  fromLast=T) | duplicated(itis_names_noDup$taxon_name_match))
itis_names_noDup <- setdiff(itis_names_noDup,itis_names_noDup[which(
  itis_names_noDup$taxon_name != itis_names_noDup$taxon_name_match &
  itis_names_noDup$dup == T),])
  # remove dup column
itis_names_noDup <- itis_names_noDup[,(-7)]
# add column with authors
itis_names_noDup$match_name_with_authors <- paste(
  itis_names_noDup$taxon_name_match,itis_names_noDup$author)
# write file
write.csv(itis_names_noDup,"taxize_itis_names_noDup.csv")

## GET SYNONYMS

itis_syn <- synonyms(taxa_names, db="itis")

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove speices/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
itis_syn_df <- synonyms.compiled(itis_syn,"itis")
# standardize column names for joining later
setnames(itis_syn_df,
  old = c("syn_name","syn_tsn","syn_author"),
  new = c("taxon_name_match","match_id","author"),
  skip_absent=T)
  colnames(itis_syn_df)
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name","taxon_name_match","author",
  "match_id","database")]
  colnames(itis_syn_df)
itis_syn_df$acceptance <- "synonym"
# add column with authors
itis_syn_df$match_name_with_authors <- paste(
  itis_syn_df$taxon_name_match,itis_syn_df$author)
# replace characters in taxa names
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" X "," x ", x))
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" ssp. "," subsp. ", x))
# remove records where taxa name and syn name are the same
itis_syn_df <- itis_syn_df[which(itis_syn_df$taxon_name !=
  itis_syn_df$taxon_name_match),]
# write file
write.csv(itis_syn_df,"taxize_itis_syn.csv")

## STACK ALL DATA

# bind together
itis_all <- rbind.fill(itis_names_noDup,itis_syn_df)
# look at duplicates
#itis_all[which(duplicated(itis_all$taxon_name) &
#  itis_all$acceptance != "synonym"),]
#itis_all[which(duplicated(itis_all$taxon_name_match)),]
# write file
write.csv(itis_all,"taxize_itis.csv")

##
### C) The Plant List (TPL)
##

# GET ALL DATA FOR TARGET FAMILIES

#tpl_families() # list of families in database
families <- c("Fagaceae","Rosaceae","Ulmaceae","Malvaceae")
tpl_names <- data.frame()
for(i in 1:length(families)){
  output_new <- tpl_get("files",family=families[i])
  output_new <- read.csv(paste("files/",families[i],".csv",sep=""), header = T,
    colClasses="character")
  tpl_names <- rbind.fill(tpl_names,output_new)
}
  #head(tpl_output); class(tpl_output); names(tpl_output)
  # COLUMNS: ID|Major group|Family|Genushybrid marker|Genus|
  #          Species hybrid marker|Species|Infraspecific rank|
  #          Infraspecific epithet|Authorship|Taxonomic status in TPL|
  #          Nomenclatural status from original data source|
  #          Confidence level|Source|Source id|IPNI id|Publication|Collation|
  #          Page|Date
# standardize column names for joining later
setnames(tpl_names,
  old = c("ID","Taxonomic.status.in.TPL","Confidence.level","Source",
          "Authorship","Family"),
  new = c("match_id","acceptance","score","source","author","family"),
  skip_absent=T)
  tpl_names$database <- "tpl"
  # create concatenated taxon_name col
  tpl_names <- unite(tpl_names, "taxon_name",
    c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
      Infraspecific.epithet), sep = " ", remove = F)
  # get rid of NAs in concatenated taxon name
  tpl_names$taxon_name <- mgsub(tpl_names$taxon_name,
    c("NA "," NA"," NA"," NA"," NA"), "")
  # replace hybrid character
  tpl_names$taxon_name <- gsub(" × "," x ",
    tpl_names$taxon_name,fixed=T)
  # trim whitespace
  tpl_names$taxon_name <- str_squish(tpl_names$taxon_name)
  # fill other columns
  tpl_names$taxon_name_match <- tpl_names$taxon_name
  tpl_names$match_name_with_authors <- paste(tpl_names$taxon_name,
    tpl_names$author)
  # keep only necessary columns
  tpl_names <- tpl_names[,c("taxon_name","taxon_name_match","author","match_id",
    "database","acceptance","match_name_with_authors","family","source")]
    colnames(tpl_names)
# write file
write.csv(tpl_names,"taxize_tpl_names.csv")

# remove duplicates
tpl_names_noDup <- tpl_names
tpl_names_noDup$dup <- c(duplicated(tpl_names_noDup$taxon_name,fromLast = TRUE)
  | duplicated(tpl_names_noDup$taxon_name))
tpl_names_noDup <- setdiff(tpl_names_noDup,tpl_names_noDup[
  which(tpl_names_noDup$acceptance != "Accepted" & tpl_names_noDup$dup == T),])
# remove dup column
tpl_names_noDup <- tpl_names_noDup[,(-10)]
# write file
write.csv(tpl_names_noDup,"taxize_tpl_names_noDup.csv")

# join with taxa list and remove non-matches
tpl_all <- tpl_names_noDup %>% filter(tpl_names_noDup$taxon_name %in%
  taxa_list_acc$taxon_name)
# write file
write.csv(tpl_all,"taxize_tpl.csv")

##
### D) International Plant Names Index (IPNI)
##

# GET ALL DATA FOR TARGET GENERA

genera <- c("Quercus","Malus","Ulmus","Tilia")
ipni_names <- data.frame()
for(i in 1:length(genera)){
  output_new <- ipni_search(genus=genera[i],output="extended") #family=,species=,infraspecies=
  ipni_names <- rbind.fill(ipni_names,output_new)
}
  head(ipni_names); class(ipni_names); names(ipni_names)
  # COLUMNS: id|version|family|full_name_without_family_and_authors|authors
# standardize column names for joining later
setnames(ipni_names,
  old = c("id","full_name_without_family_and_authors","authors"),
  new = c("match_id","taxon_name","author"),
  skip_absent=T)
  # replace hybrid character to match IPNI system
  ipni_names$taxon_name <- gsub(" × "," x ",
    ipni_names$taxon_name,fixed=T)
  # fill other columns
  ipni_names$taxon_name_match <- ipni_names$taxon_name
  ipni_names$match_name_with_authors <- paste(ipni_names$taxon_name,
    ipni_names$author)
  ipni_names$database <- "ipni"
  # keep only necessary columns
  ipni_names<- ipni_names[,c("taxon_name","taxon_name_match","author",
    "match_id","database","match_name_with_authors","family")]
# write file
write.csv(ipni_names,"taxize_ipni_names.csv")

# remove duplicates !! VERSION NUMBER THIS IS ARBITRARY ??
  # sort by version and remove duplicates
  #ipni_names_noDup <- setorder(ipni_names,-version,na.last=T)
  #ipni_names_noDup <- distinct(ipni_names_noDup,taxon_name,.keep_all=T)
  #colnames(ipni_names_noDup)
# write file
#write.csv(ipni_names_noDup,"taxize_ipni_names_noDup.csv")

# join with taxa list and remove non-matches
ipni_all <- ipni_names %>% filter(ipni_names$taxon_name %in%
  taxa_list_acc$taxon_name)
# write file
write.csv(ipni_all,"taxize_ipni.csv")

########################
# 3. Create master list
########################

# create dataframe of all synonyms found
datasets <- list(tp_all,itis_all,tpl_all,ipni_all)
all_names <- Reduce(rbind.fill,datasets)
  names(all_names)
# add a space after every period, to standardize authors more
all_names$match_name_with_authors <- gsub(".",". ",
  all_names$match_name_with_authors,fixed=T)
all_names$match_name_with_authors <- str_squish(
  all_names$match_name_with_authors)
all_names$match_name_with_authors <- gsub(". )",".)",
  all_names$match_name_with_authors,fixed=T)
all_names$match_name_with_authors <- gsub("(pro sp.)","",
  all_names$match_name_with_authors,fixed=T)
# keep unique values and create
#   "ref" col of all databases with duplicates and
#   "status" col of all acceptance statuses of duplicates
unique_names <- all_names %>% group_by(taxon_name,taxon_name_match,
  match_name_with_authors) %>%
  summarize(ref = paste(database,collapse = ','),
  status = paste(acceptance,collapse = ',')) %>%
  ungroup()
str(unique_names)
# order rows
unique_names <- setorder(unique_names,"taxon_name")
unique_names <- setorder(unique_names,"taxon_name_match")
# join with initial taxa list
colnames(taxa_list_acc)[colnames(taxa_list_acc)=="taxon_name"] <-
  "taxon_name_match"
all_data <- full_join(unique_names,taxa_list_acc)
# write CSV file of all names
write.csv(all_data,"taxize_all_names_raw2.csv")

# separate out taxon_name_match
all_data2 <- all_data %>% separate("taxon_name_match",
  c("genus","species","infra_rank","infra_name"),sep=" ",extra="warn",
  remove=F,fill="right")
  nrow(all_data2)
# remove forms
all_data3 <- all_data2[which(is.na(all_data2$infra_rank) |
  all_data2$infra_rank != "f."),]
  nrow(all_data3)
# remove records where same match name goes with more than one taxon_name
all_data3$dup <- c(duplicated(all_data3$taxon_name_match,fromLast=T)
  | duplicated(all_data3$taxon_name_match))
all_data4 <- setdiff(all_data3,all_data3[which(
  (all_data3$status == "synonym" | all_data3$status == "synonym,synonym") &
  all_data3$dup == T),])
  nrow(all_data4)
# fix status column
all_data4$status <- gsub("ipni,ipni","ipni",all_data4$status)
# write tile
write.csv(all_data4,"taxize_all_names.csv")

####################################
# 4. Create master target taxa list
####################################

# go through output BY HAND and create a CSV file with three col:
#   1) "taxon_name_acc" (accepted name)
#   2) "taxon_name" (synonym name or accepted name,for accepted taxa)
#   3) "orig_list" (part of accepted list, synonym, other category)
# save the file as "target_taxa_inclu_syn.csv"



















################################
# Find children for target taxa
################################

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

##############################
# Create master children list
##############################

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



#################################################
# Get taxonomic information from various sources
#################################################


########
### Taxonomic Name Resolution Service (TNRS)
########

  # takes a while if lots of names
tnrs_output <- tnrs(taxa_names)
  head(tnrs_output)
  class(tnrs_output) # data.frame
  names(tnrs_output)
  # COLUMNS: submittedname|acceptedname|sourceid|score|
  #          matchedname|authority|uri
tnrs_output2 <- tnrs_output
#tnrs_output2 <- tnrs_output[which(tnrs_output$score > 0.5),]# &
                                 #tnrs_output$submittedname ==
                                 #tnrs_output$matchedname),]
setnames(tnrs_output2,
  old = c("submittedname","matchedname","sourceid","authority","uri","score"),
  new = c("taxon_full_name","TNRS_taxon_full_name_match","TNRS_source",
    "TNRS_author","TNRS_id","TNRS_score"),
  skip_absent=T)
  tnrs_output2 <- tnrs_output2[(-2)]
write.csv(tnrs_output2,"tnrs_output.csv")

########
### Global Names Resolver (GNR)
########

  # takes a while if lots of names, but not as long as ITIS or TNRS
  # gives data from a WIDE variety of sources, so many repeats of each name
  #gnr_datasources() # look at data sources used
gnr_output <- gnr_resolve(taxa_names)
  # IF and error is thrown, use the following code to narrow down the list
  #   and find it and remove it
  #chunked <- split(taxa_names,chunk(taxa_names,chunk.size=10))
  #output <- data.frame()
  #for(i in 1:length(chunked)){
  #  output_new <- gnr_resolve(chunked[[i]])
  #  output <- rbind(output,output_new)
  #  print(chunked[[i]])
  #}
  #chunked[[i]]
  #taxa_names2 <- taxa_names[-(341)] # "Quercus x schuettei" threw the error
  # so we remove
  #gnr_output <- gnr_resolve(taxa_names2)
gnr_output <- as.data.frame(gnr_output)
  head(gnr_output)
  class(gnr_output) # data.frame
  names(gnr_output)
  # COLNAMES: user_supplied_name|submitted_name|matched_name|data_source_title
  #           |score
  gnr_output2 <- gnr_output
setnames(gnr_output2,
  old = c("user_supplied_name","matched_name","data_source_title"),
  new = c("taxon_full_name","taxon_full_name_match","source"),
  skip_absent=T)
  gnr_output2 <- gnr_output2[(-2)]
write.csv(gnr_output2,"gnr_output.csv")

### Other (combinations of databases)
#resolve(taxa_names) # iPlant, TNRS, GNR
  # COLUMNS: user_supplied_name|submitted_name|matched_name|data_source_title

### IUCN Red List
# !!!!! get API
#iucn_summary(test_names[1],distr_detail=T,key="")

### Tropicos distribution info
#tp_id <- unique(tp_output$nameid)
#tp_dist_output <- data.frame()
#for(i in 1:length(tp_id)){
#  output_new <- as.data.frame(tp_dist(tp_id[i]))
#  #output_new$nameid <- tp_id[i]
#  tp_dist_output <- rbind.fill(tp_dist_output,output_new)
#}
#  class(tp_dist_output)
#  names(tp_dist_output)
  # COLUMNS: location: locationid|regionlocationid|regionname|countrylocationid|
  #                    countryname|upperlocationid|uppername
  #          reference: referenceid|articletitle|collation|abbreviatedtitle|
  #                     titlepageyear|fullcitation
#tp_refs(27805271)
  # COLUMNS: referenceid|publicationid|articletitle|collation|fullcitation

### Catalague of Life (COL)
# !!!!! CURRENT VERSION IS NOT WORKING

#################################################
# taxonomic information from various sources
#################################################

names(tnrs_output2)
# TNRS: taxon_full_name,score,taxon_full_name_match,author,id,
#       source(iPlant_TNRS,NCBI)
## no duplicates
names(itis_output3)
# ITIS: taxon_full_name,author,acceptance,taxon_full_name_match,id,source(ITIS)
## duplicates removed except those matching accepted children
names(gnr_output2)
# GNR: taxon_full_name,taxon_full_name_match,source(LOTS),score
## lots of sources; matched name often has author at end; duplicates
names(tp_output3)
# Tropicos: taxon_full_name,id,taxon_full_name_match,acceptance,author,
#           source(citation)
## duplicates removed except those matching additional legitimate names
names(tpl_output4)
# TPL: taxon_full_name,id,author,acceptance,score,
#      source(RJP,TRO,WCSP)
# duplicates removed
names(ipni_output4)
# IPNI: taxon_full_name,id,authors,source(IPNI)
# duplicates removed

# read in saved datasets
tnrs <- read.csv("tnrs_output.csv",header = T,colClasses="character")
itis <- read.csv("itis_output_noDup.csv",header = T,colClasses="character")
tp <- read.csv("tp_output_noDup.csv",header = T,colClasses="character")
tpl <- read.csv("tpl_output_matched.csv",header = T,colClasses="character")
ipni <- read.csv("ipni_output_matched.csv",header = T,colClasses="character")

# join data
data <- list(tnrs,itis,tp,tpl,ipni)
joined <- reduce(data,full_join,by="taxon_full_name")




#### DID NOT FIND THIS HELPFUL ####

#################################################################
# Create file of matching names from Global Names Resolver (GNR)
#################################################################

# break taxa list into chunks to run through taxonomic service
chunked <- split(taxa_names,chunk(taxa_names,chunk.size=50))
  length(chunked)
# run all names through GNR; may take a few minutes if you have lots of names
gnr_output <- data.frame()
for(i in 1:length(chunked)){
  gnr_output_new <- gnr_resolve(names=chunked[[i]],with_context=T)
  gnr_output <- rbind(gnr_output,gnr_output_new)
  print(i)
}
# IF A NAME THROWS AN ERROR, remove it and run code chunk above again
#  chunked[[i]] <- chunked[[i]][-38]

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
