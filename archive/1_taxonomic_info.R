### Author: Emily Beckman  ###  Date: 10/17/19

### DESCRIPTION:
  # This script takes a list of taxa and uses the taxize package to pull
  # taxonomic information from multiple databases; information pulled includes:
    # - Global Names Resolver (GNR) matches
    # - Synonyms from Tropicos, Integrated Taxonomic Information Service (ITIS),
      # and Encyclopedia of Life (EOL)
    # - Children (var./subsp.) from ITIS and COL

### INPUT:
    # "target_taxa.csv" (list of target taxa)
      # two columns:
        # 1. "taxa_full_name" (genus, species, infra rank, and infra name, all
          # separated by one space each)
        # 2. "orig_list" (can say where name came from, if you are using more
          # than one source list)

### OUTPUTS:
    ## "gnr_output_unique.csv"
      # "taxize_tropicos_names.csv"
      # "taxize_itis_names.csv"
      # "taxize_itis_children.csv"
      # "taxize_col_names.csv"
      # "taxize_col_children.csv"
      # "taxize_all_names_raw.csv"
    ## "taxize_synonyms.csv"
    ## "taxize_children.csv"

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

library(rgbif)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(taxize)
library(anchors)
library(stringr)
library(batchtools)
#library(bit64)
#library(httpuv)
#library(googledrive)
#library(taxizedb)


#################
### FUNCTIONS ###
#################

# matches up column headers, keeping all columns, not just matching ones
  # (fills added columns with NAs)
# SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining
  # -dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    return(rbind(x, y))
}

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
  syn_output_df <- Reduce(rbind.all.columns, syn_output[found])
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
  child_output_df <- Reduce(rbind.all.columns, child_output[found])
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
# create list of target SPECIES names only
species_names <- taxa_names[!grepl(" var. ",taxa_names) &
  !grepl(" subsp.",taxa_names) &
  !grepl(" f. ",taxa_names)]
  #& !grepl(" x ",taxa_names,fixed=T)
  #& !grepl(" _ ",taxa_names)
unique(species_names)

####################################################################
# 2. Create file of matching names from Global Names Resolver (GNR)
####################################################################

# check out the datasources included in the GNR and when they were last updated
gnr_datasources()

# input target taxa list to GNR
  # replace hybrid symbol so it can be read correctly
taxa_names <- gsub("_","x",taxa_names,fixed=T)
  # for some reason "Quercus x schuettei" throws an error...
taxa_names <- taxa_names[-(130)]
  # run all names through GNR
  # this may take a few minutes if you have lots of names
chunked <- split(taxa_names,chunk(taxa_names,chunk.size=100))
gnr_output <- data.frame()
for(i in 1:length(chunked)){
  gnr_output_new <- gnr_resolve(names=chunked[[i]],with_context=T)
  gnr_output <- rbind(gnr_output,gnr_output_new)
}
  # keep unique values and create "ref" column with all databases with
  # duplicate names
unique_gnr <- gnr_output %>% group_by(user_supplied_name,matched_name) %>%
  summarize(ref = paste(data_source_title, collapse = ', ')) %>% ungroup()
    #%>% distinct(taxon_name_acc,syn_name,.keep_all=T)
nrow(gnr_output); nrow(unique_gnr)
  # standardize hybrid character
unique_gnr$matched_name <- gsub("×","_",unique_gnr$matched_name,fixed=T)

write.csv(unique_gnr,"gnr_output_unique.csv")

# write list of databases considered in GNR
#gnr <- gnr_datasources()
#write.csv(gnr,"gnr_datasources.csv")

#write.csv(gnr_output_best,"gnr_output_best.csv")
#tnrs(query=taxa_names[1:20],source="iPlant_TNRS")

################################################
# 3. Find synonyms and children for target taxa
################################################

### A) Tropicos (from Missouri Botanical Garden)

# set API key if needed and restart R
  #taxize::use_tropicos() # get API
  #usethis::edit_r_environ()
    # TROPICOS_KEY='41eebfee-a544-435c-991e-66d869db6edb'

# Tropicos only searches for species (not sub-specific ranks) so we will
  # only search for species

# replace hybrid character to match Tropicos system
species_names <- gsub("_","×",species_names,fixed=T); unique(species_names)

# get synonyms
names_tp <- synonyms(species_names, db="tropicos")
# !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  #  ══  Results  ═════════════════
  # Total: 206
  # Found: 191
  # Not Found: 15
# remove speices/taxa that did not have any synonyms,
  # create data frame of synonyms,
  # and add column stating which database it came from
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

### B) Integrated Taxonomic Information Service (ITIS)

# replace specific characters to match ITIS system
taxa_names <- gsub("_","X",taxa_names,fixed=T)
taxa_names <- gsub("subsp.","ssp.",taxa_names)

# get synonyms
names_itis <- synonyms(taxa_names, db="tropicos")
# !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  #  ══  Results  ═════════════════
  # Total:
  # Found:
  # Not Found:
# remove speices/taxa that did not have any synonyms,
  # create data frame of synonyms,
  # and add column stating which database it came from
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

# get children names (var. and subsp.)
children_itis <- children(species_names, db="itis")
# !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  #  ══  Results  ═════════════════
  # Total: 465
  # Found: 198
  # Not Found: 267
# remove speices/taxa that did not have any children,
  # create data frame of children,
  # and add column stating which database it came from
children_itis_df <- children.compiled(children_itis,"itis",4)
colnames(children_itis_df)
# write file
write.csv(children_itis_df,"taxize_itis_children.csv")

### C) Catalogue of Life

# replace specific characters to match COL system
taxa_names <- gsub("X","x",taxa_names,fixed=T)
taxa_names <- gsub("ssp.","subsp.",taxa_names,fixed=T)
taxa_names <- gsub("fo.","f.",taxa_names,fixed=T)

# get synonyms
names_col <- synonyms(taxa_names, db="col")
# !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  #  ══  Results  ═════════════════
  # Total: 465
  # Found: 202
  # Not Found: 263
# remove speices/taxa that did not have any synonyms,
  # create data frame of synonyms,
  # and add column stating which database it came from
names_col_df <- synonyms.compiled(names_col,"col")
# standardize column names for joining later
for(i in 1:nrow(names_col_df)){
  if(names_col_df$rank[i]=="infraspecies"){
    names_col_df$syn_name[i] <- paste(names_col_df$genus[i],
                                      names_col_df$species[i],
                                      names_col_df$infraspecies_marker[i],
                                      names_col_df$infraspecies[i])
  } else {
    names_col_df$syn_name[i] <- names_col_df$name[i]
  }
}
colnames(names_col_df)[colnames(names_col_df)=="id"] <- "syn_id"
colnames(names_col_df)[colnames(names_col_df)=="author"] <- "syn_author"
# keep only necessary columns
names_col_df <- names_col_df[,c("taxon_name_acc","database","syn_name",
  "syn_author","syn_id")]; colnames(names_col_df)
# write file
write.csv(names_col_df,"taxize_col_names.csv")

# get children names (var. and subsp.)
children_col <- children(species_names, db="col")
### !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  #  ══  Results  ═════════════════
  # Total: 206
  # Found: 192
  # Not Found: 14
# remove speices/taxa that did not have any children,
  # create data frame of children,
  # and add column stating which database it came from
children_col_df <- children.compiled(children_col,"col",4)
colnames(children_col_df)
# write file
write.csv(children_col_df,"taxize_col_children.csv")

################################
# 4. Create master synonym list
################################

# create dataframe of all synonyms found
  # list of data frames
datasets <- list(names_itis_df,names_tp_df,names_col_df)
  # go through list of data frames and stack each
all_names <- Reduce(rbind.all.columns,datasets); colnames(all_names)
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
#all_names$syn_name_with_authors[is.na(all_names$syn_name_with_authors)] <-
  #paste(all_names[is.na(all_names$syn_name_with_authors),]$syn_name,
  #all_names[is.na(all_names$syn_name_with_authors),]$syn_author)
  # keep unique values and create "ref" column of all databases with duplicates
unique_names <- all_names %>% group_by(taxon_name_acc,syn_name) %>%
  summarize(ref = paste(database, collapse = ',')) %>% ungroup()
  #%>% distinct(taxon_name_acc,syn_name,.keep_all=T)
nrow(all_names); nrow(unique_names)

# join syn list to main taxa list
taxa_list_acc$taxon_name_acc <- gsub("_","x",taxa_list_acc$taxon_name_acc)
unique_names_join <- join(unique_names,taxa_list_acc,type="full")

# keep only necessary columns
unique_names_join2 <- unique_names_join[,c("taxon_name_acc","syn_name","ref",
  "orig_list")]; colnames(unique_names_join2)

# reorder then write file
unique_names_join2 <- setorder(unique_names_join2,"taxon_name_acc","syn_name")
write.csv(unique_names_join2,"taxize_synonyms.csv")

#################################
# 5. Create master children list
#################################

# create dataframe of all children found
 # list of data frames
datasets2 <- list(children_itis_df,children_col_df)
 # go through list of data frames and stack each
all_children <- Reduce(rbind.all.columns,datasets2); colnames(all_names)
  # order rows by taxa name
all_children <- setorder(all_children,"taxon_name_acc")
  # keep unique values and create "ref" column of all databases with duplicates
unique_children <- all_children %>% group_by(taxon_name_acc,childtaxa_name) %>%
  summarize(ref = paste(database, collapse = ',')) %>% ungroup()
  #%>% distinct(taxon_name_acc,syn_name,.keep_all=T)
nrow(all_children); nrow(unique_children)

# write CSV file of all names
write.csv(unique_children,"taxize_children.csv")
