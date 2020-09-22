### Author: Emily Beckman  ###  Date: 06/28/19

### DESCRIPTION:
  # This script takes a list of species and joins it to all related names (synonyms) in GBIF, and matches to species codes used by FIA.
  # Also, GBIF occurrence point data for all target species is downloaded and can then be used in "2_compile_occurrence_data.R" script.
  # It is an important first step to gather synonyms for each target species before you download data from other occurrence data
  # platforms, to make sure you don't miss important points; GBIF provides an easy way to get most of the synonyms other platforms will have.

### INPUTS:
    # 1. list of target species (IMLS_species_desiderata.csv)
      # two columns: sp_full_name (genus, species, infra rank, and infra name) & orig_list ("Y")
    # 2. fia tree list (FIA_speciesList_2017.csv)
      # one target column: FIA.Code

### OUTPUT:
    # 1. target species list matched to GBIF keys and FIA codes (IMLS_sp_list_joined.csv)
      # GBIF-sourced columns include:
        # speciesKey,taxonKey,scientificName,acceptedScientificName,acceptedTaxonKey,taxonomicStatus,order,family
      # Some other helpful columns are added in this script:
        # genus,specificEpithet,infraspecificEpithet1,infraspecificEpithet2,our_accepted_name (species level)

### NOTES:
  # TAXIZE DATABASE INPUTS (double hash = synonyms function):   # ABBREVIATION:
    # Encyclopedia of Life (EOL)                                 # eol
    # Taxonomic Name Resolution Service                          # tnrs
    ## Integrated Taxonomic Information Service (ITIS)            # itis
    # Global Names Resolver (from EOL/GBIF)                      # gnr
    # Global Names Index (from EOL/GBIF)                         # gni        #http://gni.globalnames.org/about
    # IUCN Red List                                              # iucn       #
    ## Tropicos (from Missouri Botanical Garden)                  # tp / tropicos
    # Theplantlist.org                                           # tpl
    ## Catalogue of Life                                          # col
    # National Center for Biotechnology Information              # ncbi
    # CANADENSYS Vascan name search API                          # vascan
    # International Plant Names Index (IPNI)                     # ipni
    # Barcode of Life Data Systems (BOLD)                        # bold
    ## National Biodiversity Network (UK)                         # nbn
    # Index Fungorum                                             # fg
    # EU BON                                                     # eubon
    # Index of Names (ION)                                       # ion
    # Open Tree of Life (TOL)                                    # tol
    # NatureServe                                                # natserv


#################
### LIBRARIES ###
#################

library(googledrive)
#library(httpuv)
library(rgbif)
library(data.table)
#library(bit64)
library(tidyr)
library(plyr)
library(dplyr)
library(taxize)
library(anchors)
#library(taxizedb)

#################
### FUNCTIONS ###
#################

# Matches up column headers, keeping all columns, not just matching ones [stacking]
# (fills added columns with NAs)
    # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    return(rbind(x, y))
}


##############
### SCRIPT ###
##############

setwd("./Desktop/GA2")

#######################
# 1. Load species list
#######################

# read in species list
  #drive_download("species_list",type="csv",overwrite=T) # download from Google Drive
sp_list_acc <- read.csv("species_list_new.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
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
        names_itis[[i]]$species_name_acc <- rep(names(names_itis[i]), nrow(names_itis[[i]]))
      }
    }
  }; found <- found[-1]
  # create data frame of synonyms
  names_itis_df <- Reduce(rbind.all.columns, names_itis[found])
  # add dataset column to record these came from itis
  names_itis_df$database <- "itis"
  # concatenate some columns to standardize, rename other cols, then keep only necessary cols
  #names_itis_df$acc_name_with_authors <- paste(names_itis_df$acc_name,names_itis_df$acc_author)
  names_itis_df$syn_name_with_authors <- paste(names_itis_df$syn_name,names_itis_df$syn_author)
  colnames(names_itis_df)[colnames(names_itis_df)=="acc_tsn"] <- "acc_id"
  colnames(names_itis_df)[colnames(names_itis_df)=="syn_tsn"] <- "syn_id"
  names_itis_df <- names_itis_df[,c(2:8)]

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
        names_tp[[i]]$species_name_acc <- rep(names(names_tp[i]), nrow(names_tp[[i]]))
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
  # Carya illinoinensis is spelled incorrectly in col, so no match found; change spelling to find match
  sp_names[5] <- "Carya illinoiensis"
  # get synonyms
  names_col <- synonyms(sp_names, db="col")
  ### !! STOP BEFORE RUNNING NEXT LINE -- YOU MAY HAVE TO ANSWER SOME PROMPTS
  # add a species_name column based on each df name in list and create list (found)
    # of indices of species name matches which did not come back NA/NULL
  found <- NA
  for(i in 1:length(names_col)){
    if(length(names_col[[i]])>1){
      found <- c(found,i)
      names_col[[i]]$species_name_acc <- rep(names(names_col[i]), nrow(names_col[[i]]))
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
all_names <- setorder(all_names,"species_name_acc") # order rows by species name
# write CSV file of all names
write.csv(all_names,"taxize_all_names.csv")


### start here if already compiled taxize_all_names.csv
    all_names <- read.csv("taxize_all_names.csv", header = T, na.strings=c("","NA"), colClasses="character")
    sp_list_acc <- read.csv("species_list_new.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
    sp_names <- sp_list_acc[,1] # create list of target species names

# Create dataframe of unique names only
# standardize the infrarank abbreviations
all_names$syn_name <- gsub("ssp.","subsp.",all_names$syn_name,fixed=T)
all_names$syn_name <- gsub("fo.","f.",all_names$syn_name,fixed=T)
all_names$syn_name <- gsub("X ","",all_names$syn_name,fixed=T)
# fix some things up because of the misspelling of illinoinensis in COL database
all_names <- replace.value(all_names,"species_name_acc","Carya illinoiensis","Carya illinoinensis")
unique(all_names$species_name_acc)
      #all_names[nrow(all_names)+1,c(5,3,6)] <- c("Carya illinoinensis","Carya illinoiensis","col")
# keep unique values and make "ref" column of all databases with duplicate names
unique_names <- all_names %>%
                group_by(species_name_acc,syn_name) %>%
                summarize(ref = paste(database, collapse = ',')) %>%
                ungroup() %>%
                distinct(species_name_acc,syn_name,.keep_all=T)
unique_names <- subset(unique_names, species_name_acc != syn_name) # remove rows where syn name is the same as the species name (because just author is different)
# create columns for genus, species, infrarank, and infraname
unique_names <- unique_names %>% separate("syn_name", c("genus","species","infra_rank","infra_name"),sep=" ", fill="right", extra="warn", remove="false")
sp_list_acc <- sp_list_acc %>% separate("species_name_acc", c("genus","species","infra_rank","infra_name"),sep=" ", fill="right", extra="warn", remove="false")

# join syn list to main species list
unique_names_join <- rbind.all.columns(sp_list_acc,unique_names)

# create genus_species column
unique_names_join$genus_species <- paste(unique_names_join$genus,unique_names_join$species)

# add column stating if genus_species duplicate
unique_names_join <- setorder(unique_names_join,"genus_species") # order rows by genus_species
dup <- duplicated(unique_names_join$genus_species); unique_names_join <- cbind(unique_names_join,dup)
  tail(unique_names_join)
dup2 <- duplicated(unique_names_join$genus_species,fromLast=T); unique_names_join <- cbind(unique_names_join,dup2)
  tail(unique_names_join)
# if name is infra_rank and genus_species is not a duplicate, replace genus_species with syn_name
for(i in 1:nrow(unique_names_join)){
  if(unique_names_join$dup[i] == "FALSE" & unique_names_join$dup2[i] == "FALSE" & !is.na(unique_names_join$infra_rank[i])){
    unique_names_join$genus_species[i] <- unique_names_join$syn_name[i]
  }
}

# make sure matching genus_species names correspond with the same species_name_acc
unique_names_join$equal <- NA
unique_names_join2 <- unique_names_join %>% group_by(genus_species) %>% mutate(equal = replace(equal, n_distinct(species_name_acc)>1 & any(species_name_acc != genus_species), syn_name))

remove <- vector()
for(i in 1:nrow(unique_names_join2)){
  if(unique_names_join2$species_name_acc[i] == unique_names_join2$genus_species[i]){
    unique_names_join2$equal[i] <- NA
  }
  if(unique_names_join2$genus_species[i] %in% sp_names & is.na(unique_names_join2$infra_rank[i]) & is.na(unique_names_join2$orig_list[i])){
    remove <- c(remove,i)
  }
  if(!is.na(unique_names_join2$infra_rank[i]) & !is.na(unique_names_join2$equal[i])){
    unique_names_join2$genus_species[i] <- unique_names_join2$equal[i]
    unique_names_join2$equal[i] <- NA
  }
}
unique_names_join2 <- unique_names_join2[-remove,]

# add column stating if genus_species duplicate
unique_names_join3 <- setorder(unique_names_join2,"genus_species") # order rows by genus_species
dup3 <- duplicated(unique_names_join3$equal); unique_names_join3$dup <- dup3
  tail(unique_names_join3)
dup4 <- duplicated(unique_names_join3$equal,fromLast=T); unique_names_join3$dup2 <- dup4
  tail(unique_names_join3)
# if genus_species is not a duplicate, replace genus_species with syn_name
for(i in 1:nrow(unique_names_join3)){
  if(unique_names_join3$dup[i] == "FALSE" & unique_names_join3$dup2[i] == "FALSE"){
    unique_names_join3$equal[i] <- NA
  }
}

# write CSV file of unique names
write.csv(unique_names_join3,"species_list_joined_8_22.csv")

##### DO MANUALLY: #####
# Sort by "equal" column
# Look at groups of names with "equal" != "NA" and matching "genus_species"
# If "ref" columns are equal, remove all rows in group
# If "ref" columns are not equal, keep the one row with highest number of unique "ref"
# Remove "dup", "dup2", and "equal" columns
# Highlight duplicate values in genus_species column
# If any highlighted values are infraspecific, check them out and likely remove

########################
# 2. Download GBIF data
########################

# read in species list
sp_list_acc <- read.csv("species_list_new.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
sp_names <- sp_list_acc[,1] # create list of target species names

# create list of GBIF species keys for all species in target list
keys1 <- sapply(sp_names, function(x) name_backbone(name=x)$speciesKey, simplify = "array")
  keys1 # stop here and check to see if any species came back "NULL"; if they do, check GBIF and copy speciesKey (at end of URL) to command below
    keys <- paste("taxonKey = ",keys1[1],sep="") # ,",7777998"
        # Q. sadleriana is 2880444 ; issue with GBIF backbone so not found automatically
        # Malus ×heterophylla Spach	is 3001444
        # Malus heterophylla Sumn. is 7777998
  for(i in 2:length(keys1)){
    keys <- paste(keys,keys1[i],sep=",")
  }; keys

setwd("./gbif_read_in")

# download data (Darwin Core Archive format) from GBIF by species keys
gbif_download <- occ_download(keys,
                     "basisOfRecord = PRESERVED_SPECIMEN,HUMAN_OBSERVATION,FOSSIL_SPECIMEN,OBSERVATION,UNKNOWN,MACHINE_OBSERVATION,MATERIAL_SAMPLE,LITERATURE",
                     user="ebeckman",pwd="Quercus51",email="ebeckman@mortonarb.org")
gbif_download # IMLS Download key: 0013119-190621201848488
              # GA2 Download key: 0013195-190621201848488

# WAIT FOR THIS DOWNLOAD TO COMPLETE BEFORE RUNNING NEXT LINE
# you can check download status here: https://www.gbif.org/user/download
# it may take a while (up to 3 hours) if you have a large species list

# !!! PASTE "Download key" FROM ABOVE as first argument
occ_download_get(key="0013195-190621201848488", overwrite=TRUE) # download GBIF data to current working directory; Download file size: 351.37 MB ; 69.41 MB
unzip("0013195-190621201848488.zip") # unzip file

# read in data just downloaded
gbif_raw <- fread("occurrence.txt",quote=""); sort(unique(gbif_raw$taxonKey))

# write data to be used in later script
setwd("./..")
write.csv(gbif_raw, "gbif_raw.csv")

# this used to work to import GBIF data directly to R but I updated packages & R version, and something is off now:
  # gbif_raw <- occ_download_get("0003346-190621201848488", overwrite = TRUE) %>% occ_download_import()

#########################################
# 3. Add GBIF taxon codes to species list
#########################################

# create sp_full_name column based on scientificName column
# identify records with more than just genus and species (i.e., subsp., var., or f.)
infra <-  dplyr::filter(gbif_raw, grepl((" subsp. "),scientificName,fixed=T) |
                                  grepl((" var. "),scientificName,fixed=T) |
                                  grepl((" f. "),scientificName,fixed=T)); nrow(infra) # locate records for infra taxa
# separate scientificName into first four elements to cut off the author at end
  name <- infra %>% separate("scientificName",c("one","two","three","four"),sep=" ",fill="right",extra="warn")
  infra$syn_name <- paste(name$one,name$two,name$three,name$four,sep=" ") # join names back together
the_rest <- anti_join(gbif_raw,infra,by="taxonKey"); nrow(the_rest) # separate and join first two parts of scientificName for non-infra taxa
  name <- the_rest %>% separate("scientificName", c("one","two"),sep=" ", fill="right", extra="warn")
  the_rest$syn_name <- paste(name$one,name$two,sep=" ")
gbif_raw <- rbind(infra,the_rest) # bind infra and non-infra taxa back together
  #sort(unique(gbif_raw$sp_full_name))

# keep only the pertinent columns
gbif_join <- gbif_raw %>% distinct(syn_name,speciesKey,taxonomicStatus,acceptedScientificName,family)
# order to place accepted species names first
gbif_join <- gbif_join[order(factor(gbif_join$taxonomicStatus,levels=c("ACCEPTED","SYNONYM","DOUBTFUL"))),]

# join gbif codes/species names to species list
sp_list_acc <- read.csv("species_list_joined_8_22_edited.csv", header = T, na.strings=c("","NA"), colClasses="character")
sp_list_join <- join(sp_list_acc,gbif_join,by="syn_name",type="left",match="first"); nrow(sp_list_join)
  sp_list_join[which(is.na(sp_list_join$speciesKey)),] # these are species which have no spatial points in GBIF or are synonyms in GBIF

##########################
# 4. Add FIA species codes
##########################

# join to fia tree list
#drive_download("FIA_speciesList_2017.csv",overwrite=T) # download from Google Drive
fia_list <- read.csv("FIA_speciesList_2017.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
  fia_codes <- fia_list[,c(1,8)] # keep only pertinent columns
sp_list_join2 <- join(sp_list_join,fia_codes,by="syn_name",type="left",match="first")

# write final file
write.csv(sp_list_join2, "species_list_joined_8_23.csv")























child_itis <- itis_hierarchy(unique(names_itis_df$acc_tsn), what = "down")
test <- children(x=sp_names,db="itis")
,"col","ncbi,"))
child_col <- col_children(name = sp_names)

# create list of GBIF species keys for all species in target list
keys1 <- sapply(sp_names, function(x) name_backbone(name=x)$speciesKey, simplify = "array")
  keys1 # stop here and check to see if any species came back "NULL"; if they do, check GBIF and copy speciesKey (at end of URL) to command below
    keys <- paste("taxonKey = ",keys1[1],",2880444",",3001444",",7777998",sep="")
        # Q. sadleriana is 2880444 ; issue with GBIF backbone so not found automatically
        # Malus ×heterophylla Spach	is 3001444
        # Malus heterophylla Sumn. is 7777998
  for(i in 2:length(keys1)){
    keys <- paste(keys,keys1[i],sep=",")
  }; keys

########################
# 2. Download GBIF data
########################

setwd("./gbif_read_in")

# download data (Darwin Core Archive format) from GBIF by species keys
gbif_download <- occ_download(keys,
                     "basisOfRecord = PRESERVED_SPECIMEN,HUMAN_OBSERVATION,FOSSIL_SPECIMEN,OBSERVATION,UNKNOWN,MACHINE_OBSERVATION,MATERIAL_SAMPLE,LITERATURE",
                     user="ebeckman",pwd="Quercus51",email="ebeckman@mortonarb.org")
gbif_download # IMLS Download key: 0013119-190621201848488
              # GA2 Download key: 0013195-190621201848488

# WAIT FOR THIS DOWNLOAD TO COMPLETE BEFORE RUNNING NEXT LINE
# you can check download status here: https://www.gbif.org/user/download
# it may take a while (up to 3 hours) if you have a large species list

# !!! PASTE "Download key" FROM ABOVE as first argument
occ_download_get(key="0013195-190621201848488", overwrite=TRUE) # download GBIF data to current working directory; Download file size: 351.37 MB ; 69.41 MB
unzip("0013195-190621201848488.zip") # unzip file

# read in data just downloaded
gbif_raw <- fread("occurrence.txt",quote=""); sort(unique(gbif_raw$taxonKey))

# write data to be used in later script
setwd("./..")
write.csv(gbif_raw, "gbif_raw.csv")

# this used to work to import GBIF data directly to R but I updated packages & R version, and something is off now:
  # gbif_raw <- occ_download_get("0003346-190621201848488", overwrite = TRUE) %>% occ_download_import()

#########################################
# 3. Add GBIF taxon codes to species list
#########################################

# create sp_full_name column based on scientificName column
# identify records with more than just genus and species (i.e., subsp., var., or f.)
infra <-  dplyr::filter(gbif_raw, grepl((" subsp. "),scientificName,fixed=T) |
                                  grepl((" var. "),scientificName,fixed=T) |
                                  grepl((" f. "),scientificName,fixed=T)); nrow(infra) # locate records for infra taxa
# separate scientificName into first four elements to cut off the author at end
  name <- infra %>% separate("scientificName",c("one","two","three","four"),sep=" ",fill="right",extra="warn")
  infra$sp_full_name <- paste(name$one,name$two,name$three,name$four,sep=" ") # join names back together
the_rest <- anti_join(gbif_raw,infra,by="taxonKey"); nrow(the_rest) # separate and join first two parts of scientificName for non-infra taxa
  name <- the_rest %>% separate("scientificName", c("one","two"),sep=" ", fill="right", extra="warn")
  the_rest$sp_full_name <- paste(name$one,name$two,sep=" ")
gbif_raw <- rbind(infra,the_rest) # bind infra and non-infra taxa back together
  #sort(unique(gbif_raw$sp_full_name))

# keep only the pertinent columns
gbif_join <- gbif_raw %>% distinct(sp_full_name,speciesKey,taxonKey,scientificName,acceptedScientificName,
                                   acceptedTaxonKey,taxonomicStatus,order,family)

# join gbif codes/species names to species list
sp_list_join <- join(sp_list_acc,gbif_join,by=c("sp_full_name","taxonomicStatus"),type="full"); nrow(sp_list_join)
  sp_list_join[which(is.na(sp_list_join$speciesKey)),] # these are species which have no spatial points in GBIF or are synonyms in GBIF

##########################
# 4. Add FIA species codes
##########################

# join to fia tree list
drive_download("FIA_speciesList_2017.csv",overwrite=T) # download from Google Drive
fia_list <- read.csv("FIA_speciesList_2017.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
  fia_codes <- fia_list[,c(1,8)] # keep only pertinent columns
sp_list_join2 <- join(sp_list_join,fia_codes,by="sp_full_name",type="left",match="first")

##################################
# 5. Final touches to species list
##################################

# create columns for each part of species names
sp_list_join3 <- sp_list_join2 %>% separate(sp_full_name, c("genus","specificEpithet","infraspecificEpithet1",
                                                            "infraspecificEpithet2")," ",remove=F)

# fill our_accepted_name column based on speciesKey
sp_list_join3$our_accepted_name <- NA
  orig <- sp_list_join3[which(sp_list_join3$orig_list == "Y"),]; nrow(orig)
    orig$our_accepted_name <- orig$sp_full_name
  new <- sp_list_join3[which(is.na(sp_list_join3$orig_list)),]; nrow(new)
    for(i in 1:nrow(new)){
      for(j in 1:nrow(orig)){
        match <- grepl(pattern = new$speciesKey[i], x = orig$speciesKey[j], fixed = T)
          if(match == TRUE){
            new$our_accepted_name[i] <- orig$sp_full_name[j]
          }
        }
      }
  sp_list_join4 <- rbind(orig,new)
  sp_list_join4 <- sp_list_join4[-which(is.na(sp_list_join4$our_accepted_name)),] # remove rows with no our_accepted_name
    nrow(sp_list_join4)

# order to place accepted species names first
sp_list_join4 <- sp_list_join4[order(factor(sp_list_join4$taxonomicStatus,levels=c("ACCEPTED","SYNONYM","DOUBTFUL"))),]
# remove dublicates by scientificName
sp_list_join5 <- sp_list_join4 %>% distinct(sp_full_name, .keep_all=T); nrow(sp_list_join5)

# see species with no GBIF occurrence points
sp_list_join5[which(is.na(sp_list_join5$speciesKey)),]

# write final file
write.csv(sp_list_join5, "species_list_joined.csv")

















# an alternate way to do first part of step 3; shows the "×" symbol in front of hybrids, while the first method does not
# create sp_full_name column of accepted gbif names by pasting together genus, species, and infra rank columns
subsp <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "SUBSPECIES")
  subsp$sp_full_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",subsp$infraspecificEpithet,sep=" ")
var <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "VARIETY")
  var$sp_full_name <- paste(var$genus,var$specificEpithet,"var.",var$infraspecificEpithet,sep=" ")
form <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "FORM")
  form$sp_full_name <- paste(form$genus,form$specificEpithet,"f.",form$infraspecificEpithet,sep=" ")
the_rest <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "SPECIES")
  the_rest$sp_full_name <- paste(the_rest$genus,the_rest$specificEpithet,sep=" ")
gbif_raw <- rbind(subsp,var,form,the_rest); sort(unique(gbif_raw$sp_full_name))
